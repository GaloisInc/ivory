{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ivory.Language.Module where

import           Prelude                ()
import           Prelude.Compat

import           Ivory.Language.Area    (IvoryArea)
import           Ivory.Language.MemArea (ConstMemArea (..), MemArea (..))
import           Ivory.Language.Proc    (Def (..))
import           Ivory.Language.Proxy   (ASymbol, Proxy (..))
import           Ivory.Language.String  (IvoryString (..))
import           Ivory.Language.Struct  (IvoryStruct (..), StructDef (..),
                                         StructName)
import qualified Ivory.Language.Syntax  as I
import           Ivory.Language.Type    (IvoryExpr, unwrapExpr)

import           Control.Monad          (forM_)
import           MonadLib               (Id, ReaderM, ReaderT, WriterM, WriterT,
                                         ask, local, put, runM)
import           MonadLib.Derive        (Iso (..), derive_ask, derive_put)

-- Modules ---------------------------------------------------------------------

data Visible = Public | Private deriving (Show)

newtype ModuleM a = Module
  { unModule :: ReaderT Visible (WriterT I.Module Id) a
  } deriving (Functor,Monad,Applicative)

instance ReaderM ModuleM Visible where
  ask = derive_ask (Iso Module unModule)
  {-# INLINE ask #-}

instance WriterM ModuleM I.Module where
  put = derive_put (Iso Module unModule)
  {-# INLINE put #-}

type ModuleDef = ModuleM ()

instance Monoid (ModuleM ()) where
  mempty  = return ()
  mappend = (>>)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

-- | Add an element to the public/private list, depending on visibility
visAcc :: Visible -> a -> I.Visible a
visAcc vis e = case vis of
                       Public  -> I.Visible { I.public  = [e], I.private = [] }
                       Private -> I.Visible { I.public = [], I.private = [e] }

-- | Include a defintion in the module.
incl :: Def a -> ModuleDef
incl (DefProc p)    = do
  visibility <- ask
  put (mempty { I.modProcs   = visAcc visibility p })
incl (DefImport i)
  | null (I.importFile i) = error $ "Empty header name for " ++ show i
  | otherwise = put (mempty { I.modImports = [i] })

-- | Import an externally-defined symbol.
inclSym :: IvoryExpr t => t -> ModuleDef
inclSym t = case unwrapExpr t of
  I.ExpExtern sym
    | null (I.externFile sym) -> error $ "Empty header name for " ++ show sym
    | otherwise -> put (mempty { I.modExterns = [sym] })
  e -> error $ "Cannot import expression " ++ show e

-- | Add a dependency on another module.
depend :: I.Module -> ModuleDef
depend m = dependByName (I.modName m)

-- | Add a dependency on another module by name. Use the same name
-- here that you use when you call 'package' to build the target
-- module. This function is particularly useful when building mutually
-- dependent module structures.
dependByName :: String -> ModuleDef
dependByName nm =
  put (mempty { I.modDepends = [nm] })

-- | Include the definition of a structure in the module.
defStruct :: forall sym. (IvoryStruct sym, ASymbol sym) =>
  Proxy sym -> ModuleDef
defStruct _ = case getStructDef def of
  I.Abstract n "" -> error $ "Empty header name for struct " ++ n
  str -> do
    visibility <- ask
    put (mempty { I.modStructs = visAcc visibility str })
  where
  def :: StructDef sym
  def  = structDef

-- | Include the definition of a string type's structure.
defStringType :: forall str. (IvoryString str) => Proxy str -> ModuleDef
defStringType _ = defStruct (Proxy :: Proxy (StructName str))

-- | Include the definition of a memory area.
defMemArea :: IvoryArea area => MemArea area -> ModuleDef
defMemArea m = case m of
  MemImport ia
    | null (I.aiFile ia) -> error $ "Empty header name for " ++ show ia
    | otherwise -> put (mempty { I.modAreaImports = [ia] })
  MemArea a as -> do
    visibility <- ask
    put (mempty { I.modAreas = visAcc visibility a })
    forM_ as $ \aux -> do
      put (mempty { I.modAreas = visAcc Private aux })

-- | Include the definition of a constant memory area.
defConstMemArea :: IvoryArea area => ConstMemArea area -> ModuleDef
defConstMemArea (ConstMemArea m) = defMemArea m

-- | Package the module up. Default visibility is public.
package :: String -> ModuleDef -> I.Module
package name build = (snd (runM (unModule build) Public)) { I.modName = name }

-- | Start a block of definitions that should not be put in the header.
private :: ModuleDef -> ModuleDef
private build = Module $ local Private (unModule build)

-- | Start a block of definitions should be put in the header. This is the
-- default, and this function is just included to complement 'private'.
public :: ModuleDef -> ModuleDef
public build = Module $ local Public (unModule build)

-- Accessors -------------------------------------------------------------------

moduleName :: I.Module -> String
moduleName  = I.modName
