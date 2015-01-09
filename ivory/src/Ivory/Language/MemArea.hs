{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Language.MemArea where

import Ivory.Language.Area
import Ivory.Language.Init
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Scope
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

import Control.Applicative

import qualified MonadLib        as M
import qualified MonadLib.Derive as M

-- Running Initializers --------------------------------------------------------

-- | This is used to generate fresh names for compound initializers.
newtype AreaInitM a = AreaInitM
  { unAreaInitM :: M.ReaderT String (M.StateT Int M.Id) a }

areaInit_iso :: M.Iso (M.ReaderT String (M.StateT Int M.Id)) AreaInitM
areaInit_iso = M.Iso AreaInitM unAreaInitM

instance Functor AreaInitM where
  fmap = M.derive_fmap areaInit_iso

instance Applicative AreaInitM where
  pure  = M.derive_pure areaInit_iso
  (<*>) = M.derive_apply areaInit_iso

instance Monad AreaInitM where
  return  = M.derive_return areaInit_iso
  (>>=)   = M.derive_bind   areaInit_iso

instance M.ReaderM AreaInitM String where
  ask = M.derive_ask areaInit_iso

instance M.StateM AreaInitM Int where
  get = M.derive_get areaInit_iso
  set = M.derive_set areaInit_iso

instance FreshName AreaInitM where
  freshName s = do
    i <- M.get
    M.set $! i + 1
    name <- M.ask
    return (I.VarLitName ("_iv_" ++ name ++ "_" ++ s ++ show i))

runAreaInitM :: String -> AreaInitM a -> a
runAreaInitM s x = fst (M.runId (M.runStateT 0 (M.runReaderT s(unAreaInitM x))))

areaInit :: String -> Init area -> (I.Init, [Binding])
areaInit s ini = runAreaInitM s (runInit (getInit ini))

-- Memory Areas ----------------------------------------------------------------

-- | Externally defined memory areas.
data MemArea (area :: Area *)
  = MemImport I.AreaImport
  | MemArea I.Area [I.Area]

-- XXX do not export
memSym :: MemArea area -> I.Sym
memSym m = case m of
  MemImport i -> I.aiSym i
  MemArea a _ -> I.areaSym a

-- | Create an area from an auxillary binding.
bindingArea :: Bool -> Binding -> I.Area
bindingArea isConst b = I.Area
  { I.areaSym   = bindingSym b
  , I.areaConst = isConst
  , I.areaType  = bindingType b
  , I.areaInit  = bindingInit b
  }

makeArea :: I.Sym -> Bool -> I.Type -> I.Init -> I.Area
makeArea sym isConst ty ini = I.Area
  { I.areaSym   = sym
  , I.areaConst = isConst
  , I.areaType  = ty
  , I.areaInit  = ini
  }

-- | Define a global constant. Requires an IvoryZero constraint to ensure the
-- area has an initializers, but does not explicilty initialize to 0 so that the
-- value is placed in the .bss section.
area :: forall area. (IvoryArea area, IvoryZero area)
     => I.Sym -> Maybe (Init area) -> MemArea area
area sym (Just ini) = MemArea a1 as
  where
  (ini', binds) = areaInit sym ini
  ty            = ivoryArea (Proxy :: Proxy area)
  a1            = makeArea sym False ty ini'
  as            = map (bindingArea False) binds
area sym Nothing = MemArea a1 []
  where
  ty = ivoryArea (Proxy :: Proxy area)
  a1 = makeArea sym False ty I.zeroInit

-- | A memory area containing a guaranteed non-null pointer.
refArea :: forall area. IvoryArea area
     => I.Sym -> Init area -> MemArea area
refArea sym ini = MemArea a1 as
  where
  (ini', binds) = areaInit sym ini
  ty            = ivoryArea (Proxy :: Proxy area)
  a1            = makeArea sym False ty ini'
  as            = map (bindingArea False) binds

-- | Import an external symbol from a header.
importArea :: IvoryArea area => I.Sym -> String -> MemArea area
importArea name header = MemImport I.AreaImport
  { I.aiSym   = name
  , I.aiConst = False
  , I.aiFile  = header
  }


-- Constant Memory Areas -------------------------------------------------------

newtype ConstMemArea (area :: Area *) = ConstMemArea (MemArea area)

-- | Constant memory area definition.
constArea :: forall area. IvoryArea area
          => I.Sym -> Init area -> ConstMemArea area
constArea sym ini = ConstMemArea $ MemArea a1 as
  where
  (ini', binds) = areaInit sym ini
  ty            = ivoryArea (Proxy :: Proxy area)
  a1            = makeArea sym True ty ini'
  as            = map (bindingArea True) binds

-- | Import an external symbol from a header.
importConstArea :: IvoryArea area => I.Sym -> String -> ConstMemArea area
importConstArea name header = ConstMemArea $ MemImport I.AreaImport
  { I.aiSym   = name
  , I.aiConst = False
  , I.aiFile  = header
  }

-- Area Usage ------------------------------------------------------------------

-- | Turn a memory area into a reference.
class IvoryAddrOf (mem :: Area * -> *) ref | mem -> ref, ref -> mem  where
  addrOf :: IvoryArea area => mem area -> ref Global area

-- XXX do not export
primAddrOf :: IvoryArea area => MemArea area -> I.Expr
primAddrOf mem = I.ExpAddrOfGlobal (memSym mem)

instance IvoryAddrOf MemArea Ref where
  addrOf mem = wrapExpr (primAddrOf mem)

instance IvoryAddrOf ConstMemArea ConstRef where
  addrOf (ConstMemArea mem) = wrapExpr (primAddrOf mem)

