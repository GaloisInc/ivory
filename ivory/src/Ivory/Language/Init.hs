{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Language.Init where

import Ivory.Language.Area
import Ivory.Language.Array
import Ivory.Language.Float
import Ivory.Language.IBool
import Ivory.Language.IChar
import Ivory.Language.Monad
import Ivory.Language.Proc
import Ivory.Language.Proxy
import Ivory.Language.Ptr
import Ivory.Language.Ref
import Ivory.Language.Scope
import Ivory.Language.Struct
import Ivory.Language.Sint
import Ivory.Language.Type
import Ivory.Language.Uint
import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Effects as E

import Control.Monad (forM_)
import Data.Monoid (Monoid(..),mconcat)
import GHC.TypeLits

-- Initializers ----------------------------------------------------------------

-- | Intermediate initializer type supporting compound initializers.
-- The "IFresh" nodes are flattened into multiple "I.Init" nodes
-- in a "FreshName" monad when the variable is allocated.
data XInit
  = IVal      I.Type I.Init
  | IArray    I.Type [XInit]
  | IStruct   I.Type [(String, XInit)]
  | IFresh    I.Type XInit (I.Var -> I.Init)

-- | Return the type of the initializer.
initType :: XInit -> I.Type
initType (IVal    ty _)   = ty
initType (IArray  ty _)   = ty
initType (IStruct ty _)   = ty
initType (IFresh  ty _ _) = ty

newtype Init (area :: Area *) = Init { getInit :: XInit }

-- | Zero initializers.
class IvoryZero (area :: Area *) where
  izero :: Init area

-- Running Initializers --------------------------------------------------------

class Monad m => FreshName m where
  freshName :: String -> m I.Var

instance FreshName (Ivory eff) where
  freshName = freshVar

-- | A variable binding (on the stack or in a memory area).
data Binding = Binding
  { bindingVar    :: I.Var
  , bindingType   :: I.Type
  , bindingInit   :: I.Init
  } deriving Show

-- XXX do not export
bindingSym :: Binding -> I.Sym
bindingSym b =
  case bindingVar b of
    I.VarName s     -> s
    I.VarInternal s -> s
    I.VarLitName s  -> s

-- | Return the initializer and auxillary bindings for an
-- initializer in a context that can allocate fresh names.
runInit :: FreshName m => XInit -> m (I.Init, [Binding])
runInit ini =
  case ini of
    IVal _ i ->
      return (i, [])
    IArray _ is -> do
      binds    <- mapM runInit is
      let inis  = map fst binds
      let aux   = concatMap snd binds
      return (I.InitArray inis, aux)
    IStruct _ is -> do
      binds    <- mapM iniStruct is
      let inis  = map fst binds
      let aux   = concatMap snd binds
      return (I.InitStruct inis, aux)
    IFresh _ i f -> do
      var       <- freshName "init"
      (i', aux) <- runInit i
      let ty     = initType i
      let aux'   = aux ++ [Binding var ty i']
      return (f var, aux')
  where
    iniStruct (s, i) = do
      (i', binds) <- runInit i
      return ((s, i'), binds)

-- Stored Initializers ---------------------------------------------------------

-- | Initializers for 'Stored' things.
class IvoryVar e => IvoryInit e where
  ival :: e -> Init (Stored e)
  ival e = Init (IVal ty (I.InitExpr ty (unwrapExpr e)))
    where
    ty = ivoryType (Proxy :: Proxy e)

instance IvoryInit IBool
instance IvoryInit IChar
instance IvoryInit Uint8
instance IvoryInit Uint16
instance IvoryInit Uint32
instance IvoryInit Uint64
instance IvoryInit Sint8
instance IvoryInit Sint16
instance IvoryInit Sint32
instance IvoryInit Sint64
instance IvoryInit IFloat
instance IvoryInit IDouble
instance ProcType proc => IvoryInit (ProcPtr proc)
instance IvoryArea area => IvoryInit (Ptr Global area)
instance SingI len => IvoryInit (Ix len)

instance IvoryZero (Stored IBool) where
  izero = ival false

instance IvoryZero (Stored IChar) where
  izero = ival (char '\0')

instance IvoryArea area => IvoryZero (Stored (Ptr Global area)) where
  izero = ival nullPtr

-- catch-all case for numeric things
instance (Num a, IvoryInit a) => IvoryZero (Stored a) where
  izero = ival 0

-- Array Initializers ----------------------------------------------------------

instance (IvoryZero area, IvoryArea area, SingI len) =>
    IvoryZero (Array len area) where
  izero = Init (IVal ty I.InitZero)
    where
    ty = ivoryArea (Proxy :: Proxy (Array len area))

iarray :: forall len area. (IvoryArea area, SingI len)
       => [Init area] -> Init (Array len area)
iarray is = Init (IArray ty (take len (map getInit is)))
            -- ^ truncate to known length
  where
  len = fromInteger (fromTypeNat (sing :: Sing len))
  ty = ivoryArea (Proxy :: Proxy (Array len area))

-- Struct Initializers ---------------------------------------------------------

instance IvoryStruct sym => IvoryZero (Struct sym) where
  izero = Init (IVal ty I.InitZero)
    where
    ty = ivoryArea (Proxy :: Proxy (Struct sym))

newtype InitStruct (sym :: Symbol) = InitStruct
  { getInitStruct :: [(String, XInit)]
  }

-- Much like the C initializers, the furthest right field initializer will take
-- precidence, and fields not mentioned will be left as zero.
instance IvoryStruct sym => Monoid (InitStruct sym) where
  mempty      = InitStruct []
  mappend l r = InitStruct (mappend (getInitStruct l) (getInitStruct r))

istruct :: forall sym. IvoryStruct sym => [InitStruct sym] -> Init (Struct sym)
istruct is = Init (IStruct ty fields)
  where
  fields = [ (l,i) | (l,i) <- getInitStruct (mconcat is) ]
  ty = ivoryArea (Proxy :: Proxy (Struct sym))

(.=) :: Label sym area -> Init area -> InitStruct sym
l .= ini = InitStruct [(getLabel l, getInit ini)]

-- | Stack allocation
local :: forall eff s area. (IvoryArea area, E.GetAlloc eff ~ E.Scope s)
      => Init area
      -> Ivory eff (Ref (Stack s) area)
local ini = do
  (i, binds) <- runInit (getInit ini)

  forM_ binds $ \b -> do
    emit (I.Local (bindingType b) (bindingVar b) (bindingInit b))

  lname <- freshVar "local"
  let ty = ivoryArea (Proxy :: Proxy area)
  emit (I.Local ty lname i)

  rname <- freshVar "ref"
  let areaTy = ivoryArea (Proxy :: Proxy area)
  emit (I.AllocRef areaTy rname (I.NameVar lname))

  return (wrapExpr (I.ExpVar rname))
