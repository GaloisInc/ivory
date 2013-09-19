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

import Data.Monoid (Monoid(..),mconcat)
import GHC.TypeLits

-- | Stack allocation
local :: forall eff s area. (IvoryArea area, E.GetAlloc eff ~ E.Scope s)
      => Init area
      -> Ivory eff (Ref (Stack s) area)
local ini = do
  lname <- freshVar "local"
  let ty = ivoryArea (Proxy :: Proxy area)
  emit (I.Local ty lname (getInit ini))

  rname <- freshVar "ref"
  let areaTy = ivoryArea (Proxy :: Proxy area)
  emit (I.AllocRef areaTy rname (I.NameVar lname))

  return (wrapExpr (I.ExpVar rname))


-- | Initializer values.
newtype Init (area :: Area *) = Init { getInit :: I.Init }

-- | Zero initializers.
class IvoryZero (area :: Area *) where
  izero :: Init area


-- Stored Initializers ---------------------------------------------------------

-- | Initializers for 'Stored' things.
class IvoryVar e => IvoryInit e where
  ival :: e -> Init (Stored e)
  ival e = Init (I.InitExpr ty (unwrapExpr e))
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

instance (IvoryZero area, SingI len) => IvoryZero (Array len area) where
  izero = Init I.InitZero

iarray :: forall len area. SingI len => [Init area] -> Init (Array len area)
iarray is = Init (I.InitArray (take len (map getInit is)))
            -- ^ truncate to known length
  where
  len = fromInteger (fromTypeNat (sing :: Sing len))

idynarray :: forall a s len ref.
             ( SingI len, IvoryRef ref
             , IvoryArea a
             , IvoryVar (ref s (Array len a)))
          => ref s (Array len a)
          -> Init (DynArray a)
idynarray ref = Init (I.InitDynArray ty (unwrapExpr ref))
  where
    ty = ivoryArea (Proxy :: Proxy (Array len a))

-- Struct Initializers ---------------------------------------------------------

instance IvoryStruct sym => IvoryZero (Struct sym) where
  izero = Init I.InitZero

newtype InitStruct (sym :: Symbol) = InitStruct
  { getInitStruct :: [(String, I.Init)]
  }

-- Much like the C initializers, the furthest right field initializer will take
-- precidence, and fields not mentioned will be left as zero.
instance IvoryStruct sym => Monoid (InitStruct sym) where
  mempty      = InitStruct []
  mappend l r = InitStruct (mappend (getInitStruct l) (getInitStruct r))

istruct :: IvoryStruct sym => [InitStruct sym] -> Init (Struct sym)
istruct is = Init (I.InitStruct fields)
  where
  fields = [ (l,i) | (l,i) <- getInitStruct (mconcat is) ]

(.=) :: Label sym area -> Init area -> InitStruct sym
l .= ini = InitStruct [(getLabel l, getInit ini)]
