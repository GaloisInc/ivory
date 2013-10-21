{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Language.Area where

import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

import GHC.TypeLits (Nat,Symbol,SingI,Sing,sing)

-- Memory Areas ----------------------------------------------------------------

-- | Type proxies for @Area@s.
type AProxy a = Proxy (a :: Area *)

-- | The kind of memory-area types.
data Area k
  = Struct Symbol
  | Array Nat (Area k)
  | CArray (Area k)
  | Stored k
    -- ^ This is lifting for a *-kinded type

-- | Guard the inhabitants of the Area type, as not all *s are Ivory *s.
class IvoryArea (a :: Area *) where
  ivoryArea :: Proxy a -> I.Type

instance (SingI len, IvoryArea area) => IvoryArea (Array len area) where
  ivoryArea _ = I.TyArr len area
    where
    len  = fromInteger (fromTypeNat (sing :: Sing len))
    area = ivoryArea (Proxy :: Proxy area)

instance IvoryType a => IvoryArea (Stored a) where
  ivoryArea _ = ivoryType (Proxy :: Proxy a)

