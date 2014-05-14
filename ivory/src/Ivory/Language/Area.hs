{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Language.Area where

import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

#if __GLASGOW_HASKELL__ >= 700
import GHC.TypeLits (Nat,Symbol,KnownNat)
#else
import GHC.TypeLits (Nat,Symbol,SingI,Sing,sing)
#endif

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

#if __GLASGOW_HASKELL__ >= 700
instance (KnownNat len, IvoryArea area)
#else
instance (SingI len, IvoryArea area)
#endif
  => IvoryArea (Array len area) where
  ivoryArea _ = I.TyArr len area
    where
#if __GLASGOW_HASKELL__ >= 700
    len  = fromInteger (fromTypeNat (Proxy :: Proxy len))
#else
    len  = fromInteger (fromTypeNat (sing :: Sing len))
#endif
    area = ivoryArea (Proxy :: Proxy area)

instance IvoryType a => IvoryArea (Stored a) where
  ivoryArea _ = ivoryType (Proxy :: Proxy a)

