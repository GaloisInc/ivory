{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Language.SizeOf where

import Ivory.Language.Area
import Ivory.Language.Proxy
import Ivory.Language.Type

class IvoryArea t => IvorySizeOf (t :: Area *) where
  sizeOfBytes :: Proxy t -> Integer

instance (ANat len, IvorySizeOf area) => IvorySizeOf (Array len area) where
  sizeOfBytes _ =
    fromTypeNat (aNat :: NatType len) * sizeOfBytes (Proxy :: Proxy area)

-- | Get the size of an ivory type.
sizeOf :: (IvorySizeOf t, IvoryExpr a, Num a) => Proxy t -> a
sizeOf  = fromInteger . sizeOfBytes

