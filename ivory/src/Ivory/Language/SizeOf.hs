{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Language.SizeOf where

import Ivory.Language.Proxy
import Ivory.Language.Type


class IvoryType t => IvorySizeOf (t :: k) where
  sizeOfBytes :: Proxy t -> Integer

instance IvorySizeOf () where
  sizeOfBytes _ = 0

-- | Get the size of an ivory type.
sizeOf :: (IvorySizeOf t, IvoryExpr a, Num a) => Proxy t -> a
sizeOf  = fromInteger . sizeOfBytes
