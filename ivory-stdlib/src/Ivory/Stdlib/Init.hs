{-# LANGUAGE DataKinds #-}

module Ivory.Stdlib.Init where

import Ivory.Language

-- | Variant of 'izero' that constrains the length of the array to match
-- a given type-level natural. This reduces the need for the
-- ScopedTypeVariables extension in Ivory code.
izerolen :: (IvoryArea area, IvoryZero area, ANat bound)
         => Proxy bound
         -> Init (Array bound area)
izerolen _ = izero

-- | Variant of 'iarray' that constrains the length of the array to
-- match a given type-level natural. This reduces the need for the
-- ScopedTypeVariables extension in Ivory code.
iarraylen :: (IvoryArea area, IvoryZero area, ANat bound)
          => Proxy bound
          -> [Init area]
          -> Init (Array bound area)
iarraylen _ = iarray
