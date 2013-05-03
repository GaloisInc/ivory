{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
--
-- Bits.hs --- Bit-sized unsigned integer types.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BitData.Bits where

import GHC.TypeLits
import Ivory.Language

import Ivory.BitData.DefBitRep

----------------------------------------------------------------------
-- Bit Representations

-- | Type function: "BitRep (n :: Nat)" returns an Ivory type given a
-- bit size as a type-level natural.  Instances of this type family
-- for bits [1..64] are generated using Template Haskell.
type family BitRep (n :: Nat) :: *

defBitRep ''BitRep ''Uint8  [1..8]
defBitRep ''BitRep ''Uint16 [9..16]
defBitRep ''BitRep ''Uint32 [17..32]
defBitRep ''BitRep ''Uint64 [33..64]

-- | Set of constraints we require on a bit representation type.
type IvoryRep a = (IvoryBits a, IvoryOrd a, IvoryInit a,
                   IvoryStore a, IvoryType a)

----------------------------------------------------------------------
-- Bit Data Type

-- | A wrapper for an Ivory type that can hold an "n" bit unsigned
-- integer.
newtype Bits (n :: Nat) = Bits { unBits :: BitRep n }

-- | "Bit" is a type alias for "Bits 1".
type Bit = Bits 1

-- | Type function to extract the "n" from a "Bits n" type.
type family BitSize a :: Nat
type instance BitSize (Bits n) = n

-- | Convert a Haskell integer to a bit data value without bounds
-- checking.  This must not be exported, but is used by the
-- quasiquoter when the values are constant at compile-time and
-- already bounds checked.
unsafeIntToBits :: (IvoryRep (BitRep n), Integral a) => a -> Bits n
unsafeIntToBits = Bits . fromIntegral

-- | Return a bit value of all zeros of the given size.
zeroBits :: (IvoryRep (BitRep n)) => Bits n
zeroBits = Bits 0

-- XXX do not export, used when unwrapping/rewrapping values when
-- setting fields and the size has obviously not changed.
unsafeRepToBits :: BitRep n -> Bits n
unsafeRepToBits = Bits

-- | Convert an Ivory value to a bit value.  If the input value
-- contains out of range bits, they will be ignored.
repToBits :: forall n. (SingI n, IvoryRep (BitRep n))
          => BitRep n -> Bits n
repToBits x = Bits (x .& mask)
  where mask = fromIntegral (2 ^ fromSing (sing :: Sing n) - 1 :: Integer)

-- | Convert a bit value to an Ivory value.
bitsToRep :: Bits n -> BitRep n
bitsToRep = unBits
