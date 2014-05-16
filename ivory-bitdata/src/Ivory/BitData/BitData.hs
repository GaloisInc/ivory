{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
--
-- BitData.hs --- Typed bit data types.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BitData.BitData where

import Ivory.Language

import Ivory.BitData.Bits

-- | Class of bit data types defined by the "bitdata" quasiquoter.
class (ANat (BitSize (BitType a)),
       IvoryRep (BitDataRep a),
       BitType a ~ Bits (BitSize (BitType a))) => BitData a where
  -- | Return the base "(Bits n)" type as defined in the "bitdata"
  -- quasiquoter.
  type BitType a :: *

  -- | Convert a bit data type to its raw bit value.  This is always
  -- well defined and should be exported.
  toBits :: a -> BitType a

  -- | Convert a raw bit value to a bit data type.  All values may not
  -- be well defined with respect to the original set of bit data
  -- constructors.  For now, we allow these "junk" values to be
  -- created, but that may change in the future (perhaps by
  -- implementing a checked, Ivory run-time conversion).
  fromBits :: BitType a -> a

-- | The Ivory type that stores the actual value for a bit data type.
--
-- This is a shorthand to simplify the constraints on functions that
-- take arguments of the "BitData" class.
type BitDataRep a = BitRep (BitSize (BitType a))

-- | Convert a raw Ivory type to a bit data type.  If the input value
-- is too large, the out of range upper bits will be masked off.
fromRep :: BitData a => BitDataRep a -> a
fromRep = fromBits . repToBits

-- XXX do not export---used when unwrapping/rewrapping values when
-- setting fields and the size has obviously not changed.
unsafeFromRep :: BitData a => BitDataRep a -> a
unsafeFromRep = fromBits . unsafeRepToBits

-- | Convert a bit data value to its Ivory representation.
toRep :: BitData a => a -> BitDataRep a
toRep = unBits . toBits

-- | Identity instance of "BitData" for the base "Bits n" type.
instance (ANat n, IvoryRep (BitRep n)) => BitData (Bits n) where
  type BitType (Bits n) = Bits n
  toBits = id
  fromBits = id

-- | Description of a bit field defined by the "bitdata" quasiquoter.
-- Each field defined in the record syntax will generate a top-level
-- definition of "BitDataField".
--
-- This constructor must remain unexported so that only fields checked
-- by the quasiquoter are created.
data BitDataField a b = BitDataField
  { bitDataFieldPos  :: Int
  , bitDataFieldLen  :: Int
  , bitDataFieldName :: String
  } deriving Show

-- | Bit data field composition.  (like Control.Category.>>>)
(#>) :: BitDataField a b -> BitDataField b c -> BitDataField a c
(BitDataField p1 _ n1) #> (BitDataField p2 l2 n2) = BitDataField pos len name
  where
    name = n1 ++ "." ++ n2
    pos = p1 + p2
    len = l2

-- | Extract a field from a bit data definition.  Returns the value as
-- the type defined on the right hand side of the field definition in
-- the "bitdata" quasiquoter.
getBitDataField :: (BitData a, BitData b,
                    BitCast (BitDataRep a) (BitDataRep b))
                => BitDataField a b -> a -> b
getBitDataField f x = unsafeFromRep (bitCast ((toRep x `iShiftR` pos) .& mask))
  where pos  = fromIntegral (bitDataFieldPos f)
        mask = fromIntegral ((2 ^ bitDataFieldLen f) - 1 :: Integer)

-- | Infix operator to read a bit data field.  (like Data.Lens.^.)
(#.) :: (BitData a, BitData b,
         BitCast (BitDataRep a) (BitDataRep b))
     => a -> BitDataField a b -> b
(#.) = flip getBitDataField

-- | Set a field from a bit data definition.
setBitDataField :: (BitData a, BitData b,
                    SafeCast (BitDataRep b) (BitDataRep a))
                => BitDataField a b -> a -> b -> a
setBitDataField f x y = unsafeFromRep ((toRep x .& mask) .| val)
  where val  = safeCast (toRep y) `iShiftL` pos
        pos  = fromIntegral (bitDataFieldPos f)
        fmax = fromIntegral ((2 ^ bitDataFieldLen f) - 1 :: Integer)
        mask = iComplement (fmax `iShiftL` pos)

-- | Set a single-bit field in a bit data value.
setBitDataBit :: BitData a => BitDataField a Bit -> a -> a
setBitDataBit f x = unsafeFromRep (toRep x .| (1 `iShiftL` pos))
  where pos = fromIntegral (bitDataFieldPos f)

-- | Clear a single-bit field in a bit data value.
clearBitDataBit :: BitData a => BitDataField a Bit -> a -> a
clearBitDataBit f x = unsafeFromRep (toRep x .& (iComplement (1 `iShiftL` pos)))
  where pos = fromIntegral (bitDataFieldPos f)
