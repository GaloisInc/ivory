--
-- BitData.hs --- Top-level module for ivory-bitdata.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BitData (
  -- * quasiquoter
  bitdata

  -- * bit types
  , Bits(), Bit, BitArray(), BitRep()
  , repToBits, bitsToRep, zeroBits
  , bitLength, bitIx

  -- * bit data
  , BitData(), BitDataField(), BitDataRep

  -- * bit data conversions
  , toBits, fromBits
  , toRep, fromRep

  -- * bit data field operations
  , setBitDataBit, clearBitDataBit, getBitDataField, setBitDataField

  -- * bit data operators
  , (#!) -- access nth element of BitArray
  , (#.) -- flip getBitDataField
  , (#>) -- BitDataField composition (like Control.Category.>>>)

  -- * bit actions
  , BitDataM(), runBits, withBits, withBitsRef
  , clear, setBit, clearBit, setField
) where

import Ivory.BitData.Bits
import Ivory.BitData.BitData
import Ivory.BitData.Array
import Ivory.BitData.Quote
import Ivory.BitData.Monad
