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
  , Bits(), Bit, BitRep()
  , repToBits, bitsToRep, zeroBits

  -- * bit data
  , BitData(), BitDataField(), BitDataRep

  -- * bit data conversions
  , toBits, fromBits
  , toRep, fromRep

  -- * bit data field operations
  , setBitDataBit, clearBitDataBit, getBitDataField, setBitDataField

  -- * bit actions
  , BitDataM(), runBits, withBits, withBitsRef
  , clear, setBit, clearBit, setField
) where

import Ivory.BitData.Bits
import Ivory.BitData.BitData
import Ivory.BitData.Quote
import Ivory.BitData.Monad
