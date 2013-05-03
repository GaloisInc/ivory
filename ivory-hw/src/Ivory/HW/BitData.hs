{-# LANGUAGE FlexibleContexts #-}
--
-- BitData.hs --- Linking registers to bitdata.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.BitData where

import Ivory.BitData
import Ivory.Language

import Ivory.HW.Prim
import Ivory.HW.Reg

-- | A register associated with a bit data type.
newtype BitDataReg d = BitDataReg (Reg (BitDataRep d))

-- | Create a bit data register given its address.
mkBitDataReg :: IvoryIOReg (BitDataRep d) => Integer -> BitDataReg d
mkBitDataReg = BitDataReg . mkReg

getReg :: (BitData d, IvoryIOReg (BitDataRep d))
       => BitDataReg d -> Ivory eff d
getReg (BitDataReg r) = do
  val <- readReg r
  return $ fromRep val

-- | Set a register to a value taken from a block of bit
-- modifications.  The previous value is discarded.
setReg :: (BitData d, IvoryIOReg (BitDataRep d))
       => BitDataReg d -> BitDataM d a -> Ivory eff a
setReg (BitDataReg r) mf = do
  let (result, val) = runBits 0 mf
  writeReg r val
  return result

-- | Modify a register by a set of bit modification actions.
modifyReg :: (BitData d, IvoryIOReg (BitDataRep d))
          => BitDataReg d -> BitDataM d a -> Ivory eff a
modifyReg (BitDataReg r) mf = do
  val <- readReg r
  let (result, val') = runBits val mf
  writeReg r val'
  return result
