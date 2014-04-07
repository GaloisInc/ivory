--
-- HW.hs --- I/O register access from Ivory.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW (
  -- * I/O Registers
  Reg(), IvoryIOReg(), mkReg, readReg, writeReg

  -- * Bit Data I/O registers
  , BitDataReg(), mkBitDataReg,  mkBitDataRegNamed
  , getReg, setReg, modifyReg
) where

import Ivory.HW.Reg
import Ivory.HW.BitData
import Ivory.HW.Prim
