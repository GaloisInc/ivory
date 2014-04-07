{-# LANGUAGE ScopedTypeVariables #-}
--
-- Reg.hs --- I/O register access from Ivory.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.Reg where

import Numeric (showHex)
import Ivory.Language

import Ivory.HW.IOArea
import Ivory.HW.Prim
import Ivory.HW.Machine

-- | An I/O register containing a value of type "t".  Define registers
-- using the "mkReg" functions.
data Reg t = Reg Integer

-- XXX decouple module from STM32F4 : paramaterize mkReg by definition for
-- ioAreas

-- | Smart constructor that ensures a register address is in bounds
-- when created.  This raises an error if the address is invalid.
mkReg :: forall t. IvoryIOReg t => Integer -> Reg t
mkReg addr =
  if (any (addrInBounds addr (ioRegSize (undefined :: t))) ioAreas)
    then Reg addr
    else error $ "I/O register out of bounds at address 0x" ++
                 (showHex addr "")

-- | Read an I/O register, returning an Ivory value.
readReg :: IvoryIOReg a => Reg a -> Ivory eff a
readReg (Reg addr) = call ioRegRead (fromIntegral addr)

-- | Write an I/O register from an Ivory value.
writeReg :: IvoryIOReg a => Reg a -> a -> Ivory eff ()
writeReg (Reg addr) val = call_ ioRegWrite (fromIntegral addr) val
