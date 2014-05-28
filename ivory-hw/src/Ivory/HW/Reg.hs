{-# LANGUAGE ScopedTypeVariables #-}
--
-- Reg.hs --- I/O register access from Ivory.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.Reg where

import Ivory.Language
import Ivory.HW.Prim

-- | An I/O register containing a value of type "t".  Define registers
-- using the "mkReg" functions.
data Reg t = Reg Integer

-- | Previously, this was a smart constructor to raise an error if the address
--   is invalid, but we didn't find a way to parameterize the valid address
--   space by the platform, so now mkReg accepts all addresses.
mkReg :: forall t. IvoryIOReg t => Integer -> Reg t
mkReg addr = Reg addr

-- | Read an I/O register, returning an Ivory value.
readReg :: IvoryIOReg a => Reg a -> Ivory eff a
readReg (Reg addr) = call ioRegRead (fromIntegral addr)

-- | Write an I/O register from an Ivory value.
writeReg :: IvoryIOReg a => Reg a -> a -> Ivory eff ()
writeReg (Reg addr) val = call_ ioRegWrite (fromIntegral addr) val
