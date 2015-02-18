{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--
-- Prim.hs --- I/O register primitives.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.Prim where

import Ivory.Language

hw_moduledef :: ModuleDef
hw_moduledef = do
  incl ioRegReadU8
  incl ioRegWriteU8
  incl ioRegReadU16
  incl ioRegWriteU16
  incl ioRegReadU32
  incl ioRegWriteU32

class IvoryBits a => IvoryIOReg a where
  ioRegSize  :: a -> Integer
  ioRegRead  :: Def ('[Uint32] :-> a)
  ioRegWrite :: Def ('[Uint32, a] :-> ())

ioRegReadU8 :: Def ('[Uint32] :-> Uint8)
ioRegReadU8 = importProc "ivory_hw_io_read_u8" "ivory_hw_prim.h"

ioRegWriteU8 :: Def ('[Uint32, Uint8] :-> ())
ioRegWriteU8 = importProc "ivory_hw_io_write_u8" "ivory_hw_prim.h"

instance IvoryIOReg Uint8 where
  ioRegSize _  = 1
  ioRegRead  = ioRegReadU8
  ioRegWrite = ioRegWriteU8

ioRegReadU16 :: Def ('[Uint32] :-> Uint16)
ioRegReadU16 = importProc "ivory_hw_io_read_u16" "ivory_hw_prim.h"

ioRegWriteU16 :: Def ('[Uint32, Uint16] :-> ())
ioRegWriteU16 = importProc "ivory_hw_io_write_u16" "ivory_hw_prim.h"

instance IvoryIOReg Uint16 where
  ioRegSize _  = 2
  ioRegRead  = ioRegReadU16
  ioRegWrite = ioRegWriteU16

ioRegReadU32 :: Def ('[Uint32] :-> Uint32)
ioRegReadU32 = importProc "ivory_hw_io_read_u32" "ivory_hw_prim.h"

ioRegWriteU32 :: Def ('[Uint32, Uint32] :-> ())
ioRegWriteU32 = importProc "ivory_hw_io_write_u32" "ivory_hw_prim.h"

instance IvoryIOReg Uint32 where
  ioRegSize _  = 4
  ioRegRead  = ioRegReadU32
  ioRegWrite = ioRegWriteU32
