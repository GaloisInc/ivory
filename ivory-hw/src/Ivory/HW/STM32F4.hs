--
-- STM32F4.hs --- Machine definition for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.STM32F4 where

import Ivory.HW.IOArea

-- STM32F4 I/O memory areas.
ioAPB1, ioAPB2 :: IOArea
ioAPB1 = IOArea 0x40000000 0x40008000
ioAPB2 = IOArea 0x40010000 0x40015800

ioAHB1, ioAHB2, ioAHB3 :: IOArea
ioAHB1 = IOArea 0x40020000 0x40080000
ioAHB2 = IOArea 0x50000000 0x50060C00
ioAHB3 = IOArea 0x60000000 0xA0001000

ioCM4 :: IOArea
ioCM4 = IOArea 0xE0000000 0xE0100000

-- | All I/O memory areas for this machine.
ioAreas :: [IOArea]
ioAreas = [ioAPB1, ioAPB2, ioAHB1, ioAHB2, ioAHB3, ioCM4]
