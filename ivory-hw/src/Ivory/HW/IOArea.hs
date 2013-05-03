--
-- IOArea.hs --- Memory area type definition.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.IOArea where

-- | A region of the I/O address space.
data IOArea = IOArea
  { ioAreaStart :: Integer
  , ioAreaEnd   :: Integer
  } deriving Show

-- | "addrInBounds addr size area" returns True if the address range
-- "[addr, addr + size)" lies within the bounds of "area".
addrInBounds :: Integer -> Integer -> IOArea -> Bool
addrInBounds addr size (IOArea start end) =
  (size >= 0) && (addr >= start) && (addr + size <= end)
