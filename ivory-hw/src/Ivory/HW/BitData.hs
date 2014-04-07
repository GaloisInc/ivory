{-# LANGUAGE FlexibleContexts #-}
--
-- BitData.hs --- Linking registers to bitdata.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.BitData where

import Numeric (showHex)

import Ivory.BitData
import Ivory.Language

import Ivory.HW.Prim
import Ivory.HW.Reg

-- | A register associated with a bit data type.
data BitDataReg d =
  BitDataReg
    { bdr_reg  :: Reg (BitDataRep d)
    , bdr_name :: Maybe String
    }

bdrComment :: BitDataReg d -> String -> Ivory eff ()
bdrComment r c = comment (regname ++ " " ++ c )
  where
  regname = case bdr_name r of
    Just n -> n
    Nothing -> "0x" ++ (showHex regaddr "")
  regaddr = case bdr_reg r of Reg a -> a

-- | Create a bit data register given its address.
mkBitDataReg :: IvoryIOReg (BitDataRep d) => Integer -> BitDataReg d
mkBitDataReg a = BitDataReg { bdr_reg = mkReg a, bdr_name = Nothing }

-- | Create a bit data register given its address and name.
mkBitDataRegNamed :: IvoryIOReg (BitDataRep d) => Integer -> String -> BitDataReg d
mkBitDataRegNamed a n = BitDataReg { bdr_reg = mkReg a, bdr_name = Just n }

getReg :: (BitData d, IvoryIOReg (BitDataRep d))
       => BitDataReg d -> Ivory eff d
getReg r = do
  bdrComment r "get register"
  val <- readReg (bdr_reg r)
  return $ fromRep val

-- | Set a register to a value taken from a block of bit
-- modifications.  The previous value is discarded.
setReg :: (BitData d, IvoryIOReg (BitDataRep d))
       => BitDataReg d -> BitDataM d a -> Ivory eff a
setReg r mf = do
  let (result, val) = runBits 0 mf
  bdrComment r "set register"
  writeReg (bdr_reg r) val
  return result

-- | Modify a register by a set of bit modification actions.
modifyReg :: (BitData d, IvoryIOReg (BitDataRep d))
          => BitDataReg d -> BitDataM d a -> Ivory eff a
modifyReg r mf = do
  val <- readReg (bdr_reg r)
  let (result, val') = runBits val mf
  bdrComment r "modify register"
  writeReg (bdr_reg r) val'
  return result
