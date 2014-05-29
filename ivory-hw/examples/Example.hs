{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Example where

import Ivory.HW
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import ExampleTypes

----------------------------------------------------------------------
-- Driver Definition

[ivory|
 bitdata SPI_CR1 :: Bits 16 = spi_cr1
   { spi_cr1_bidimode :: Bit
   , spi_cr1_bidioe   :: Bit
   , spi_cr1_crcen    :: Bit
   , spi_cr1_crcnext  :: Bit
   , spi_cr1_dff      :: Bit
   , spi_cr1_rxonly   :: Bit
   , spi_cr1_ssm      :: Bit
   , spi_cr1_ssi      :: Bit
   , spi_cr1_lsbfirst :: Bit
   , spi_cr1_spe      :: Bit
   , spi_cr1_br       :: SPIBaud
   , spi_cr1_mstr     :: Bit
   , spi_cr1_cpol     :: Bit
   , spi_cr1_cpha     :: Bit
   }

 bitdata SPI_CR2 :: Bits 16 = spi_cr2
   { _                :: Bits 8
   , spi_cr2_txeie    :: Bit
   , spi_cr2_rxneie   :: Bit
   , spi_cr2_errie    :: Bit
   , spi_cr2_frf      :: Bit
   , _                :: Bit
   , spi_cr2_ssoe     :: Bit
   , spi_cr2_txdmaen  :: Bit
   , spi_cr2_rxdmaen  :: Bit
   }

 bitdata SPI_CR3 :: Bits 4 = foo
   { _                :: Bits 1
   , foo1  :: Bit
   , foo2  :: Bit
   }
|]

fooit :: Reg Uint8
fooit = mkReg 30000000

regSPI1_CR1 :: BitDataReg SPI_CR1
regSPI1_CR1 = mkBitDataReg 0x40013000

regSPI1_CR2 :: BitDataReg SPI_CR2
regSPI1_CR2 = mkBitDataReg 0x40013004

----------------------------------------------------------------------
-- Test Module

test1 :: Def ('[] :-> Uint16)
test1 = proc "test1" $ body $ do
  setReg regSPI1_CR1 $ do
    setBit   spi_cr1_cpha
    clearBit spi_cr1_cpol
    setField spi_cr1_br spi_baud_div_64
  modifyReg regSPI1_CR2 $ do
    clearBit spi_cr2_rxdmaen
    clearBit spi_cr2_txdmaen
  d <- getReg regSPI1_CR1
  ret $ toRep d

cmodule :: Module
cmodule = package "io" $ do
  incl test1

main :: IO ()
main = void $ runCompiler [cmodule] (initialOpts {stdOut = True, constFold = True})
-- main = (mapM_ . mapM_) putStrLn $ showModule $ compileModule cmodule
