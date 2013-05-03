{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import Ivory.Language
import Ivory.Compile.C
import Ivory.Compile.C.CmdlineFrontend

import Ivory.BitData
import ExampleTypes

[bitdata|
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

 -- The "SPI_CR2" register defined using a layout clause.
 bitdata SPI_CR2 :: Bits 16 = spi_cr2
   { spi_cr2_txeie    :: Bit
   , spi_cr2_rxneie   :: Bit
   , spi_cr2_errie    :: Bit
   , spi_cr2_frf      :: Bit
   , spi_cr2_ssoe     :: Bit
   , spi_cr2_txdmaen  :: Bit
   , spi_cr2_rxdmaen  :: Bit
   } as 8b0 # spi_cr2_txeie # spi_cr2_rxneie # spi_cr2_errie # spi_cr2_frf
      # 1b0 # spi_cr2_ssoe # spi_cr2_txdmaen # spi_cr2_rxdmaen

 -- The "SPI_CR2" register defined using the default layout and
 -- padding fields.
 bitdata Alt_SPI_CR2 :: Bits 16 = alt_spi_cr2
   { _                    :: Bits 8
   , alt_spi_cr2_txeie    :: Bit
   , alt_spi_cr2_rxneie   :: Bit
   , alt_spi_cr2_errie    :: Bit
   , alt_spi_cr2_frf      :: Bit
   , _                    :: Bit
   , alt_spi_cr2_ssoe     :: Bit
   , alt_spi_cr2_txdmaen  :: Bit
   , alt_spi_cr2_rxdmaen  :: Bit
   }
|]

test1 :: Def ('[Uint16] :-> Uint16)
test1 = proc "test1" $ \x -> body $ do
  ret $ withBits x $ do
    clearBit spi_cr1_cpha
    setBit   spi_cr1_cpol
    setField spi_cr1_br spi_baud_div_8

get_baud :: Def ('[Uint16] :-> Uint8)
get_baud = proc "get_baud" $ \x -> body $ do
  ret $ toRep $ getBitDataField spi_cr1_br (fromRep x)

cmodule :: Module
cmodule = package "hw" $ do
  incl get_baud
  incl test1

main :: IO ()
main = runCompiler [cmodule] (initialOpts {stdOut = True, constFold = True})
