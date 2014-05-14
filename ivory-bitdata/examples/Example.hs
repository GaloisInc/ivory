{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Example where

import Control.Monad

import Ivory.Language
-- import Ivory.Compile.C
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

 -- The "NVIC_ISER" register is an array of 32 bits.
 --
 -- We will want to access the array both at Ivory run-time using an
 -- "Ix 32" and at code generation time using a Haskell integer.
 bitdata NVIC_ISER :: Bits 32 = nvic_iser
   { nvic_iser_setena :: BitArray 32 Bit
   }

 -- A bit data type with an array of 4-bit integers.
 bitdata ArrayTest :: Bits 32 = array_test
   { at_4bits :: BitArray 8 (Bits 4)
   }
|]

test1 :: Def ('[Uint16] :-> Uint16)
test1 = proc "test1" $ \x -> body $ do
  ret $ withBits x $ do
        clearBit spi_cr1_cpha
        setBit   spi_cr1_cpol
        setField spi_cr1_br spi_baud_div_8

test2 :: Def ('[Uint32] :-> Uint8)
test2 = proc "test2" $ \x -> body $ do
  let d = fromRep x :: NVIC_ISER
  ret $ toRep (d #. nvic_iser_setena #! 0)

-- | Iterate over the elements of a bit array.
forBitArray_ arr f =
  forM_ [0..bitLength arr] $ \i ->
    f (arr #! i)

-- | Test looping over the elements of a bit array:
test3 :: Def ('[Uint32] :-> Uint32)
test3 = proc "test3" $ \x -> body $ do
  let d = fromRep x
  total <- local (ival 0)
  forBitArray_ (d #. at_4bits) $ \i -> do
    x' <- deref total
    let y = safeCast (toRep i)
    store total (x' + y)
  ret =<< deref total

get_baud :: Def ('[Uint16] :-> Uint8)
get_baud = proc "get_baud" $ \x -> body $ do
  let d = fromRep x
  ret (toRep (d #. spi_cr1_br))

cmodule :: Module
cmodule = package "hw" $ do
  incl get_baud
  incl test1
  incl test2
  incl test3

main :: IO ()
main = void $ runCompiler [cmodule] (initialOpts {stdOut = True, constFold = True})

