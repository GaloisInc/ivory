{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BitDataTypes where

import Ivory.Language

[ivory|
 bitdata SPIBaud :: Bits 3
   = spi_baud_div_2   as 0
   | spi_baud_div_4   as 1
   | spi_baud_div_8   as 2
   | spi_baud_div_16  as 3
   | spi_baud_div_32  as 4
   | spi_baud_div_64  as 5
   | spi_baud_div_128 as 6
   | spi_baud_div_256 as 7
|]

-- | Examples from Ivory paper.
[ivory|
 bitdata BaudRate :: Bits 2
   = baud_9600  as 0b00
   | baud_19200 as 0b01
   | baud_38400 as 0b10
   -- bit pattern 0b11 is invalid
|]
