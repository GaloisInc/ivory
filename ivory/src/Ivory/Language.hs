{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- XXX testing
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ivory.Language (
    -- * Kinds
    Area(..)
  , Proc(..)

    -- * Types
  , IvoryType(), IvoryArea()
  , IvoryVar()
  , IvoryExpr()

  , OpaqueType()

    -- ** Non-null References
  , IvoryRef()
  , ConstRef()
  , IvoryStore()
  , Ref(), refToPtr, constRef, deref, store, refCopy

    -- ** Stack Allocation
  , IvoryInit(..), Init()
  , IvoryZeroVal(izeroval)
  , IvoryZero(izero)
  , iarray
  , InitStruct(), (.=), istruct
  , local

    -- ** SizeOf
  , IvorySizeOf(..), sizeOf

    -- ** Nullable Pointers
  , Ptr(), nullPtr

    -- ** Booleans
  , IBool(), true, false

    -- ** Characters
  , IChar(), char

    -- ** Strings
  , IString()

    -- ** Signed Integers
  , Sint8()
  , Sint16()
  , Sint32()
  , Sint64()

    -- ** Unsigned Integers
  , Uint8()
  , Uint16()
  , Uint32()
  , Uint64()

    -- ** Floating-point Numbers
  , IFloat()
  , IDouble()
  , isnan, isinf, roundF, ceilF, floorF
  , ifloat, idouble

    -- * Effects
  , Effects(..)
  , BreakEff(..),  GetBreaks(), AllowBreak(), ClearBreak(), noBreak
  , ReturnEff(..), GetReturn(), ClearReturn(), noReturn
  , AllocEff(..),  GetAlloc(),  ClearAlloc(),  noAlloc
  , AllocEffects, ProcEffects, NoEffects

    -- * Language

    -- ** Monadic Interface
  , Ivory()
  , RefScope(..)

    -- ** Subexpression naming
  , assign

    -- ** Constants
  , extern

    -- ** Arithmetic (operators from the 'Num' class are also provided).
  , IvoryIntegral((.%), iDiv)

    -- ** Comparisons
  , IvoryEq((==?),(/=?))
  , IvoryOrd((>?),(>=?),(<?),(<=?))

    -- ** Boolean operators
  , iNot, (.&&), (.||)

    -- ** Bit operators
  , IvoryBits((.&),(.|),(.^),iComplement,iShiftL,iShiftR), extractByte
  , BitSplit(lbits, ubits), BitCast(bitCast)

    -- ** Bit data
  , Bits(), Bit(), BitArray()

    -- ** External memory areas
  , MemArea(), area, importArea
  , ConstMemArea(), constArea, importConstArea
  , IvoryAddrOf(addrOf)

    -- ** Procedures
  , Def()
  , ProcPtr(), procPtr
  , proc, externProc, importProc
  , Body(), body

    -- *** Pre/Post-Conditions
  , requires
  , checkStored
  , ensures

    -- ** Assumption/Assertion statements
  , assert
  , assume

    -- ** Structures
  , IvoryStruct(..), StructDef(), (~>), Label()
  , ASymbol

    -- ** Arrays
  , (!)
  , fromIx, toIx, Ix(), ixSize
  , arrayLen
  , toCArray
  , ANat

    -- ** Strings
  , IvoryString(..)

    -- ** Looping
  , for, times
  , breakOut
  , arrayMap
  , forever

    -- ** Call
  , call, indirect
  , call_, indirect_

    -- ** Conditional Branching
  , ifte_, (?), withRef

    -- ** Return
  , ret, retVoid

    -- ** Type-safe casting.
  , SafeCast(), RuntimeCast(), Default()
  , safeCast, castWith, castDefault
  , SignCast(), signCast

    -- ** Module Definitions
  , AST.Module(), moduleName, package
  , ModuleDef, incl, depend, defStruct
  , defStringType
  , defMemArea, defConstMemArea
  , inclHeader
  , private, public
  , sourceDep

    -- * Quasiquoters
  , ivory
  , ivoryFile

    -- * Utilities
  , Proxy(..), comment

  ) where

import Ivory.Language.Area
import Ivory.Language.Array
import Ivory.Language.Assert
import Ivory.Language.Bits
import Ivory.Language.CArray
import Ivory.Language.Cast
import Ivory.Language.Comment
import Ivory.Language.Cond
import Ivory.Language.Const
import Ivory.Language.Effects
import Ivory.Language.Float
import Ivory.Language.IBool
import Ivory.Language.IChar
import Ivory.Language.IIntegral
import Ivory.Language.IString
import Ivory.Language.Init
import Ivory.Language.Loop
import Ivory.Language.MemArea
import Ivory.Language.Module
import Ivory.Language.Monad
import Ivory.Language.Proc
import Ivory.Language.Proxy
import Ivory.Language.Ptr
import Ivory.Language.Ref
import Ivory.Language.Scope
import Ivory.Language.Sint
import Ivory.Language.SizeOf
import Ivory.Language.String
import Ivory.Language.Struct
import Ivory.Language.Type
import Ivory.Language.Uint
import Ivory.Language.Syntax.Concrete.QQ
import Ivory.Language.Syntax.Concrete.QQ.BitData.Bits
import Ivory.Language.Syntax.Concrete.QQ.BitData.Array
import qualified Ivory.Language.Syntax.AST as AST

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
   -- , spi_cr1_br       :: SPIBaud
   , spi_cr1_br       :: Bits 3
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
   } as 8b0 # spi_cr2_txeie # spi_cr2_rxneie # spi_cr2_errie
   # spi_cr2_frf # 1b0 # spi_cr2_ssoe # spi_cr2_txdmaen
   # spi_cr2_rxdmaen

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
