{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- XXX
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}


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

    -- ** Constant strings
  , IString()
    -- ** Dynamic bounded-length strings
  , IvoryString(..)

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
  , IvoryBits((.&),(.|),(.^),iComplement,iShiftL,iShiftR, iBitSize), extractByte
  , BitSplit(lbits, ubits), BitCast(bitCast)
  , TwosComplementCast(twosComplementCast, twosComplementRep)

    -- ** Bit data

  -- * bit types
  , Bits(), Bit, BitArray(), BitRep()
  , repToBits, bitsToRep, zeroBits
  , bitLength, bitIx

  -- * bit data
  , BitData(), BitDataField(), BitDataRep

  -- * bit data conversions
  , toBits, fromBits
  , toRep, fromRep

  -- * bit data field operations
  , setBitDataBit, clearBitDataBit, getBitDataField, setBitDataField

  -- * bit data operators
  , (#!) -- access nth element of BitArray
  , (#.) -- flip getBitDataField
  , (#>) -- BitDataField composition (like Control.Category.>>>)

  -- * bit actions
  , BitDataM(), runBits, withBits, withBitsRef
  , clear, setBit, clearBit, setField
  , bitToBool, boolToBit

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
  , fromTypeNat

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
import Ivory.Language.BitData.Array
import Ivory.Language.BitData.BitData
import Ivory.Language.BitData.Bits
import Ivory.Language.BitData.Monad
import qualified Ivory.Language.Syntax.AST as AST

-------------------------------------------------------------------
-------------

-- [ivory|
-- -- struct Bar { int32_t aBar; }

-- struct Bar { aBar :: Array 10 (Stored Sint32); }

-- struct Boo { struct Bar aBoo; }
-- |]

-- fooBar :: Def ('[Ref s (Struct "Boo")] :-> Sint32)
-- fooBar = proc "fooBar" $ \ref -> body $ do
--   x <- deref (ref ~> aBoo ~> aBar ! 3)
--   ret x

-- foo x = do
--   a <- local (ival 0)
--   store a x
--   b <- deref a
--   return b

[ivory|

struct Bar { int32_t aBar; }

struct Boo { struct Bar aBoo; }

int32_t bar(int32_t x, * struct Boo a, * int32_t y, * int32_t[10] arr) {
  (a . aBoo) -> aBar = *y;
  (a . aBoo) -> aBar = *y;
  arr[toIx(*y + *y)] = *y + *y;
  return 3 ;
  -- a <- $foo(x);
  -- return a;
}
|]
