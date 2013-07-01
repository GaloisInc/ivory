{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ivory.Language (
    -- * Kinds
    Area(..)
  , Proc(..)

    -- * Types
  , IvoryType(), IvoryArea()
  , IvoryVar()
  , IvoryExpr()

    -- ** Non-null References
  , IvoryRef()
  , ConstRef()
  , IvoryStore()
  , Ref(), refToPtr, constRef, deref, store, refCopy

    -- ** Stack Allocation
  , IvoryInit(..), Init()
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

    -- * Effects
  , Effects(..)
  , BreakEff(..),  CanBreak(),  AllowBreak(), ClearBreak(), noBreak
  , ReturnEff(..), GetReturn(), ClearReturn(), noReturn
  , AllocEff(..),  GetAlloc(),  ClearAlloc(),  noAlloc

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
  , IvoryStruct(..), StructDef(), ivory, (~>), Label()

    -- ** Arrays
  , (!)
  , fromIx, toIx, Ix(), ixSize
  , arrayLen
  , SingI()
  , toCArray

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
  , defMemArea, defConstMemArea
  , inclHeader
  , private, public
  , sourceDep


    -- * Utilities
  , Proxy(..)
  ) where

import Ivory.Language.Area
import Ivory.Language.Array
import Ivory.Language.Assert
import Ivory.Language.Bits
import Ivory.Language.CArray
import Ivory.Language.Cast
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
import Ivory.Language.Struct
import Ivory.Language.Struct.Quote (ivory)
import Ivory.Language.Type
import Ivory.Language.Uint
import qualified Ivory.Language.Syntax.AST as AST

import GHC.TypeLits (SingI)


-- Language --------------------------------------------------------------------

-- | Unwrap a pointer, and use it as a reference.
withRef :: IvoryArea area
        => Ptr as area
        -> (Ref as area -> Ivory eff t)
        -> Ivory eff f
        -> Ivory eff ()
withRef ptr t = ifte_ (nullPtr /=? ptr) (t (ptrToRef ptr))

-- | Sub-expression naming.
assign :: forall eff a. IvoryExpr a => a -> Ivory eff a
assign e = do
  r <- freshVar "let"
  emit (AST.Assign (ivoryType (Proxy :: Proxy a)) r (unwrapExpr e))
  return (wrapExpr (AST.ExpVar r))

-- | Primitive return from function.
ret :: (GetReturn eff ~ Returns r, IvoryVar r) => r -> Ivory eff ()
ret r = emit (AST.Return (typedExpr r))

retVoid :: (GetReturn eff ~ Returns ()) => Ivory eff ()
retVoid  = emit AST.ReturnVoid
