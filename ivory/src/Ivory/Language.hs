{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- XXX
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

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


-- [ivory|
--   struct Foo {
--     fool :: Stored Uint64
-- }
-- |]

-- mStations :: MemArea (Array 32 (Struct "Foo"))
-- mStations = area "mStations" Nothing

-- mStations2 :: MemArea (Array 32 (Stored Uint64))
-- mStations2 = area "mStations" Nothing

-- --foo = return $ (addrOf mStations ! 0) ~> fool

-- [ivory|
-- uint64_t foo() {
--   return (&mStations2)[0];

-- }

-- |]


e :: IBool
e = (4::Sint32) >? 3

type SomeInt = Uint32

macroStmts x y = do
  a <- local (ival 0)
  store a (x + y)

macroStmtsRet x y = do
  a <- local (ival 0)
  store a (x + y)
  return =<< deref a

macroExp x y = do
  x <? y

[ivory|

-- A top level definition.
bar = 3 + 2;

-- Define a top-level type alias.
type Boo = int8_t;

-- (Both of the above can be done on Haskell and you can use antiquotation.)

-- Define a struct. A base type is turned into a memory area by adding a
-- '&'. Note this is a type-level operator. Arrays and structs are always memory
-- areas.
struct Bar00 {
  &int32_t aBar00;
}

struct Foo
 { -- When context makes apparent and we can syntactically check the type, a
   -- base type is promoted to a memory area (i.e., bool is rewritten to &bool).
   bool aFoo
   -- '&' at the type level binds mostly tightly and is righ-associative. When
   -- we use a type alias, the '&' must be added explicitly.
 ; &SomeInt[4] aBar
 }

-- Safe string structs: strings of max length 16 bytes Generates two field
-- labels: stringLengthL (int32_t) and stringDataL (array uint8_t[16]). The type
-- of the struct is ivory_string_FooStr. See the String library in ivory-stdlib
-- for some string library functions.
string struct FooStr 16

-- Nested struct references
struct Foo2
  { struct Foo foo2f
  }

-- Abstract structs
abstract struct fooStruct "foobar/foo.h"

-- Struct declared with C-like types
struct Goo
 { int32_t aGoo;
   bool bGoo;
   int32_t[4] cGoo;
 }

-- Use the expression and alias
Boo foo(Boo x) {
  return (bar + x);
}

-- Calling builtin functions
void foo67(* float a)
{ store *a as sin(*a);
}

-- if-the-else blocks.  Braces are mandatory.
int32_t foo1() {
  if (true) {
    let a = 5;
    return a;
  }
  else {
    let b = 3;
    return b + 3;
  }
}

-- Empty statements in if-then-else
void foo30(Boo x) {
  if(true) {}
  else { return; }
  return;
}

-- Allocation on the stack (dereference is an expression)
int32_t foo0() {
  alloc *x = 3;
  store *x as 4;
  return *x + 4;
}


-- Global allocation (G), stack allocation (S), and undetermined scope (var c)
uint8_t foo12(* uint8_t a, G*uint8_t b, * uint8_t c, S* uint8_t d) {
  store *b as *a;
  return *b + *c + *d;
}

-- Allocated on either the stack or globally, with user-provided type-variable (xx).
xx* uint32_t foo14(xx* struct Foo f) {
  let a = f . aBar;
  let u = a @ 2;
  return u;
}

-- Allocate an array and index into it. A precondition is given for the function
-- argument.
uint32_t foo6(v *uint32_t[3] arr0) {
  alloc arr1[] = {1,2,3};
  map ix {
    store *arr0 @ ix as *arr1 @ ix;
  }
  -- Same as *arr0 @ 1 ;
  return arr0[1] ;
}
{ pre(arr0[1] > 0);
}

-- Use a struct. Polymorphic region reference.
void foo00(* int32_t[5] arr, * struct Bar00 str) {
  -- The following two are equivalent
  store * arr @ 2 as 3;
  store arr[2]   as 4;
  -- As well as the next two
  store *str.aBar00 as 1;
  store str->aBar00 as 2;

  store arr[2]      as arr[3];
}

-- Boolean operators
uint32_t foo7(bool b, uint32_t a) {
    return (b && !b ? a+3 : signum(abs(a)) - 4);
}

void foo2() {return;}

-- Function calls, with a return value and without.
uint32_t foo8(bool b, uint32_t a) {
    foo2();
    let x = foo7(b, a);
    foo7 (b, a);
    return (b && !b ? a+3 : signum(abs(a)) - x);
}

-- Local assignment (constant).
uint32_t foo9(uint32_t a) {
  forever {
    let b = a + 3;
    return b;
  }
  return 0;
}

-- Memcopy.
void foo10(*uint32_t[3] r0, *uint32_t[3] r1) {
  memcpy r0 r1;
}

-- Const decorators.
uint32_t foo11(const *uint32_t i) {
  return *i;
}

-- Pre and post conditions.
uint32_t foo15(uint32_t a, * struct Foo f, * struct Foo g) {
        return a;
}
{ pre(a < 4);
  pre(a > 0);
  post(return > 5);
  pre(* f . aFoo && * g . aFoo);
}

-- Stack allocated variable.
uint32_t foo16(S*uint32_t i) {
  return *i;
}
{ pre(*i > 3); }

-- Global allocated variable.
uint32_t foo17(G*uint32_t i) {
  return *i;
}

-- allocate and initialize a struct.
void foo92() {
  alloc s = {aFoo = true};
  return;
}

-- Mapping over an array
void mapProc(*uint8_t[4] arr, uint8_t x) {
  map ix {
    let v = arr @ ix;
    store *v as *v + x;
  }
}

-- Casting
void foo57(* uint8_t a, * uint16_t b, * int8_t c, * int32_t d)
{
  store *b as safeCast(*a);     -- can always safely cast
  store *b as castWith(3, *d);  -- cast with a default value if out of range
  store *c as twosCompCast(*a); -- interpret under 2s compliment
}

-- Statements and expression macros
int32_t foo68(int32_t x, int32_t y) {
  -- macroStmts is a Haskell function
  $macroStmts(x, y);
  -- with a return value (Ivory eff a)
  a <- $macroStmtsRet(x, y);
  -- macroExp is a Haskell function, too
  if($macroExp(x, y)) { return x; }
  else { return a; }
}

int32_t foo100(int32_t x) {
  return x;
}

-- Calling functions (with return values) as expressions
int32_t bar82() {
  let y = foo100(4);
  return foo100(foo100(y));
}

------------------------------------------------------------
-- Larger example (hypothetical pack and unpack functions). In practice, use
-- ivory-serialize.
int32_t unpackSint32(uint8_t a, uint8_t b, uint8_t c, uint8_t d)
{
  alloc *x = 0;
  store *x as safeCast(a) | safeCast(b) << 0x8 | safeCast(c) << 0x10 | safeCast(d) << 0x18;
  return twosCompCast(*x);
}

int32_t unpack(* uint8_t[10] msg, int32_t ixx)
{
 let ix = toIx(ixx);
 let res = unpackSint32( * msg @ ix
                       , * msg @ (ix+1)
                       , * msg @ (ix+2)
                       , * msg @ (ix+3)
                       );
 return res;
}

void pack(*uint8_t[10] msg, int32_t x, int32_t ixx)
{
  let ix = toIx(ixx);
  let ux = twosCompRep(x);
  store * msg @ ix     as bitCast(ux);
  store * msg @ (ix+1) as bitCast(ux >> 0x08);
  store * msg @ (ix+2) as bitCast(ux >> 0x10);
  store * msg @ (ix+3) as bitCast(ux >> 0x18);
}

struct Foo7 { struct ivory_string_FooStr aFoo7; }

struct Bar2 { int32_t aBar7; }

struct Boo { struct Bar2 aBoo; }

void foobar (* struct Foo7 s) {
  store (s . aFoo7 . stringDataL[0]) as 3;
  return;
}

int32_t bar72(int32_t x, * struct Boo a, * int32_t y, * int32_t[10] arr) {
  store (a . aBoo) -> aBar7 as *y;
  store (a . aBoo) -> aBar7 as *y;
  store arr[toIx(*y + *y)]  as *y + *y;
  return a.aBoo->aBar7;
}

|]
