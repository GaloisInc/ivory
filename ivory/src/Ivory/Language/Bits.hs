{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Language.Bits where

import Ivory.Language.Cast
import Ivory.Language.IBool
import Ivory.Language.Sint
import Ivory.Language.Type
import Ivory.Language.Uint
import Ivory.Language.IIntegral

import Data.Word()

import qualified Ivory.Language.Syntax as AST

-- XXX do not export
bitOp :: forall a. IvoryExpr a => AST.ExpOp -> a -> a -> a
bitOp op a b = wrapExpr (AST.ExpOp op [unwrapExpr a, unwrapExpr b])

class (Num a, IvoryExpr a) => IvoryBits a where
  (.&) :: a -> a -> a
  (.&) = bitOp AST.ExpBitAnd

  (.|) :: a -> a -> a
  (.|) = bitOp AST.ExpBitOr

  (.^) :: a -> a -> a
  (.^) = bitOp AST.ExpBitXor

  iComplement :: a -> a
  iComplement a = wrapExpr (AST.ExpOp AST.ExpBitComplement [unwrapExpr a])

  iBitSize :: a -> Int

  -- XXX what should the type of the shift count argument be?  having
  -- it be polymorphic is kind of a pain.  for now, just have it be
  -- the same type as the value being shifted.
  iShiftL :: a -> a -> a
  iShiftL = bitOp AST.ExpBitShiftL

  iShiftR :: a -> a -> a
  iShiftR = bitOp AST.ExpBitShiftR

instance IvoryBits Uint8 where
  iBitSize _ = 8
instance IvoryBits Uint16 where
  iBitSize _ = 16
instance IvoryBits Uint32 where
  iBitSize _ = 32
instance IvoryBits Uint64 where
  iBitSize _ = 64

-- | Extraction of the upper or lower half of a bit type into the next
-- smallest bit type.
class (IvoryBits a, IvoryBits b) => BitSplit a b | a -> b where
  ubits :: a -> b
  lbits :: a -> b

instance BitSplit Uint64 Uint32 where
  ubits x = ivoryCast ((x `iShiftR` 32) .& 0xffffffff)
  lbits x = ivoryCast (x .& 0xffffffff)

instance BitSplit Uint32 Uint16 where
  ubits x = ivoryCast ((x `iShiftR` 16) .& 0xffff)
  lbits x = ivoryCast (x .& 0xffff)

instance BitSplit Uint16 Uint8 where
  ubits x = ivoryCast ((x `iShiftR` 8) .& 0xff)
  lbits x = ivoryCast (x .& 0xff)

-- | A narrowing cast from one bit type to another.  This explicitly
-- discards the upper bits of the input value to return a smaller
-- type, and is only defined for unsigned integers.
class (IvoryBits a, IvoryBits b) => BitCast a b where
  bitCast :: a -> b

-- Uint64:
instance BitCast Uint64 Uint64 where
  bitCast = id

instance BitCast Uint64 Uint32 where
  bitCast = lbits

instance BitCast Uint64 Uint16 where
  bitCast = lbits . lbits

instance BitCast Uint64 Uint8 where
  bitCast = lbits . lbits . lbits

-- Uint32:
instance BitCast Uint32 Uint32 where
  bitCast = id

instance BitCast Uint32 Uint16 where
  bitCast = lbits

instance BitCast Uint32 Uint8 where
  bitCast = lbits . lbits

-- Uint16:
instance BitCast Uint16 Uint16 where
  bitCast = id

instance BitCast Uint16 Uint8 where
  bitCast = lbits

-- Uint8:
instance BitCast Uint8 Uint8 where
  bitCast = id

-- | Re-interpret the bits of an unsigned integer as though they were a
-- signed number in two's complement representation.
class ( IvoryBits unsigned
      , IvoryEq unsigned
      , IvoryExpr signed
      , Num signed
      , IvoryIntegral unsigned
      , Bounded unsigned
      , Bounded signed
      , IvoryOrd signed
      ) => TwosComplementCast unsigned signed
      | signed -> unsigned, unsigned -> signed where

  -- The C standard makes conversion from unsigned to signed
  -- well-defined as long as the value can be preserved. But if the
  -- unsigned value is bigger than the largest representable value of
  -- the signed type, as is the case for a value we want to treat as
  -- negative, the behavior is implementation-specific, and may trap.
  --
  -- This algorithm checks the most significant bit, which is the sign
  -- bit. If it is clear, then the value is representable in the target
  -- type (assuming they're the same bit-width) and we can just cast.
  --
  -- One way to define two's complement is that -x = ~x + 1. Negating
  -- both sides gives x = -~x - 1, and that is the identity we use when
  -- the sign bit is set.
  --
  -- If the sign bit was set, then if we flip every bit we will have a
  -- value that is representable and we can cast it to the target type.
  -- So we insert the cast after complementing the value but before
  -- negating and subtracting 1.
  --
  -- On machines that natively use two's complement for signed numbers,
  -- this should optimize away to use zero instructions. However, the C
  -- standard also permits implementations that use one's complement or
  -- sign-magnitude representations, and this algorithm is expected to
  -- work in those implementations as well.
  twosComplementCast :: unsigned -> signed
  twosComplementCast v = ((v `iShiftR` n) ==? 1) ?
    ( negate (ivoryCast (iComplement v)) - 1
    , ivoryCast v)
    where
    n = fromIntegral (iBitSize v - 1)

  -- Takes a signed value interpreted as a two's complement, and returns an
  -- unsigned value with the identity bit pattern. For the instances below, this
  -- is guaranteed not to overflow.
  twosComplementRep :: signed -> unsigned
  twosComplementRep v = (v <? 0) ?
    ( m - (s1 - 1)
    , ivoryCast v
    )
    where
    m :: unsigned
    m = maxBound :: unsigned
    -- v is negative when s1 is used. If v == minBound, then (-v) overflows the
    -- signed type. So we find maxBound of the signed type within the unsigned
    -- type with maxBound / 2 + 1 (e.g., 255/2+1 == 128 for int8_t and uint8_t).
    s1 :: unsigned
    s1 = (v ==? minBound) ? (maxBound `iDiv` 2 + 1, ivoryCast $ negate v)

-- Instances *must* have the same bit-width.
instance TwosComplementCast Uint8  Sint8
instance TwosComplementCast Uint16 Sint16
instance TwosComplementCast Uint32 Sint32
instance TwosComplementCast Uint64 Sint64

-- | Extract the least significant byte from an integer.  This returns
-- the two values (x & 0xFF, x >> 8), with the first value safely
-- casted to an 8-bit integer.
--
-- This is convenient to use with a state monad and 'Control.Monad.State.state', such as:
--
-- > evalState x $ do
-- >   a <- state extractByte
-- >   b <- state extractByte
-- >   return (a, b)
extractByte :: (BitCast a Uint8) => a -> (Uint8, a)
extractByte x = (bitCast x, x `iShiftR` 8)
