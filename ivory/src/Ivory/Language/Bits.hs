{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Language.Bits where

import Ivory.Language.Type
import Ivory.Language.Uint
import Ivory.Language.Cast

import Data.Word
import Data.Bits(finiteBitSize)

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
  iBitSize _ = finiteBitSize (0 :: Word8)
instance IvoryBits Uint16 where
  iBitSize _ = finiteBitSize (0 :: Word16)
instance IvoryBits Uint32 where
  iBitSize _ = finiteBitSize (0 :: Word32)
instance IvoryBits Uint64 where
  iBitSize _ = finiteBitSize (0 :: Word64)

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

-- | Extract the least significant byte from an integer.  This returns
-- the two values (x & 0xFF, x >> 8), with the first value safely
-- casted to an 8-bit integer.
--
-- This is convenient to use with a state monad and "sets", such as:
--
-- > fst $ runState x $ do
-- >   a <- sets extractByte
-- >   b <- sets extractByte
-- >   return (a, b)
extractByte :: (BitCast a Uint8) => a -> (Uint8, a)
extractByte x = (bitCast x, x `iShiftR` 8)
