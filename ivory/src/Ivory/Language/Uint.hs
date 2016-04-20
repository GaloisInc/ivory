module Ivory.Language.Uint where

import Ivory.Language.BoundedInteger
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

import Data.Word (Word8,Word16,Word32,Word64)


-- Unsigned Types --------------------------------------------------------------

-- | 8-bit words.
newtype Uint8 = Uint8 { getUint8 :: I.Expr }
  deriving Show

instance IvoryType Uint8 where
  ivoryType _ = I.TyWord I.Word8

instance IvoryVar Uint8 where
  wrapVar    = wrapVarExpr
  unwrapExpr = getUint8

instance IvoryExpr Uint8 where
  wrapExpr = Uint8

instance Num Uint8 where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = id
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = boundedFromInteger Uint8 (0 :: Word8)

instance Bounded Uint8 where
  minBound = 0
  maxBound = wrapExpr (I.ExpMaxMin True)


-- | 16-bit words.
newtype Uint16 = Uint16 { getUint16 :: I.Expr }

instance IvoryType Uint16 where
  ivoryType _ = I.TyWord I.Word16

instance IvoryVar Uint16 where
  wrapVar    = wrapVarExpr
  unwrapExpr = getUint16

instance IvoryExpr Uint16 where
  wrapExpr = Uint16

instance Num Uint16 where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = id
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = boundedFromInteger Uint16 (0 :: Word16)

instance Bounded Uint16 where
  minBound = 0
  maxBound = wrapExpr (I.ExpMaxMin True)


-- | 32-bit words.
newtype Uint32 = Uint32 { getUint32 :: I.Expr }

instance IvoryType Uint32 where
  ivoryType _ = I.TyWord I.Word32

instance IvoryVar Uint32 where
  wrapVar    = wrapVarExpr
  unwrapExpr = getUint32

instance IvoryExpr Uint32 where
  wrapExpr = Uint32

instance Num Uint32 where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = id
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = boundedFromInteger Uint32 (0 :: Word32)

instance Bounded Uint32 where
  minBound = 0
  maxBound = wrapExpr (I.ExpMaxMin True)


-- | 64-bit words.
newtype Uint64 = Uint64 { getUint64 :: I.Expr }

instance IvoryType Uint64 where
  ivoryType _ = I.TyWord I.Word64

instance IvoryVar Uint64 where
  wrapVar    = wrapVarExpr
  unwrapExpr = getUint64

instance IvoryExpr Uint64 where
  wrapExpr = Uint64

instance Num Uint64 where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = id
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = boundedFromInteger Uint64 (0 :: Word64)

instance Bounded Uint64 where
  minBound = 0
  maxBound = wrapExpr (I.ExpMaxMin True)
