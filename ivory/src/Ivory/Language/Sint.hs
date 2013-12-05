{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Language.Sint where

import Ivory.Language.Area
import Ivory.Language.BoundedInteger
import Ivory.Language.SizeOf
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

import Data.Int (Int8,Int16,Int32,Int64)

-- Signed Types ----------------------------------------------------------------

-- | 8-bit integers.
newtype Sint8 = Sint8 { getSint8 :: I.Expr }

instance IvoryType Sint8 where
  ivoryType _ = I.TyInt I.Int8

instance IvoryVar Sint8 where
  wrapVar    = wrapVarExpr
  unwrapExpr = getSint8

instance IvoryExpr Sint8 where
  wrapExpr = Sint8

instance IvorySizeOf (Stored Sint8) where
  sizeOfBytes _ = 1

instance Num Sint8 where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = exprUnary abs
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = boundedFromInteger Sint8 (0 :: Int8)

instance Bounded Sint8 where
  minBound = fromIntegral (minBound :: Int8)
  maxBound = fromIntegral (maxBound :: Int8)


-- | 16-bit integers.
newtype Sint16 = Sint16 { getSint16 :: I.Expr }

instance IvoryType Sint16 where
  ivoryType _ = I.TyInt I.Int16

instance IvoryVar Sint16 where
  wrapVar    = wrapVarExpr
  unwrapExpr = getSint16

instance IvoryExpr Sint16 where
  wrapExpr = Sint16

instance IvorySizeOf (Stored Sint16) where
  sizeOfBytes _ = 2

instance Num Sint16 where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = exprUnary abs
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = boundedFromInteger Sint16 (0 :: Int16)

instance Bounded Sint16 where
  minBound = fromIntegral (minBound :: Int16)
  maxBound = fromIntegral (maxBound :: Int16)


-- | 32-bit integers.
newtype Sint32 = Sint32 { getSint32 :: I.Expr }

instance IvoryType Sint32 where
  ivoryType _ = I.TyInt I.Int32

instance IvoryVar Sint32 where
  wrapVar    = wrapVarExpr
  unwrapExpr = getSint32

instance IvoryExpr Sint32 where
  wrapExpr = Sint32

instance IvorySizeOf (Stored Sint32) where
  sizeOfBytes _ = 4

instance Num Sint32 where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = exprUnary abs
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = boundedFromInteger Sint32 (0 :: Int32)

instance Bounded Sint32 where
  minBound = fromIntegral (minBound :: Int32)
  maxBound = fromIntegral (maxBound :: Int32)


-- | 64-bit integers.
newtype Sint64 = Sint64 { getSint64 :: I.Expr }

instance IvoryType Sint64 where
  ivoryType _ = I.TyInt I.Int64

instance IvoryVar Sint64 where
  wrapVar    = wrapVarExpr
  unwrapExpr = getSint64

instance IvoryExpr Sint64 where
  wrapExpr = Sint64

instance IvorySizeOf (Stored Sint64) where
  sizeOfBytes _ = 8

instance Num Sint64 where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = exprUnary abs
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = boundedFromInteger Sint64 (0 :: Int64)

instance Bounded Sint64 where
  minBound = fromIntegral (minBound :: Int64)
  maxBound = fromIntegral (maxBound :: Int64)
