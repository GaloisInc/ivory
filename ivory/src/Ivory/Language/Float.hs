{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Language.Float where

import Ivory.Language.Area
import Ivory.Language.IBool
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.SizeOf
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I


-- | NaN testing.
isnan :: forall a. (IvoryVar a, Floating a) => a -> IBool
isnan a = wrapExpr (I.ExpOp (I.ExpIsNan ty) [unwrapExpr a])
  where
  ty = ivoryType (Proxy :: Proxy a)

-- | Infinite testing.
isinf :: forall a. (IvoryVar a, Floating a) => a -> IBool
isinf a = wrapExpr (I.ExpOp (I.ExpIsInf ty) [unwrapExpr a])
  where
  ty = ivoryType (Proxy :: Proxy a)

-- Floating Point --------------------------------------------------------------

newtype IFloat = IFloat { getIFloat :: I.Expr }

ifloat :: Float -> IFloat
ifloat  = IFloat . I.ExpLit . I.LitFloat

instance IvoryType IFloat where
  ivoryType _ = I.TyFloat

instance IvoryVar IFloat where
  wrapVar    = wrapVarExpr
  unwrapExpr = getIFloat

instance IvoryExpr IFloat where
  wrapExpr = IFloat

instance IvorySizeOf (Stored IFloat) where
  sizeOfBytes _ = 4

instance IvoryEq  IFloat

instance IvoryOrd IFloat

instance IvoryStore IFloat

instance Num IFloat where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = exprUnary abs
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = ifloat . fromInteger

instance Fractional IFloat where
  (/)          = exprBinop (/)
  recip        = exprUnary recip
  fromRational = ifloat . fromRational

instance Floating IFloat where
  pi      = ifloat pi
  exp     = exprUnary exp
  sqrt    = exprUnary sqrt
  log     = exprUnary log
  (**)    = exprBinop (**)
  logBase = exprBinop (logBase)
  sin     = exprUnary sin
  tan     = exprUnary tan
  cos     = exprUnary cos
  asin    = exprUnary asin
  atan    = exprUnary atan
  acos    = exprUnary acos
  sinh    = exprUnary sinh
  tanh    = exprUnary tanh
  cosh    = exprUnary cosh
  asinh   = exprUnary asinh
  atanh   = exprUnary atanh
  acosh   = exprUnary acosh

-- Double Precision ------------------------------------------------------------

newtype IDouble = IDouble { getIDouble :: I.Expr }

idouble :: Double -> IDouble
idouble  = IDouble . I.ExpLit . I.LitDouble

instance Fractional IDouble where
  (/)          = exprBinop (/)
  recip        = exprUnary recip
  fromRational = idouble . fromRational

instance IvoryType IDouble where
  ivoryType _ = I.TyDouble

instance IvoryVar IDouble where
  wrapVar    = wrapVarExpr
  unwrapExpr = getIDouble

instance IvoryExpr IDouble where
  wrapExpr = IDouble

instance IvorySizeOf (Stored IDouble) where
  sizeOfBytes _ = 8

instance IvoryEq  IDouble

instance IvoryOrd IDouble

instance IvoryStore IDouble

instance Num IDouble where
  (*)         = exprBinop (*)
  (+)         = exprBinop (+)
  (-)         = exprBinop (-)
  abs         = exprUnary abs
  signum      = exprUnary signum
  negate      = exprUnary negate
  fromInteger = idouble . fromInteger

instance Floating IDouble where
  pi      = idouble pi
  exp     = exprUnary exp
  sqrt    = exprUnary sqrt
  log     = exprUnary log
  (**)    = exprBinop (**)
  logBase = exprBinop (logBase)
  sin     = exprUnary sin
  tan     = exprUnary tan
  cos     = exprUnary cos
  asin    = exprUnary asin
  atan    = exprUnary atan
  acos    = exprUnary acos
  sinh    = exprUnary sinh
  tanh    = exprUnary tanh
  cosh    = exprUnary cosh
  asinh   = exprUnary asinh
  atanh   = exprUnary atanh
  acosh   = exprUnary acosh


-- Rounding --------------------------------------------------------------------

-- XXX do not export
primRound :: IvoryExpr a => I.ExpOp -> a -> a
primRound op a = wrapExpr (I.ExpOp op [unwrapExpr a])

class (Floating a, IvoryExpr a) => IvoryFloat a where
  -- | Round a floating point number.
  roundF :: a -> a
  roundF  = primRound I.ExpRoundF

  -- | Take the ceiling of a floating point number.
  ceilF :: a -> a
  ceilF  = primRound I.ExpCeilF

  -- | Take the floor of a floating point number.
  floorF :: a -> a
  floorF  = primRound I.ExpFloorF

instance IvoryFloat IFloat
instance IvoryFloat IDouble
