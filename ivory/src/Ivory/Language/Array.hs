{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Language.Array (
    Ix(),
    IxRep, ixRep,
    fromIx,
    toIx,
    ixSize,
    arrayLen,
    (!),
  ) where

import Ivory.Language.IBool
import Ivory.Language.Area
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Sint
import Ivory.Language.IIntegral
import Ivory.Language.Type
import Ivory.Language.Cast
import qualified Ivory.Language.Syntax as I

import GHC.TypeLits (Nat)

--------------------------------------------------------------------------------
-- Indexes

-- Note: it is assumed in ivory-opts and the ivory-backend that the associated
-- type is an Sint32, so this should not be changed in the front-end without
-- modifying the other packages.
type IxRep = Sint32

-- | The representation type of a @TyIndex@, this is fixed to @Int32@ for the
-- time being.
ixRep :: I.Type
ixRep = ivoryType (Proxy :: Proxy IxRep)

-- | Values in the range @0 .. n-1@.
newtype Ix (n :: Nat) = Ix { getIx :: I.Expr }

instance (ANat n) => IvoryType (Ix n) where
  ivoryType _ = I.TyIndex (fromTypeNat (aNat :: NatType n))

instance (ANat n) => IvoryVar (Ix n) where
  wrapVar    = wrapVarExpr
  unwrapExpr = getIx

instance (ANat n) => IvoryExpr (Ix n) where
  wrapExpr e | 0 /= fromTypeNat (aNat :: NatType n) = Ix e
             | otherwise = error "cannot have an index with width 0"

instance (ANat n) => IvoryStore (Ix n)

instance (ANat n) => Num (Ix n) where
  (*)           = ixBinop (*)
  (-)           = ixBinop (-)
  (+)           = ixBinop (+)
  abs           = ixUnary abs
  signum        = ixUnary signum
  fromInteger   = mkIx . fromInteger

instance (ANat n) => IvoryEq  (Ix n)
instance (ANat n) => IvoryOrd (Ix n)

fromIx :: ANat n => Ix n -> IxRep
fromIx = wrapExpr . unwrapExpr

-- | Casting from a bounded Ivory expression to an index.  This is safe,
-- although the value may be truncated.  Furthermore, indexes are always
-- positive.
toIx :: forall a n. (SafeCast a IxRep, ANat n) => a -> Ix n
toIx = mkIx . unwrapExpr . (safeCast :: a -> IxRep)

-- | The number of elements that an index covers.
ixSize :: forall n. (ANat n) => Ix n -> Integer
ixSize _ = fromTypeNat (aNat :: NatType n)

instance ( ANat n, IvoryIntegral to, Default to
         ) => SafeCast (Ix n) to where
  safeCast ix | Just s <- toMaxSize (ivoryType (Proxy :: Proxy to))
              , ixSize ix <= s
              = ivoryCast (fromIx ix)
              | otherwise
              = error ixCastError
  -- -- It doesn't make sense to case an index downwards dynamically.
  -- inBounds _ _ = error ixCastError

ixCastError :: String
ixCastError = "Idx cast : cannot cast index: result type is too small."

-- XXX don't export
mkIx :: forall n. (ANat n) => I.Expr -> Ix n
mkIx e = wrapExpr (I.ExpToIx e base)
  where
  base = ixSize (undefined :: Ix n)

-- XXX don't export
ixBinop :: (ANat n)
        => (I.Expr -> I.Expr -> I.Expr)
        -> (Ix n -> Ix n -> Ix n)
ixBinop f x y = mkIx $ f (rawIxVal x) (rawIxVal y)

-- XXX don't export
ixUnary :: (ANat n) => (I.Expr -> I.Expr) -> (Ix n -> Ix n)
ixUnary f = mkIx . f . rawIxVal

-- XXX don't export
rawIxVal :: ANat n => Ix n -> I.Expr
rawIxVal n = case unwrapExpr n of
               I.ExpToIx e _  -> e
               e@(I.ExpVar _) -> e
               e             -> error $ "Front-end: can't unwrap ixVal: "
                             ++ show e

-- Arrays ----------------------------------------------------------------------

arrayLen :: forall s len area n ref.
            (Num n, ANat len, IvoryArea area, IvoryRef ref)
         => ref s ('Array len area) -> n
arrayLen _ = fromInteger (fromTypeNat (aNat :: NatType len))

-- | Array indexing.
(!) :: forall s len area ref.
       ( ANat len, IvoryArea area, IvoryRef ref
       , IvoryExpr (ref s ('Array len area)), IvoryExpr (ref s area))
    => ref s ('Array len area) -> Ix len -> ref s area
arr ! ix = wrapExpr (I.ExpIndex ty (unwrapExpr arr) ixRep (getIx ix))
  where
  ty    = ivoryArea (Proxy :: Proxy ('Array len area))
