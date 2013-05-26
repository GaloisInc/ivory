{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Language.Array where

import Ivory.Language.Area
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Sint
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

import GHC.TypeLits (SingI(..),Sing,Nat)


-- Arrays ----------------------------------------------------------------------

-- Note: it is assumed in ivory-opts and the ivory-backend that the associated
-- type is an Sint32, so this should not be changed in the front-end without
-- modifying the other packages.
type IxRep = Sint32

-- | Values in the range @0 .. n-1@.
newtype Ix (n :: Nat) = Ix { getIx :: I.Expr }

instance (SingI n) => IvoryType (Ix n) where
  ivoryType _ = ivoryType (Proxy :: Proxy IxRep)

instance (SingI n) => IvoryVar (Ix n) where
  wrapVar    = wrapVarExpr
  unwrapExpr = getIx

instance (SingI n) => IvoryExpr (Ix n) where
  wrapExpr = Ix

instance (SingI n) => IvoryStore (Ix n)

instance (SingI n) => Num (Ix n) where
  (*)           = ixBinop (*)
  (-)           = ixBinop (-)
  (+)           = ixBinop (+)
  abs           = ixUnary abs
  signum        = ixUnary signum
  fromInteger   = mkIx . fromInteger

fromIx :: SingI n => Ix n -> IxRep
fromIx = wrapExpr . unwrapExpr

-- | Casting from a bounded Ivory expression to an index.  This is safe,
-- although the value may be truncated.  Furthermore, indexes are always
-- positive.
toIx :: (IvoryExpr a, Bounded a, SingI n) => a -> Ix n
toIx = mkIx . unwrapExpr

-- | The number of elements that an index covers.
ixSize :: forall n. (SingI n) => Ix n -> Integer
ixSize _ = fromTypeNat (sing :: Sing n)

arrayLen :: forall s len area n ref.
            (Num n, SingI len, IvoryType area, IvoryRef ref)
         => ref s (Array len area) -> n
arrayLen _ = fromInteger (fromTypeNat (sing :: Sing len))

-- | Array indexing.
(!) :: forall s len area ref.
       ( SingI len, IvoryType area, IvoryRef ref
       , IvoryExpr (ref s (Array len area)), IvoryExpr (ref s area))
    => ref s (Array len area) -> Ix len -> ref s area
arr ! ix = wrapExpr (I.ExpIndex ty (unwrapExpr arr) ixRep (getIx ix))
  where
  ty    = ivoryType (Proxy :: Proxy (Array len area))
  ixRep = ivoryType (Proxy :: Proxy IxRep)

-- XXX don't export
mkIx :: forall n. (SingI n) => I.Expr -> Ix n
mkIx e = wrapExpr (I.ExpToIx e base)
  where
  base = ixSize (undefined :: Ix n)

-- XXX don't export
ixBinop :: (SingI n)
        => (I.Expr -> I.Expr -> I.Expr)
        -> (Ix n -> Ix n -> Ix n)
ixBinop f x y = mkIx $ f (rawIxVal x) (rawIxVal y)

-- XXX don't export
ixUnary :: (SingI n) => (I.Expr -> I.Expr) -> (Ix n -> Ix n)
ixUnary f = mkIx . f . rawIxVal

-- XXX don't export
rawIxVal :: SingI n => Ix n -> I.Expr
rawIxVal n = case unwrapExpr n of
               I.ExpToIx e _ -> e
               _             -> error "Front-end: can't unwrap ixVal."
