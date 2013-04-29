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

type IxRep = Sint32

fromIx :: SingI n => Ix n -> IxRep
fromIx = wrapExpr . unwrapExpr

toIx :: (SingI n) => IxRep -> Ix n
toIx n = ixMod (unwrapExpr n)

-- | Values in the range @0 .. n-1@.
newtype Ix (n :: Nat) = Ix { getIx :: I.Expr }

-- | The number of elements that an index covers.
ixSize :: forall n. (SingI n) => Ix n -> Integer
ixSize _ = fromTypeNat (sing :: Sing n)

arrayLen :: forall s len area n ref.
            (Num n, SingI len, IvoryType area, IvoryRef ref)
         => ref s (Array len area) -> n
arrayLen _ = fromInteger (fromTypeNat (sing :: Sing len))

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
  fromInteger i = ixMod (fromInteger i)

-- XXX don't export
ixMod :: forall n. (SingI n) => I.Expr -> Ix n
ixMod e = wrapExpr (I.ExpOp I.ExpMod [e,base])
  where
  base = fromInteger (fromTypeNat (sing :: Sing n))

-- XXX don't export
ixBinop :: (SingI n)
        => (I.Expr -> I.Expr -> I.Expr)
        -> (Ix n -> Ix n -> Ix n)
ixBinop f x y = ixMod (f (unwrapExpr x) (unwrapExpr y))

-- XXX don't export
ixUnary :: (SingI n)
        => (I.Expr -> I.Expr) -> (Ix n -> Ix n)
ixUnary f a = ixMod (f (unwrapExpr a))

-- | Array indexing.
(!) :: forall s len area ref.
       ( SingI len, IvoryType area, IvoryRef ref
       , IvoryExpr (ref s (Array len area)), IvoryExpr (ref s area))
    => ref s (Array len area) -> Ix len -> ref s area
arr ! ix = wrapExpr (I.ExpIndex ty (unwrapExpr arr) ixRep (getIx ix))
  where
  ty    = ivoryType (Proxy :: Proxy (Array len area))
  ixRep = ivoryType (Proxy :: Proxy IxRep)
