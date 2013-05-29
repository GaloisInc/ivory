module Ivory.Language.IIntegral where

import Ivory.Language.Sint
import Ivory.Language.Type
import Ivory.Language.Uint
import qualified Ivory.Language.Syntax.AST as I

--------------------------------------------------------------------------------

-- | Integral, without the baggage from Haskell (i.e., supertypes of 'Real' and
-- 'Enum').
class (IvoryExpr a, Num a) => IvoryIntegral a where
  iDiv :: a -> a -> a
  iDiv l r = wrapExpr (iDivE (unwrapExpr l) (unwrapExpr r))

  -- | Has C semantics: like Haskell's `rem`.
  (.%) :: a -> a -> a
  l .% r = wrapExpr (iModE (unwrapExpr l) (unwrapExpr r))

iDivE :: I.Expr -> I.Expr -> I.Expr
iDivE l r = I.ExpOp I.ExpDiv [l,r]

-- | Has C semantics: like Haskell's `rem`.
iModE :: I.Expr -> I.Expr -> I.Expr
iModE l r = I.ExpOp I.ExpMod [l,r]

--------------------------------------------------------------------------------

instance IvoryIntegral Sint8
instance IvoryIntegral Sint16
instance IvoryIntegral Sint32
instance IvoryIntegral Sint64
instance IvoryIntegral Uint8
instance IvoryIntegral Uint16
instance IvoryIntegral Uint32
instance IvoryIntegral Uint64

--------------------------------------------------------------------------------
