{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Language.IBool where

import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Sint
import Ivory.Language.Type
import Ivory.Language.Uint
import qualified Ivory.Language.Syntax as AST


-- Booleans --------------------------------------------------------------------

newtype IBool = IBool { getIBool :: AST.Expr }

instance IvoryType IBool where
  ivoryType _ = AST.TyBool

instance IvoryVar IBool where
  wrapVar    = wrapVarExpr
  unwrapExpr = getIBool

instance IvoryExpr IBool where
  wrapExpr = IBool

-- IfTE Support ----------------------------------------------------------------

-- | If-then-else.
ifte_ :: IBool -> Ivory eff a -> Ivory eff b -> Ivory eff ()
ifte_ cmp t f = do
  (_,tb) <- collect t
  (_,fb) <- collect f
  emit (AST.IfTE (unwrapExpr cmp) (blockStmts tb) (blockStmts fb))

-- | Conditional expressions.
(?) :: IvoryExpr a => IBool -> (a,a) -> a
cond ? (t,f) = wrapExpr
             $ AST.ExpOp AST.ExpCond [unwrapExpr cond,unwrapExpr t,unwrapExpr f]


-- Constants -------------------------------------------------------------------

true :: IBool
true  = wrapExpr (AST.ExpLit (AST.LitBool True))

false :: IBool
false  = wrapExpr (AST.ExpLit (AST.LitBool False))


-- Comparisons -----------------------------------------------------------------

-- XXX do not export
boolOp :: forall a. IvoryVar a => (AST.Type -> AST.ExpOp) -> a -> a -> IBool
boolOp op a b = wrapExpr (AST.ExpOp (op ty) [unwrapExpr a,unwrapExpr b])
  where
  ty = ivoryType (Proxy :: Proxy a)

class IvoryExpr a => IvoryEq a where
  (==?) :: a -> a -> IBool
  (==?)  = boolOp AST.ExpEq
  infix 4 ==?

  (/=?) :: a -> a -> IBool
  (/=?)  = boolOp AST.ExpNeq
  infix 4 /=?

class IvoryEq a => IvoryOrd a where
  (>?)  :: a -> a -> IBool
  (>?)   = boolOp (AST.ExpGt False)
  infix 4 >?

  (>=?) :: a -> a -> IBool
  (>=?)  = boolOp (AST.ExpGt True)
  infix 4 >=?

  (<?)  :: a -> a -> IBool
  (<?)   = boolOp (AST.ExpLt False)
  infix 4 <?

  (<=?) :: a -> a -> IBool
  (<=?)  = boolOp (AST.ExpLt True)
  infix 4 <=?

instance IvoryEq  IBool
instance IvoryOrd IBool

instance IvoryEq  Uint8
instance IvoryOrd Uint8
instance IvoryEq  Uint16
instance IvoryOrd Uint16
instance IvoryEq  Uint32
instance IvoryOrd Uint32
instance IvoryEq  Uint64
instance IvoryOrd Uint64

instance IvoryEq  Sint8
instance IvoryOrd Sint8
instance IvoryEq  Sint16
instance IvoryOrd Sint16
instance IvoryEq  Sint32
instance IvoryOrd Sint32
instance IvoryEq  Sint64
instance IvoryOrd Sint64

-- Boolean logic ---------------------------------------------------------------

iNot :: IBool -> IBool
iNot a = wrapExpr (AST.ExpOp AST.ExpNot [unwrapExpr a])

(.&&) :: IBool -> IBool -> IBool
l .&& r = wrapExpr (AST.ExpOp AST.ExpAnd [unwrapExpr l,unwrapExpr r])
infixr 3 .&&

(.||) :: IBool -> IBool -> IBool
l .|| r = wrapExpr (AST.ExpOp AST.ExpOr [unwrapExpr l,unwrapExpr r])
infixr 2 .||
