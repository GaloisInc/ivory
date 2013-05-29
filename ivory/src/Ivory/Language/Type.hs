{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Language.Type where

import Ivory.Language.Proxy
import qualified Ivory.Language.Syntax as AST

-- Ivory Types -----------------------------------------------------------------

-- | The connection between haskell and ivory types.
class IvoryType (t :: a) where
  ivoryType :: Proxy t -> AST.Type

-- void
instance IvoryType () where
  ivoryType _ = AST.TyVoid

-- | Lifting a variable name.
class IvoryType t => IvoryVar t where
  wrapVar    :: AST.Var -> t
  unwrapExpr :: t -> AST.Expr

-- | Unwrapping for ivory expressions.
class IvoryVar t => IvoryExpr t where
  wrapExpr   :: AST.Expr -> t

-- Utilities -------------------------------------------------------------------

-- XXX do not export
wrapVarExpr :: IvoryExpr t => AST.Var -> t
wrapVarExpr  = wrapExpr . AST.ExpVar

-- XXX do not export
typedExpr :: forall t. IvoryVar t => t -> AST.Typed AST.Expr
typedExpr t = AST.Typed
  { AST.tType  = ivoryType (Proxy :: Proxy t)
  , AST.tValue = unwrapExpr t
  }

-- XXX do not export
exprBinop :: IvoryExpr a => (AST.Expr -> AST.Expr -> AST.Expr) -> (a -> a -> a)
exprBinop k x y = wrapExpr (k (unwrapExpr x) (unwrapExpr y))

-- XXX do not export
exprUnary :: IvoryExpr a => (AST.Expr -> AST.Expr) -> (a -> a)
exprUnary k x = wrapExpr (k (unwrapExpr x))
