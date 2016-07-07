{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Language.Type where

import Ivory.Language.Proxy
import qualified Ivory.Language.Syntax as AST
import GHC.TypeLits (Symbol)

-- Ivory Types -----------------------------------------------------------------

-- | The connection between Haskell and Ivory types.
class IvoryType t where
  ivoryType :: Proxy t -> AST.Type

-- | @void@ type
instance IvoryType () where
  ivoryType _ = AST.TyVoid

-- | Lifting a variable name.
class IvoryType t => IvoryVar t where
  wrapVar    :: AST.Var -> t
  unwrapExpr :: t -> AST.Expr

-- | Unwrapping for Ivory expressions.
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

-- Proxy Type ------------------------------------------------------------------

-- | An opaque type that can never be implemented.
data OpaqueType = OpaqueType

instance IvoryType OpaqueType where
  ivoryType _ = AST.TyOpaque

-- New Types -------------------------------------------------------------------

newtype NewType (sym :: Symbol) = NewType { getNewType :: AST.Expr }

mkNewType :: (ASymbol sym) => Proxy (sym :: Symbol) -> AST.Type
mkNewType p = AST.TyNewType (fromTypeSym p)
