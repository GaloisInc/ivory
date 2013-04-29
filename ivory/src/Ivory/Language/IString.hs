module Ivory.Language.IString where

import Ivory.Language.Type
import qualified Ivory.Language.Syntax as AST

import Data.String (IsString(..))


newtype IString = IString { getIString :: AST.Expr }

instance IvoryType IString where
  ivoryType _ = AST.TyPtr AST.TyChar

instance IvoryVar IString where
  wrapVar    = wrapVarExpr
  unwrapExpr = getIString

instance IvoryExpr IString where
  wrapExpr = IString

instance IsString IString where
  fromString str = wrapExpr $ AST.ExpLit $ case str of
    [] -> AST.LitNull
    _  -> AST.LitString str
