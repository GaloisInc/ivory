module Ivory.Language.IString where

import qualified Ivory.Language.Syntax as AST
import           Ivory.Language.Type

import           Data.String           (IsString (..))


newtype IString = IString { getIString :: AST.Expr }

instance IvoryType IString where
  ivoryType _ = AST.TyConstRef AST.TyChar

instance IvoryVar IString where
  wrapVar    = wrapVarExpr
  unwrapExpr = getIString

instance IvoryExpr IString where
  wrapExpr = IString

instance IsString IString where
  fromString str = wrapExpr $ AST.ExpLit $ AST.LitString str
