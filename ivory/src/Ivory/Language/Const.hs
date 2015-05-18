{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Language.Const where

import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as AST

-- Extern constants ------------------------------------------------------------

-- | Import an externally defined constant by providing a global name.
extern :: forall t. IvoryExpr t => AST.Sym -> String -> t
extern sym file = wrapExpr $ AST.ExpExtern $ AST.Extern
  { AST.externSym  = sym
  , AST.externFile = file
  , AST.externType = ivoryType (Proxy :: Proxy t)
  }
