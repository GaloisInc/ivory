{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Language.Assert where

import Ivory.Language.Monad
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as AST

assert :: forall a eff. IvoryExpr a => a -> Ivory eff ()
assert e = emit (AST.Assert (unwrapExpr e))

assume :: forall a eff. IvoryExpr a => a -> Ivory eff ()
assume e = emit (AST.Assume (unwrapExpr e))

