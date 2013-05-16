module Ivory.Language.Const where

import Ivory.Language.Type
import qualified Ivory.Language.Syntax as AST

-- Extern constants ------------------------------------------------------------

-- | Import and externally defined constant by providing a global name.
extern :: IvoryExpr t => AST.Sym -> t
extern = wrapExpr . AST.ExpSym
