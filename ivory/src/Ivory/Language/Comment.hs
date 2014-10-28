
module Ivory.Language.Comment where

import Ivory.Language.Monad
import qualified Ivory.Language.Syntax as AST

comment :: String -> Ivory eff ()
comment c = emit (AST.Comment $ AST.UserComment c)
