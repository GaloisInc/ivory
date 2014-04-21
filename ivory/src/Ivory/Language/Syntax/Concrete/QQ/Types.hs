--
-- Ivory QuasiQuoter helper types.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.Types where

import Ivory.Language.Syntax.Concrete.ParseAST

--------------------------------------------------------------------------------

data DerefExp
  = RefExp RefVar
  | RefArrIxExp RefVar Exp
  deriving (Eq, Show)
