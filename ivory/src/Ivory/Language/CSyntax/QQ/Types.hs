--
-- Ivory QuasiQuoter helper types.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.QQ.Types where

import Ivory.Language.CSyntax.ParseAST

--------------------------------------------------------------------------------

data DerefExp
  = RefExp RefVar
  | ArrIxExp RefVar Exp
  deriving (Eq, Show)
