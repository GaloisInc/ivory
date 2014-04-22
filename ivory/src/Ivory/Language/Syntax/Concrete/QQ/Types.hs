--
-- Ivory QuasiQuoter helper types.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.Types where

import Ivory.Language.Syntax.Concrete.ParseAST

import Language.Haskell.TH (Name)

--------------------------------------------------------------------------------

-- | Valid dereference expressions
data DerefExp
  = RefExp RefVar
  | RefArrIxExp RefVar Exp
  | RefFieldExp RefVar FieldNm
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Dereference expression environment
type DerefVarEnv = [(DerefExp, Name)]
