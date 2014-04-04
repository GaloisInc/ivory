{-# LANGUAGE LambdaCase #-}

--
-- Procedure parser.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.Parsers.ProcParser where

import Control.Applicative hiding ((<|>), many)

import Ivory.Language.CSyntax.Parsers.Common
import Ivory.Language.CSyntax.Parsers.StmtParser
import Ivory.Language.CSyntax.Parsers.TypeParser

import Ivory.Language.CSyntax.ParseAST
import qualified Ivory.Language.CSyntax.TokenParser as T

--------------------------------------------------------------------------------

-- Procedure parser
-- > retType foo(argTy0, argTy1, ...) { stmts }
procP :: P ProcDef
procP = ProcDef
      <$> tyP
      <*> T.identifier
      <*> tyArgs
      <*> blockP
  where
  tyArgs = T.parens (T.commaSep tyArg)
  tyArg  = (,) <$> tyP <*> T.identifier

--------------------------------------------------------------------------------

