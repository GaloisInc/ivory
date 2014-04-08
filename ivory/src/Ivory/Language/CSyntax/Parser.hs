{-# LANGUAGE DeriveDataTypeable #-}
--
-- Parser for C-like statements into (Ivory) template-haskell.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.Parser
  ( ivoryCParser
  ) where

import Prelude hiding (exp, init)
import Text.Parsec
import Text.Parsec.String (Parser)

import Language.Haskell.TH hiding (Stmt, Exp, litP, Type)

import Ivory.Language.CSyntax.Parsers.ProcParser
import Ivory.Language.CSyntax.ParseAST
import qualified Ivory.Language.CSyntax.TokenParser as T

--------------------------------------------------------------------------------
-- Turn a parser into a QuasiQuoter.

-- | Run a parser on a string with file, line, and column info.
mParse :: Monad m => Parser a -> (String, Int, Int) -> String -> m a
mParse parser (file, line, col) str =
  case parse parser' "" str of
    Left err  -> fail $ show err
    Right e   -> return e
  where
    parser' = do
      pos <- getPosition
      setPosition
        $ (flip setSourceName) file
        $ (flip setSourceLine) line
        $ (flip setSourceColumn) col
        pos
      T.whiteSpace
      p <- parser
      eof -- Ensure end of input reached
      return p

-- | Run a parser returning a value in the Q monad.
qParse :: Parser a -> String -> Q a
qParse parser str = do
  loc <- location
  let body = loc_start loc
  mParse parser (loc_filename loc, fst body, snd body) str

ivoryCParser :: String -> Q [ProcDef]
ivoryCParser = qParse (many procP)

--------------------------------------------------------------------------------
-- Construct Ivory module.






--------------------------------------------------------------------------------
-- Program parsers.

-- parseInit :: P String -> P Init
-- parseInit p = do init <- p; return (Ival init)

--------------------------------------------------------------------------------

-- test :: String -> IO [(Type, String)]
-- test = mParse undefined ("",0,0)
--   where
--   -- run0 = T.parens (T.commaSep T.identifier)
--   -- run = T.parens (T.commaSep tyarg)
--   -- tyarg = (,) <$> typeP <*> T.identifier

-- a = " a :=  3 ; b = 4;   return (a + b); "
-- b = " if(a;) {b;} {c;} "
-- c = "if (abas) {a := 3;} {asadf := 4;}"
-- d = " a :=  3 ; "
-- e = "7 ? 8 : 3+4"
-- f = "return (4+5)"
-- g = "return (a ? 3 : 4)"
-- h = "a & b"
-- i = "a >= b"
-- j = "foo()"
-- k = "v = foo()"
-- l = "void foo (g * uint16_t a, s* uint8_t b, *uint8_t c) {}"
-- m = "g * uint16_t"
-- n = "g * uint16_t a"
-- o = "(g * uint16_t a, s * uint8_t b, *uint8_t c)"
-- p = "(a, b,c)"
