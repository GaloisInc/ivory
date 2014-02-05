{-# LANGUAGE DeriveDataTypeable #-}

--
-- Parser for C-like statements into (Ivory) template-haskell.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.Parser
  ( ivoryCParser
  , Stmt(..)
  ) where

import Prelude hiding (exp)
import Text.Parsec
import Text.Parsec.String (Parser)

import Language.Haskell.TH hiding (Stmt)

import Control.Monad
import Data.Generics

--------------------------------------------------------------------------------

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
      spaces
      p <- parser
      eof -- Ensure end of input reached
      return p

-- | Run a parser returning a value in the Q monad.
qParse :: Parser a -> String -> Q a
qParse parser str = do
  loc <- location
  let body = loc_start loc
  mParse parser (loc_filename loc, fst body, snd body) str

ivoryCParser :: String -> Q [Stmt]
ivoryCParser = qParse programP

--------------------------------------------------------------------------------

-- | To be interpreted as an Ivory expression.
type IvoryExp = String

-- | AST for parsing C-like statements.
data Stmt
  = Assign String IvoryExp -- var, ivory expression
  | Return IvoryExp
  deriving (Show, Read, Typeable, Data)

--------------------------------------------------------------------------------

{-
  a = 3 + 4;
  b = 2;
  return (a + b);

  a <- assign (3 + 4)
  b <- assign 2
  ret (a + b)
-}


type P s          = Parsec String () s

lexeme :: P String -> P String
lexeme p = do x <- p; spaces; return x

symbolChar :: P Char
symbolChar = alphaNum <|> char '_'

symbol :: String -> P String
symbol = lexeme . string

var :: P String
var = lexeme (many1 symbolChar)

skipSym :: String -> P ()
skipSym = void . symbol

semi :: P ()
semi = skipSym ";"

-- XXX hack for now to grab all the Ivory code.  later, we probably want to have
-- anti quotation for this.
untilEnd :: P String
untilEnd = do
  exp <- manyTill anyToken semi
  let exp' = reverse $ dropWhile (== ' ') (reverse exp)
  return exp'

assignP :: P Stmt
assignP = do
  spaces
  v <- var
  skipSym "="
  spaces
  exp <- untilEnd
  return (Assign v exp)

returnP :: P Stmt
returnP = do
  skipSym "return"
  spaces
  exp <- untilEnd
  return (Return exp)

stmtsP :: P Stmt
stmtsP = try returnP <|> assignP

programP :: P [Stmt]
programP = many stmtsP

{-
test :: String -> IO [Stmt]
test = mParse program ("",0,0)

program = " a =  3 ; b = 4;   return (a + b); "
-}
