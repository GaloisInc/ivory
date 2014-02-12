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
  , Exp(..)
  , ExpOp(..)
  , Literal(..)
  , Init(..)
  , RefVar
--  , IvoryAnti(..)
  ) where

import Prelude hiding (exp, init)
import Text.Parsec
import Text.Parsec.String (Parser)

import Language.Haskell.TH hiding (Stmt, Exp)

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

type Var       = String
type RefVar    = String

data Literal
  = LitInteger Integer
  deriving (Show, Read, Typeable, Data)

data Exp
  = ExpLit Literal
  | ExpVar Var
  | ExpDeref RefVar -- Note: these are statements in Ivory.  We constrain the
                 -- language here: you can only deref a RefVar.
  | ExpOp ExpOp [Exp]
  | IvoryAnti String
    -- ^ Ivory antiquotation
  deriving (Show, Read, Typeable, Data)

data ExpOp
  = AddOp
  deriving (Show, Read, Typeable, Data)

data Init = Init String
  deriving (Show, Read, Typeable, Data)

-- | AST for parsing C-like statements.
data Stmt
  = IfTE Exp [Stmt] [Stmt]
    -- ^ if (exp) { stmts } else { stmts }
--  | Assert
--  | CompilerAssert
--  | Assume
  | Return Exp
    -- ^ return exp;
  | ReturnVoid
    -- ^ return;
--  | Deref
  | Store RefVar Exp
    -- ^ * var = exp;
  | Assign Var Exp
    -- ^ var = exp;
--  | Call
--  | Local
--  | RefCopy
  | AllocRef RefVar Init
    -- ^ * var = init;
--  | Loop
--  | Forever
--  | Break
  deriving (Show, Read, Typeable, Data)

--------------------------------------------------------------------------------

type P s = Parsec String () s

lexeme :: P a -> P a
lexeme p = do x <- p; spaces; return x

symbolChar :: P Char
symbolChar = alphaNum <|> char '_'

symbol :: String -> P String
symbol = lexeme . string

var :: P String
var = lexeme (many1 symbolChar)

skipSym :: String -> P ()
skipSym = void . symbol

-- | Between symbol pairs.
betSym :: String -> String -> P a -> P a
betSym start end = between (symbol start) (symbol end)

-- Parse until the argument, without consuming it.
-- parseUntil :: Char -> P String
-- parseUntil c = do
--   s <- manyTill (noneOf [c]) (lookAhead $ char c); spaces; return s

initializer :: P ()
initializer = skipSym "="

refDecl :: P RefVar
refDecl = do skipSym "*"; var

-- derefExp :: P String -> P Exp
-- derefExp p = do skipSym "*"; exp <- p; return (Deref exp)

parseEnd :: P ()
parseEnd = skipSym ";"

-- | Parse until the end of a statement, consuming, but not returning, the end
-- token.
parseTilEnd :: P String
parseTilEnd = do
  s <- manyTill (noneOf [';']) (try $ char ';'); spaces; return s

-- | Parse antiquoted Ivory expression.
-- antiExp :: P Exp
-- antiExp = undefined

-- | Parse an expression.
-- parseExp :: P String -> P Exp
-- parseExp p = try (derefExp p)
--          <|> (do exp <- try p; return (Exp exp))
--          <?> "<other Ivory expression>"

--------------------------------------------------------------------------------
-- Expression parsers

parseLit :: P Literal
parseLit = do
  str <- lexeme (many1 digit)
  return (LitInteger $ read str)

parseLitExp :: P Exp
parseLitExp = do l <- parseLit; return (ExpLit l)

parseVarExp :: P Exp
parseVarExp = do v <- var; return (ExpVar v)

-- *var
parseDerefExp :: P Exp
parseDerefExp = do skipSym "*"; ref <- var; return (ExpDeref ref)

parseAddExp :: P (Exp -> Exp -> Exp)
parseAddExp = do
  skipSym "+"
  return (\e0 e1 -> ExpOp AddOp [e0, e1])

parseOpExp :: P (Exp -> Exp -> Exp)
parseOpExp =
  try parseAddExp

parseTermExp :: P Exp
parseTermExp =
      try parseLitExp
  <|> try parseVarExp
  <|> try parseDerefExp
--  <|> try parseAnit
  <?> "<other term expression>"

factor :: P Exp
factor =
      parens parseExp
  <|> parseTermExp

parseExp :: P Exp
parseExp =
      factor `chainl1` parseOpExp
  <?> "<other expression>"

--------------------------------------------------------------------------------

parseInit :: P String -> P Init
parseInit p = do init <- p; return (Init init)

parens :: P a -> P a
parens p = betSym "(" ")" p

-- | Parse a block of statements in curly braces.
parseBlock :: P [Stmt]
parseBlock = betSym "{" "}" programP

stmtsP :: P Stmt
stmtsP = try ifteP
     <|> try assignP
     <|> try returnP
     <|> try refP
     <|> try storeP
     <?> "<other statement parser>"

programP :: P [Stmt]
programP = many stmtsP

--------------------------------------------------------------------------------
-- Statement parsers

-- | if-then-else parser.  Then and else blocks must appear within curly braces.
ifteP :: P Stmt
ifteP = do
  spaces
  skipSym "if"
  cond  <- parseExp
  blk0  <- parseBlock
  skipSym "else"
  blk1  <- parseBlock
  return (IfTE cond blk0 blk1)

-- $ case cond of
    -- Exp exp ->
    -- Deref _ -> unexpected "<dereference in if-then-else branch condition>"

-- Assignment parser.
assignP :: P Stmt
assignP = do
  spaces
  v <- var
  initializer
  exp <- parseExp
  parseEnd
  return (Assign v exp)

-- | Stack-allocation parser.
refP :: P Stmt
refP = do
  spaces
  p <- refDecl
  initializer
  init <- parseInit parseTilEnd
  return (AllocRef p init)

-- | Parse a return statement.
returnP :: P Stmt
returnP = do
  spaces
  skipSym "return"
  mexp <- optionMaybe parseExp
  parseEnd
  return $ case mexp of
             Nothing -> ReturnVoid
             Just e  -> Return e
  -- exp <- parseExp
  -- return $ case exp of
  --   Exp ""  -> ReturnVoid
  --   e       -> Return e

-- | Parse assignment to a reference.
storeP :: P Stmt
storeP = do
  spaces
  p <- refDecl
  skipSym "="
  exp <- parseExp
  return (Store p exp)

{-
test :: String -> IO Stmt
test = mParse ifteP ("",0,0)

a = " a :=  3 ; b = 4;   return (a + b); "
d = " a :=  3 ; "
b = " if(a;) {b;} {c;} "
c = "if (abas) {a := 3;} {asadf := 4;}"
-}
