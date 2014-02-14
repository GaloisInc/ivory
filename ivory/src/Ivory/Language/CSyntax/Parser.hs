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
  , RefVar
  , AllocRef(..)
  ) where

import Prelude hiding (exp, init)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String (Parser)

import Language.Haskell.TH hiding (Stmt, Exp, litP)

import Control.Monad
--import Data.Generics

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
--type Size      = Integer

data Literal
  = LitInteger Integer
  deriving (Eq, Show, Read)

data Exp
  = ExpLit Literal
  | ExpVar Var
  | ExpDeref RefVar -- Note: these are statements in Ivory.  We constrain the
                 -- language here: you can only deref a RefVar.
  | ExpOp ExpOp [Exp]
  | ExpArrIx RefVar Exp
  | ExpAnti String
    -- ^ Ivory antiquotation
  deriving (Eq, Show, Read)

data ExpOp
  = AddOp
  deriving (Eq, Show, Read)

data AllocRef
  = AllocBase RefVar Exp
  | AllocArr  RefVar [Exp]
  deriving (Eq, Show, Read)

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
--  | Deref XXX dereferencing is an expression in our language here.
  | Store RefVar Exp
    -- ^ * var = exp;
  | Assign Var Exp
    -- ^ var = exp;
--  | Call
--  | Local
--  | RefCopy
  | AllocRef AllocRef
    -- ^ * var = init;
    -- ^ arr[] = {0,1,2};
--  | Loop
--  | Forever
--  | Break
  deriving (Eq, Show, Read)

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

-- | Between symbol pairs, ignoring whitespace.
betSym :: String -> String -> P a -> P a
betSym start end = between (withWS start) (withWS end)
  where
  withWS sym = do spaces; (symbol sym)

assign :: P ()
assign = skipSym "="

refDecl :: P RefVar
refDecl = do skipSym "*"; var

endP :: P ()
endP = skipSym ";"

-- | Parse until the end of a statement, consuming, but not returning, the end
-- token.
-- parseTilEnd :: P String
-- parseTilEnd = do
--   s <- manyTill (noneOf [';']) (try $ char ';'); spaces; return s

-- | Parse antiquoted Ivory expression.
-- antiExp :: P Exp
-- antiExp = undefined

-- | Parse an expression.
-- expP :: P String -> P Exp
-- expP p = try (derefExp p)
--          <|> (do exp <- try p; return (Exp exp))
--          <?> "<other Ivory expression>"

--------------------------------------------------------------------------------
-- Expression parsers

litP :: P Literal
litP = do
  str <- lexeme (many1 digit)
  return (LitInteger $ read str)

litExpP :: P Exp
litExpP = do l <- litP; return (ExpLit l)

varExpP :: P Exp
varExpP = do v <- var; return (ExpVar v)

-- *var
derefExpP :: P Exp
derefExpP = do skipSym "*"; ref <- var; return (ExpDeref ref)

addExpP :: P (Exp -> Exp -> Exp)
addExpP = do
  skipSym "+"
  return (\e0 e1 -> ExpOp AddOp [e0, e1])

opExpP :: P (Exp -> Exp -> Exp)
opExpP = try addExpP

arrIndexP :: P Exp
arrIndexP = do
  arr <- var
  exp <- betSym "[" "]" expP
  return (ExpArrIx arr exp)

-- Parse antiquotation (Ivory) expression.
antiExpP :: P Exp
antiExpP = do
  skipSym ":i"
  v <- var
  return (ExpAnti v)

termExpP :: P Exp
termExpP =
      try litExpP
  <|> try arrIndexP
  <|> try derefExpP
  <|> try antiExpP
  <|> try varExpP -- Try plain variable last
  <?> "<other term expression>"

factorP :: P Exp
factorP =
      parens expP
  <|> termExpP

expP :: P Exp
expP =
      factorP `chainl1` opExpP
  <?> "<other expression>"

--------------------------------------------------------------------------------

-- parseInit :: P String -> P Init
-- parseInit p = do init <- p; return (Ival init)

parens :: P a -> P a
parens p = betSym "(" ")" p

-- | Parse a block of statements in curly braces.
blockP :: P [Stmt]
blockP = betSym "{" "}" programP

-- | Comments parser: starts with -- and goes until a newline is reached.
commentP :: P ()
commentP = do spaces; skipSym "--"; rst; spaces
  where
  rst = do
    c <- anyChar
    if c == '\n'
      then return ()
      else rst

programP :: P [Stmt]
programP = do stmts <- many programP'; return (catMaybes stmts)
  where
  programP' :: P (Maybe Stmt)
  programP' = try fromComment <|> fromStmt

  fromComment = do commentP; return Nothing
  fromStmt    = do stmt <- stmtsP; return (Just stmt)

--------------------------------------------------------------------------------
-- Memory allocation

----------------------------------------
-- Array allocation

-- | Parse var[]; return var.
arrLValue :: P String
arrLValue = do
  arr <- var
  skipSym "[]"
  return arr

-- | Parse { e0, e1, ... en}
-- where ei is an expression.
arrInitP :: P [Exp]
arrInitP =
  let initsP = expP `sepBy1` (do skipSym ","; spaces) in
  betSym "{" "}" initsP

arrAllocP :: P AllocRef
arrAllocP = do
  arr <- arrLValue
  assign
  init <- arrInitP
  return (AllocArr arr init)

----------------------------------------
-- Ref allocation

allocRefP :: P AllocRef
allocRefP = do
  r <- refDecl
  assign
  exp <- expP
  return (AllocBase r exp)

--------------------------------------------------------------------------------
-- Statement parsers

stmtsP :: P Stmt
stmtsP = try ifteP
     <|> go assignP
     <|> go returnP
     <|> go allocP
     <|> go storeP
     <?> "<other statement parser>"
  where
  go = try . stmtP

stmtP :: P Stmt -> P Stmt
stmtP p = do
  spaces
  res <- p
  endP
  (try commentP <|> spaces)
  return res

-- | if-then-else parser.  Then and else blocks must appear within curly braces.
ifteP :: P Stmt
ifteP = do
  spaces
  skipSym "if"
  cond  <- expP
  blk0  <- blockP
  skipSym "else"
  blk1  <- blockP
  return (IfTE cond blk0 blk1)

-- Assignment parser.
assignP :: P Stmt
assignP = do
  skipSym "let"
  v <- var
  assign
  exp <- expP
  return (Assign v exp)

-- | Stack-allocation parser.
allocP :: P Stmt
allocP = do
  skipSym "let"
  alloc <- try allocRefP <|> arrAllocP
  return (AllocRef alloc)

-- | Parse a return statement.
returnP :: P Stmt
returnP = do
  skipSym "return"
  mexp <- optionMaybe expP
  return $ case mexp of
             Nothing -> ReturnVoid
             Just e  -> Return e

-- | Parse assignment to a reference.
storeP :: P Stmt
storeP = do
  r <- refDecl
  skipSym "="
  exp <- expP
  return (Store r exp)

{-
test :: String -> IO Stmt
test = mParse ifteP ("",0,0)

a = " a :=  3 ; b = 4;   return (a + b); "
d = " a :=  3 ; "
b = " if(a;) {b;} {c;} "
c = "if (abas) {a := 3;} {asadf := 4;}"
-}
