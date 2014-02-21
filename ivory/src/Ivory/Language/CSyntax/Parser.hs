{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String (Parser)

import Language.Haskell.TH hiding (Stmt, Exp, litP)

import Control.Applicative hiding ((<|>), many)
import Control.Monad

import Ivory.Language.CSyntax.ParseAST

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


--------------------------------------------------------------------------------

type P s = Parsec String () s

lexeme :: P a -> P a
lexeme p = p <* spaces

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
  withWS sym = spaces *> (symbol sym)

assign :: P ()
assign = skipSym "="

refDecl :: P RefVar
refDecl = skipSym "*" *> var

endP :: P ()
endP = skipSym ";"

--------------------------------------------------------------------------------
-- Expression parsers

litP :: P Literal
litP = (LitInteger . read) <$> lexeme (many1 digit)

litExpP :: P Exp
litExpP = ExpLit <$> litP

varExpP :: P Exp
varExpP = ExpVar <$> var

-- *var
derefExpP :: P Exp
derefExpP = skipSym "*"
         *> (ExpDeref <$> var)

addExpP :: P (Exp -> Exp -> Exp)
addExpP = skipSym "+"
       *> pure (\e0 e1 -> ExpOp AddOp [e0, e1])

opExpP :: P (Exp -> Exp -> Exp)
opExpP = try addExpP

arrIndexP :: P Exp
arrIndexP = liftA2 ExpArrIx var (betSym "[" "]" expP)

-- Parse antiquotation (Ivory) expression.
antiExpP :: P Exp
antiExpP = skipSym ":i"
       *> (ExpAnti <$> var)

termExpP :: P Exp
termExpP =
      try litExpP
  <|> try arrIndexP
  <|> try derefExpP
  <|> try antiExpP
  <|> try varExpP -- Try plain variable last
  <?> "<other term expression>"

factorP :: P Exp
factorP = parens expP
      <|> termExpP

expP :: P Exp
expP = factorP `chainl1` opExpP
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
commentP = spaces *> skipSym "--" *> rst *> spaces
  where
  rst = do
    c <- anyChar
    if c == '\n'
      then return ()
      else rst


programP :: P [Stmt]
programP = catMaybes <$> many programP'
  where
  programP' :: P (Maybe Stmt)
  programP' = try fromComment <|> fromStmt

  fromComment = commentP *> pure Nothing
  fromStmt    = Just <$> stmtsP

--------------------------------------------------------------------------------
-- Memory allocation

----------------------------------------
-- Array allocation

-- | Parse var[]; return var.
arrLValue :: P String
arrLValue = var <* skipSym "[]"

-- | Parse { e0, e1, ... en}
-- where ei is an expression.
arrInitP :: P [Exp]
arrInitP =
  let initsP = expP `sepBy1` (skipSym "," *> spaces) in
  betSym "{" "}" initsP

-- | Parse arr[] = { e0, e1, ... en}
arrAllocP :: P AllocRef
arrAllocP = parseAssign arrLValue arrInitP AllocArr

----------------------------------------
-- Ref allocation

-- | Parse *ref = e
allocRefP :: P AllocRef
allocRefP = parseAssign refDecl expP AllocBase

--------------------------------------------------------------------------------
-- Statement parsers

-- | Parse and assignment
parseAssign :: P a -> P b -> (a -> b -> c) -> P c
parseAssign lval rval constr =
     skipSym "let"
  *> liftA2 constr (lval <* assign) rval

stmtsP :: P Stmt
stmtsP = try ifteP
     <|> go assignP
     <|> go returnP
     <|> go allocP
     <|> go storeP
     <?> "<other statement parser>"
  where
  go = try . stmtP

-- | Parse a statement or comment.
stmtP :: P Stmt -> P Stmt
stmtP p = spaces
       *> p
       <* endP
       <* (try commentP <|> spaces)

-- | if-then-else parser.  Then and else blocks must appear within curly braces.
ifteP :: P Stmt
ifteP = spaces
     *> skipSym "if"
     *> liftA3 IfTE expP blockP (skipSym "else" *> blockP)

-- Assignment parser.
assignP :: P Stmt
assignP = parseAssign var expP Assign

-- | Stack-allocation parser.
allocP :: P Stmt
allocP = AllocRef <$> (try allocRefP <|> arrAllocP)

-- | Parse a return statement.
returnP :: P Stmt
returnP = skipSym "return"
       *> (    \case
                 Nothing -> ReturnVoid
                 Just e  -> Return e
           <$> optionMaybe expP
          )

-- | Parse assignment to a reference.
storeP :: P Stmt
storeP = liftA2 Store refDecl (skipSym "=" *> expP)

{-
test :: String -> IO Stmt
test = mParse ifteP ("",0,0)

a = " a :=  3 ; b = 4;   return (a + b); "
d = " a :=  3 ; "
b = " if(a;) {b;} {c;} "
c = "if (abas) {a := 3;} {asadf := 4;}"
-}
