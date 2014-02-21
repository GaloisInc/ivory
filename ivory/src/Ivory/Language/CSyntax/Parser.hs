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

ivoryCParser :: String -> Q [Stmt]
ivoryCParser = qParse programP

--------------------------------------------------------------------------------

type P s = Parsec String () s

--------------------------------------------------------------------------------
-- Tokens

assign :: P ()
assign = void (T.symbol "=")

refDecl :: P RefVar
refDecl = T.symbol "*" *> T.identifier

endP :: P ()
endP = T.semi *> pure ()

--------------------------------------------------------------------------------
-- Expression parsers

-- | Parse a literal integer.
litP :: P Literal
litP = LitInteger <$> T.integer

-- | Parse a literal integer as an expression.
litExpP :: P Exp
litExpP = ExpLit <$> litP

-- | Parse an Ivory variable.
varExpP :: P Exp
varExpP = ExpVar <$> T.identifier

-- | Parse a dereference expression (*var).
derefExpP :: P Exp
derefExpP = T.symbol "*" *> (ExpDeref <$> T.identifier)

-- | Parse addition.
addExpP :: P (Exp -> Exp -> Exp)
addExpP = T.symbol "+"
       *> pure (\e0 e1 -> ExpOp AddOp [e0, e1])

-- | Parse a binary operator.
opExpP :: P (Exp -> Exp -> Exp)
opExpP = try addExpP

-- | Parse an array index: arr[e0]
arrIndexP :: P Exp
arrIndexP = liftA2 ExpArrIx T.identifier (T.brackets expP)

-- | Parse an expression.
expP :: P Exp
expP = factorP `chainl1` opExpP
   <?> "<other expression>"
  where
  factorP :: P Exp
  factorP = T.parens expP <|> termExpP

-- | Parse antiquotation (Ivory) expression.
antiExpP :: P Exp
antiExpP = T.symbol ":i" *> (ExpAnti <$> T.identifier)

-- | Parse an expression.
termExpP :: P Exp
termExpP = try litExpP
       <|> try arrIndexP
       <|> try derefExpP
       <|> try antiExpP
       <|> try varExpP -- Try plain variable last
       <?> "<other term expression>"

--------------------------------------------------------------------------------
-- Memory allocation

-- | Parse var[]; return var.
arrLValue :: P String
arrLValue = T.identifier <* T.braces T.whiteSpace

-- | Parse { e0, e1, ... en}
-- where ei is an expression.
arrInitP :: P [Exp]
arrInitP = T.braces (T.commaSep expP)

-- | Parse assignment: var lval = init
parseAssign :: P a -> P b -> (a -> b -> c) -> P c
parseAssign lval rval constr =
     T.symbol "alloc"
  *> liftA2 constr (lval <* assign) rval

-- | Parse arr[] = { e0, e1, ... en}
arrAllocP :: P AllocRef
arrAllocP = parseAssign arrLValue arrInitP AllocArr

-- | Parse *ref = e
allocRefP :: P AllocRef
allocRefP = parseAssign refDecl expP AllocBase

--------------------------------------------------------------------------------
-- Statement parsers

-- | Parse a statement or comment.
stmtP :: P Stmt -> P Stmt
stmtP p = T.whiteSpace
       *> p
       <* endP
       <* T.whiteSpace

-- | if-then-else parser.  Then and else blocks must appear within curly braces.
ifteP :: P Stmt
ifteP = T.whiteSpace
     *> T.symbol "if"
     *> liftA3 IfTE expP blockP (T.symbol "else" *> blockP)

-- Assignment parser.
assignP :: P Stmt
assignP = parseAssign T.identifier expP Assign

-- | Stack-allocation parser.
allocP :: P Stmt
allocP = AllocRef <$> (try allocRefP <|> arrAllocP)

-- | Parse a return statement.
returnP :: P Stmt
returnP = T.symbol "return"
       *> (someExp <$> optionMaybe expP)
  where
  someExp = \case
              Nothing -> ReturnVoid
              Just e  -> Return e

-- | Parse assignment to a reference.
storeP :: P Stmt
storeP = liftA2 Store refDecl (T.symbol "=" *> expP)

stmtsP :: P Stmt
stmtsP = try ifteP
     <|> go assignP
     <|> go returnP
     <|> go allocP
     <|> go storeP
     <?> "<other statement parser>"
  where
  go = try . stmtP

--------------------------------------------------------------------------------
-- Program parsers.

-- parseInit :: P String -> P Init
-- parseInit p = do init <- p; return (Ival init)

-- | Parse a block of statements in curly braces.
blockP :: P [Stmt]
blockP = T.braces programP

programP :: P [Stmt]
programP = many stmtsP
--------------------------------------------------------------------------------

{-
test :: String -> IO Stmt
test = mParse ifteP ("",0,0)

a = " a :=  3 ; b = 4;   return (a + b); "
d = " a :=  3 ; "
b = " if(a;) {b;} {c;} "
c = "if (abas) {a := 3;} {asadf := 4;}"
-}
