{-# LANGUAGE LambdaCase #-}

--
-- Statement parsers.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.Parsers.StmtParser where

import Text.Parsec

import Control.Monad
import Control.Applicative hiding ((<|>), many)

import Ivory.Language.CSyntax.Parsers.Common
import Ivory.Language.CSyntax.Parsers.ExpParser
import Ivory.Language.CSyntax.ParseAST
import qualified Ivory.Language.CSyntax.TokenParser as T

--------------------------------------------------------------------------------
-- Statement parsers

-- | Generic assignment parser
parseAssign :: String -> P a -> P b -> (a -> b -> c) -> P c
parseAssign key lval rval constr =
     T.symbol key
  *> liftA2 constr (lval <* assign) rval

-- | if-then-else parser.  Then and else blocks must appear within curly braces.
ifteP :: P Stmt
ifteP = T.whiteSpace
     *> T.symbol "if"
     *> liftA3 IfTE expP blockP (T.symbol "else" *> blockP)

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
storeP = liftA2 Store refLVar (T.symbol "=" *> expP)

-- | Simple ssignment parser: var = exp
assignP :: P Stmt
assignP = parseAssign "let" T.identifier expP Assign

-- | Stack-allocation parser.
allocP :: P Stmt
allocP = AllocRef <$> (try allocRefP <|> arrAllocP)

-- | Loop parser.
loopP :: P Stmt
loopP = T.symbol "map"
     *> liftA2 Loop T.identifier blockP

-- | Parse a statement or comment.
stmtP :: P Stmt -> P Stmt
stmtP p = T.whiteSpace
       *> p
       <* endP
       <* T.whiteSpace

stmtsP :: P Stmt
stmtsP = try ifteP
     <|> go assignP
     <|> go returnP
     <|> go allocP
     <|> go storeP
     <|> try loopP
     <?> noParse "statement parser"
  where
  go = try . stmtP

-- | Parse a block of statements in curly braces.
blockP :: P [Stmt]
blockP = T.braces programP

programP :: P [Stmt]
programP = many stmtsP

--------------------------------------------------------------------------------
-- Tokens

-- | Parse { e0, e1, ... en }
-- where ei is an expression.
arrInitP :: P [Exp]
arrInitP = T.braces (T.commaSep expP)

-- | Parse alloc: var lval = init
parseAlloc :: P a -> P b -> (a -> b -> c) -> P c
parseAlloc = parseAssign "alloc"

-- | Parse var[]; return var.
arrLValue :: P String
arrLValue = T.identifier <* T.whiteSpace <* T.brackets T.whiteSpace

-- | Parse alloc arr[] = { e0, e1, ... en }
arrAllocP :: P AllocRef
arrAllocP = parseAlloc arrLValue arrInitP AllocArr

-- | Parse alloc *ref = e
allocRefP :: P AllocRef
allocRefP = parseAlloc ref expP AllocBase

-- | Parse an array index: arr[exp]
arrIx :: P RefLVal
arrIx = liftA2 ArrIx T.identifier (T.brackets expP)

-- | Parse either an reference or array index.
refLVar :: P RefLVal
refLVar = try ref' <|> try arrIx <?> noParse "ref lvar parser"
  where
  ref' = RefVar <$> ref

-- | Parse assignment.
assign :: P ()
assign = void (T.symbol "=")

-- | Parse a pointer: *var
ref :: P RefVar
ref = T.symbol "*" *> T.identifier

-- | Parse statement end.
endP :: P ()
endP = T.semi *> pure ()

--------------------------------------------------------------------------------
