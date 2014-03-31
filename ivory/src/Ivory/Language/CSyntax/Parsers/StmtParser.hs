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
  *> liftA2 constr (lval <* assignSym) rval

-- | if-then-else parser.  Then and else blocks must appear within curly braces.
ifteP :: P Stmt
ifteP = T.symbol "if"
     *> liftA3 IfTE expP blockP (T.symbol "else" *> blockP)

-- | Assertion parser.
assertP :: P Stmt
assertP = T.symbol "assert" *> (Assert <$> expP)

-- | Assumption parser.
assumeP :: P Stmt
assumeP = T.symbol "assert" *> (Assume <$> expP)

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

-- | Simple assignment parser: let var = exp
assignP :: P Stmt
assignP = parseAssign "let" T.identifier expP Assign

-- | Function calls.  Either
-- > v = foo(e0, e1, ..., en)
-- or
-- > foo(e0, e1, ..., en)
-- if the function has a void return type.
callP :: P Stmt
callP = try (parseAssign "" T.identifier rvalP constr)
    <|> Call Nothing <$> T.identifier <*> argsP
  where
  argsP              = T.parens (T.commaSep expP)
  rvalP              = (,) <$> T.identifier <*> argsP
  constr v (f, args) = Call (Just v) f args

-- | Stack-allocation parser.
allocP :: P Stmt
allocP = AllocRef <$> (try allocRefP <|> arrAllocP)

-- | Reference copy parser.
refCopyP :: P Stmt
refCopyP = T.symbol "memcpy" *> (RefCopy <$> expP <*> expP)

-- | Loop parser.
loopP :: P Stmt
loopP = T.symbol "map" *> liftA2 Loop T.identifier blockP

-- | Forever loop parser.
foreverP :: P Stmt
foreverP = T.symbol "forever" *> (Forever <$> blockP)

-- | Parse a statement or comment.
stmtP :: P Stmt -> P Stmt
stmtP p = p
       <* endP

stmtsP :: P Stmt
stmtsP = try ifteP
     <|> go assertP
     <|> go assumeP
     <|> go assignP
     <|> go returnP
     <|> go allocP
     <|> go refCopyP
     <|> go storeP
     <|> go callP
     <|> try loopP
     <|> try foreverP
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
arrLValue = T.identifier <* T.brackets T.whiteSpace

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
assignSym :: P ()
assignSym = void (T.symbol "=")

-- | Parse a pointer: *var
ref :: P RefVar
ref = T.symbol "*" *> T.identifier

-- | Parse statement end.
endP :: P ()
endP = T.semi *> pure ()

--------------------------------------------------------------------------------

