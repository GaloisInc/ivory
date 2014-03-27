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

-- | Parse assignment.
assign :: P ()
assign = void (T.symbol "=")

-- | Parse a pointer: *var
ref :: P RefVar
ref = T.symbol "*" *> T.identifier

-- | Parse an array index: arr[exp]
arrIx :: P RefLVal
arrIx = liftA2 ArrIx T.identifier (T.brackets expP)

-- | Parse either an reference or array index.
refLVar :: P RefLVal
refLVar = try ref' <|> try arrIx <?> noParse "ref lvar parser"
  where
  ref' = RefVar <$> ref

-- | Parse statement end.
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

-- C's operator precedences: http://www.swansontec.com/sopc.html
-- 0: highest precedence.

binExpP0 :: P (Exp -> Exp -> Exp)
binExpP0 = binOpP "*"  MulOp
       <|> binOpP "/"  DivOp
       <|> binOpP "%"  ModOp

binExpP1 :: P (Exp -> Exp -> Exp)
binExpP1 = binOpP "+"  AddOp
       <|> binOpP "-"  SubOp

binExpP3 :: P (Exp -> Exp -> Exp)
-- Put <=, >= parsers before their counterparts.
binExpP3 = binOpP ">=" (GtOp True)
       <|> binOpP ">"  (GtOp False)
       <|> binOpP "<=" (LtOp True)
       <|> binOpP "<"  (LtOp False)

binExpP4 :: P (Exp -> Exp -> Exp)
binExpP4 = binOpP "==" EqOp
       <|> binOpP "!=" NeqOp

-- | Expressions without 'condExpP'.  Chaining must be in this order to get
-- operator presedences right.
expP' :: P Exp
expP' =     factorP
  `chainl1` binExpP0
  `chainl1` binExpP1
  `chainl1` binExpP3
  `chainl1` binExpP4
  `chainl1` (binOpPDisambig "&" "&&" BitAndOp)
  `chainl1` (binOpP "^" BitXorOp)
  `chainl1` (binOpPDisambig "|" "||" BitOrOp)
  `chainl1` (binOpP "&&" AndOp)
  `chainl1` (binOpP "||" OrOp)
  <?> "expression' parser"

-- | Parse a parenthesized expression or a simple term.  This should be the
-- argument to a operator that takes an expression.
factorP :: P Exp
factorP = T.parens expP' <|> termExpP

binOpP :: String -> ExpOp -> P (Exp -> Exp -> Exp)
binOpP op = binOpP' (T.symbol op)

-- Look ahead to disambiguate against a partially-matching operator.
binOpPDisambig :: String -> String -> ExpOp -> P (Exp -> Exp -> Exp)
binOpPDisambig op0 op1 opAST = binOpP' getSym opAST
  where
  getSym = try (lookAhead $ T.symbol op1)
       <|> T.symbol op0

binOpP' :: P String -> ExpOp -> P (Exp -> Exp -> Exp)
binOpP' opP opAST =
  opP *> pure (\e0 e1 -> ExpOp opAST [e0, e1])

-- | Unary operators.
unaryExpP :: P Exp
unaryExpP = un "!"      NotOp
        <|> un "-"      NegateOp
        <|> un "abs"    AbsOp
        <|> un "signum" SignumOp
        <|> un "exp"    FExpOp
        <|> un "sqrt"   FSqrtOp
        <|> un "log"    FLogOp
        <|> un "pow"    FPowOp
        <|> un "sin"    FSinOp
        <|> un "cos"    FCosOp
        <|> un "tan"    FTanOp
        <|> un "asin"   FAsinOp
        <|> un "acos"   FAcosOp
        <|> un "atan"   FAtanOp
        <|> un "sinh"   FSinhOp
        <|> un "cosh"   FCoshOp
        <|> un "tanh"   FTanhOp
        <|> un "asinh"  FAsinhOp
        <|> un "acosh"  FAcoshOp
        <|> un "atanh"  FAtanhOp
        <|> un "isnan"  IsNanOp
        <|> un "isinf"  IsInfOp
        <|> un "round"  RoundFOp
        <|> un "ceil"   CeilFOp
        <|> un "floor"  FloorFOp
        <|> un "~"      BitComplementOp
        <|> un "<<"     BitShiftLOp
        <|> un ">>"     BitShiftROp
  where
  -- Parse a factorP: parenthesized exp or a simple term.  Otherwise, binary
  -- operators are sucked up.
  un op opAST = try (T.symbol op *> (ExpOp opAST <$> (:[]) <$> factorP))

-- | Parse a conditional expression.
condExpP :: P Exp
condExpP = ExpOp CondOp <$> args
  where
  -- Turn a list of parsers into P [args]
  args = foldr (liftA2 (:)) (pure [])
    [expP', T.symbol "?" *> expP', T.symbol ":" *> expP']

-- | Parse an array index: arr[e0]
arrIndexP :: P Exp
arrIndexP = liftA2 ExpArrIx T.identifier (T.brackets expP)

-- | Top level expression parser.
expP :: P Exp
expP = try (T.parens expP)
   <|> try condExpP
   <|> try expP' -- Should be last
   <?> "expression parser"

-- | Parse antiquotation (Ivory) expression.
antiExpP :: P Exp
antiExpP = T.symbol ":i" *> (ExpAnti <$> T.identifier)

-- | Parse an expression.
termExpP :: P Exp
termExpP = try litExpP
       <|> try arrIndexP
       <|> try derefExpP
       <|> try antiExpP
       <|> try unaryExpP
       <|> try varExpP -- Try plain variable last
       <?> "term expression parser"

--------------------------------------------------------------------------------
-- Memory allocation

-- | Parse { e0, e1, ... en}
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

noParse :: String -> String
noParse str = "<no valid parse: " ++ str ++ ">"

--------------------------------------------------------------------------------

test :: String -> IO Exp
test = mParse expP ("",0,0)

a = " a :=  3 ; b = 4;   return (a + b); "
b = " if(a;) {b;} {c;} "
c = "if (abas) {a := 3;} {asadf := 4;}"
d = " a :=  3 ; "
e = "7 ? 8 : 3+4"
f = "return (4+5)"
g = "return (a ? 3 : 4)"
h = "a & b"
i = "a >= b"
