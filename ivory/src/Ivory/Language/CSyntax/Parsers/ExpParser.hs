--
-- Expression parsers.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.Parsers.ExpParser where

import           Text.Parsec

import           Control.Applicative hiding ((<|>), many)

import           Ivory.Language.CSyntax.Parsers.Common
import           Ivory.Language.CSyntax.ParseAST
import qualified Ivory.Language.CSyntax.TokenParser as T

--------------------------------------------------------------------------------

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
        -- XXX ?? These should be binary operators.
        <|> un "<<"     BitShiftLOp
        <|> un ">>"     BitShiftROp
        <|> un "const"  ConstRefOp
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
   <?> noParse "expression parser"

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
       <?> noParse "term expression parser"

