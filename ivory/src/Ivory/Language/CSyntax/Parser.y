-- # -*- mode: haskell -*-
{
--
-- Ivory lexer.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.Parser where

import Ivory.Language.CSyntax.ParseAST
import Ivory.Language.CSyntax.Lexer

}

%name      ivoryParser
%tokentype { Token }
%monad     { Alex }
%lexer     { lexwrap } { TokEOF }
%error     { parseError }

%token
  integer   { TokInteger $$ }
  ident     { TokIdent $$ }

  -- Statements
  'if'       { TokReserved "if" }
  'else'     { TokReserved "else" }
  'assert'   { TokReserved "assert" }
  'assume'   { TokReserved "assume" }
  'assign'   { TokReserved "let" }
  'return'   { TokReserved "return" }
  'alloc'    { TokReserved "alloc" }
  'refCopy'  { TokReserved "memcpy" }
  'loop'     { TokReserved "map" }
  'forever'  { TokReserved "forever" }

 -- Expressions
  'abs'     { TokReserved "abs" }
  'signum'  { TokReserved "signum" }
  'exp'     { TokReserved "exp" }
  'sqrt'    { TokReserved "sqrt" }
  'log'     { TokReserved "log" }
  'pow'     { TokReserved "pow" }

  'sin'     { TokReserved "sin" }
  'cos'     { TokReserved "cos" }
  'tan'     { TokReserved "tan" }

  'asin'    { TokReserved "asin" }
  'acos'    { TokReserved "acos" }
  'atan'    { TokReserved "atan" }

  'sinh'    { TokReserved "sinh" }
  'cosh'    { TokReserved "cosh" }
  'tanh'    { TokReserved "tanh" }

  'asinh'   { TokReserved "asinh" }
  'acosh'   { TokReserved "acosh" }
  'atanh'   { TokReserved "atanh" }

  'isnan'   { TokReserved "isnan" }
  'isinf'   { TokReserved "isinf" }
  'round'   { TokReserved "round" }
  'ceil'    { TokReserved "ceil" }
  'floor'   { TokReserved "floor" }
  'const'   { TokReserved "const" }

  '?'       { TokSym "?" }
  ':'       { TokSym ":" }

  '=='      { TokSym "==" }
  '!='      { TokSym "!=" }

  -- Used for deref and mult
  '*'       { TokSym "*" }

  '/'       { TokSym "/" }

  '+'       { TokSym "+" }
  '-'       { TokSym "-" }
  '%'       { TokSym "%" }

  '='       { TokSym "=" }

  '<'       { TokSym "<" }
  '<='      { TokSym "<=" }
  '>='      { TokSym ">=" }
  '>'       { TokSym ">" }

  '|'       { TokSym "|" }
  '&'       { TokSym "&" }
  '^'       { TokSym "^" }
  '~'       { TokSym "~" }

  '!'       { TokSym "!" }
  '&&'      { TokSym "&&" }
  '||'      { TokSym "||" }

  '('       { TokBrack "(" }
  ')'       { TokBrack ")" }
  '{'       { TokBrack "{" }
  '}'       { TokBrack "}" }
  '['       { TokBrack "[" }
  ']'       { TokBrack "]" }

  ';'       { TokSep ";" }
  ','       { TokSep "," }

-- Follow C's operator precedences
-- XXX double-check precedence of ==, !=, ~, etc.

%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%right
  '*'
  '!'
  '-'
  '~'
  'abs'
  'signum'
  'exp'
  'sqrt'
  'log'
  'pow'
  'sin'
  'cos'
  'tan'
  'asin'
  'acos'
  'atan'
  'sinh'
  'cosh'
  'tanh'
  'asinh'
  'acosh'
  'atanh'
  'isnan'
  'isinf'
  'round'
  'ceil'
  'floor'
  'const'

%%

----------------------------------------
-- Statements
stmt :
       'if' exp '{' stmts '}'
         'else' '{' stmts '}'         { IfTE $2 (reverse $4) (reverse $8) }
     | 'assert' exp                   { Assert $2 }
     | 'assume' exp                   { Assume $2 }
     | 'assign' ident '=' exp         { Assign $2 $4 }
     | 'return'                       { ReturnVoid }
     | 'return' exp                   { Return $2 }
     | 'alloc' '*' ident '=' exp      { AllocRef (AllocBase $3 $5) }
     | 'alloc' ident '[' ']' '='
         '{' exps '}'                 { AllocRef (AllocArr $2 (reverse $7)) }
     | 'refCopy' ident ident          { RefCopy $2 $3 }
     | '*' ident '=' exp              { Store (RefVar $2) $4 }
     | ident '[' exp ']' '=' exp      { Store (ArrIx $1 $3) $6 }
     | ident '(' exps ')'             { Call Nothing $1 $3 }
     | ident '=' ident '(' exps ')'   { Call (Just $1) $3 $5 }
     | 'loop' ident '{' stmts '}'     { Loop $2 $4 }
     | 'forever' '{' stmts '}'        { Forever $3 }

-- Zero or more statements, separated by arbitrary many ';'s.
stmts : stmts ';' stmt          { $3 : $1 }
      | stmts ';'               { $1 }
      | stmt                    { [$1] }
      | {- empty -}             { [] }

----------------------------------------
-- Initializers

-- Zero or more expressions, separated by arbitrary many ','s.
exps :  exps ',' exp           { $3 : $1 }
      | exps ','               { $1 }
      | exp                    { [$1] }
      | {- empty -}            { [] }

----------------------------------------
-- Expressions
exp : integer            { ExpLit (LitInteger $1) }
    | ident              { ExpVar $1 }
    | '(' exp ')'        { $2 }
    | ident '[' exp ']'  { ExpArrIx $1 $3 }
    -- XXX antiExpP

    -- Unary operators
    | '*' ident          { ExpDeref $2 }
    | '!'            exp { ExpOp NotOp [$2] }
    | '-'            exp { ExpOp NegateOp [$2] }
    | '~'            exp { ExpOp BitComplementOp [$2] }
    | 'abs'          exp { ExpOp AbsOp [$2] }
    | 'signum'       exp { ExpOp SignumOp [$2] }
    | 'exp'          exp { ExpOp FExpOp [$2] }
    | 'sqrt'         exp { ExpOp FSqrtOp [$2] }
    | 'log'          exp { ExpOp FLogOp [$2] }
    | 'pow'          exp { ExpOp FPowOp [$2] }
    | 'sin'          exp { ExpOp FSinOp [$2] }
    | 'cos'          exp { ExpOp FCosOp [$2] }
    | 'tan'          exp { ExpOp FTanOp [$2] }
    | 'asin'         exp { ExpOp FAsinOp [$2] }
    | 'acos'         exp { ExpOp FAcosOp [$2] }
    | 'atan'         exp { ExpOp FAtanOp [$2] }
    | 'sinh'         exp { ExpOp FSinhOp [$2] }
    | 'cosh'         exp { ExpOp FCoshOp [$2] }
    | 'tanh'         exp { ExpOp FTanhOp [$2] }
    | 'asinh'        exp { ExpOp FAsinhOp [$2] }
    | 'acosh'        exp { ExpOp FAcoshOp [$2] }
    | 'atanh'        exp { ExpOp FAtanhOp [$2] }
    | 'isnan'        exp { ExpOp IsNanOp [$2] }
    | 'isinf'        exp { ExpOp IsInfOp [$2] }
    | 'round'        exp { ExpOp RoundFOp [$2] }
    | 'ceil'         exp { ExpOp CeilFOp [$2] }
    | 'floor'        exp { ExpOp FloorFOp [$2] }
    | 'const'        exp { ExpOp ConstRefOp [$2] }

    -- Binary operators
    | exp '||'  exp      { ExpOp OrOp [$1, $3] }
    | exp '&&'  exp      { ExpOp AndOp [$1, $3] }
    | exp '|'   exp      { ExpOp BitOrOp [$1, $3] }
    | exp '^'   exp      { ExpOp BitXorOp [$1, $3] }
    | exp '&'   exp      { ExpOp BitAndOp [$1, $3] }

    | exp '=='  exp      { ExpOp EqOp [$1, $3] }
    | exp '!='  exp      { ExpOp NeqOp [$1, $3] }

    | exp '<'   exp      { ExpOp (LtOp False) [$1, $3] }
    | exp '<='  exp      { ExpOp (LtOp True) [$1, $3] }
    | exp '>'   exp      { ExpOp (GtOp False) [$1, $3] }
    | exp '>='  exp      { ExpOp (GtOp True) [$1, $3] }

    | exp '+'   exp      { ExpOp AddOp [$1, $3] }
    | exp '-'   exp      { ExpOp AddOp [$1, $3] }

    | exp '*'   exp      { ExpOp MulOp [$1, $3] }
    | exp '/'   exp      { ExpOp DivOp [$1, $3] }
    | exp '%'   exp      { ExpOp ModOp [$1, $3] }

    -- Tertiary operators
    | exp '?' exp ':' exp { ExpOp CondOp [$1, $3, $5] }



--------------------------------------------------------------------------------

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap cont = cont =<< alexMonadScan'

-- We rewrite alexMonadScan' to return the position when lexing fails (the
-- default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (pos, _, _, _) -> alexError (show pos)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

getPosn :: Alex (Int,Int)
getPosn = do
  (AlexPn _ l c,_,_,_) <- alexGetInput
  return (l,c)

parseError :: Token -> Alex a
parseError t = do
  (l,c) <- getPosn
  fail (show l ++ ":" ++ show c ++ ": Parse error on Token: " ++ show t ++ "\n")

-- XXX testing

parseTest :: String -> Either String Stmt
parseTest s = runAlex s ivoryParser

parseFileTest :: FilePath -> IO (Either String Stmt)
parseFileTest fp = do
  cs <- readFile fp
  return (parseTest cs)

}
