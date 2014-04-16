-- # -*- mode: haskell -*-
{
--
-- Ivory lexer.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--
-- Parser.hs file is generated!

module Ivory.Language.Syntax.Concrete.Parser where

import Ivory.Language.Syntax.Concrete.ParseAST
import Ivory.Language.Syntax.Concrete.Lexer

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
  if       { TokReserved "if" }
  else     { TokReserved "else" }
  assert   { TokReserved "assert" }
  assume   { TokReserved "assume" }
  assign   { TokReserved "let" }
  return   { TokReserved "return" }
  alloc    { TokReserved "alloc" }
  refCopy  { TokReserved "memcpy" }
  loop     { TokReserved "map" }
  forever  { TokReserved "forever" }

 -- Expressions
  abs     { TokReserved "abs" }
  signum  { TokReserved "signum" }
  expOp   { TokReserved "exp" }
  sqrt    { TokReserved "sqrt" }
  log     { TokReserved "log" }
  pow     { TokReserved "pow" }

  sin     { TokReserved "sin" }
  cos     { TokReserved "cos" }
  tan     { TokReserved "tan" }

  asin    { TokReserved "asin" }
  acos    { TokReserved "acos" }
  atan    { TokReserved "atan" }

  sinh    { TokReserved "sinh" }
  cosh    { TokReserved "cosh" }
  tanh    { TokReserved "tanh" }

  asinh   { TokReserved "asinh" }
  acosh   { TokReserved "acosh" }
  atanh   { TokReserved "atanh" }

  isnan   { TokReserved "isnan" }
  isinf   { TokReserved "isinf" }
  round   { TokReserved "round" }
  ceil    { TokReserved "ceil" }
  floor   { TokReserved "floor" }
  const   { TokReserved "const" }

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
  '<<'      { TokSym "<<" }
  '>>'      { TokSym ">>" }

  '('       { TokBrack "(" }
  ')'       { TokBrack ")" }
  '{'       { TokBrack "{" }
  '}'       { TokBrack "}" }
  '['       { TokBrack "[" }
  ']'       { TokBrack "]" }

  ';'       { TokSep ";" }
  ','       { TokSep "," }

  -- Types
  struct   { TokReserved "struct" }

  bool     { TokReserved "bool" }
  char     { TokReserved "char" }
  float    { TokReserved "float" }
  double   { TokReserved "double" }
  void     { TokReserved "void" }

  int8_t   { TokReserved "int8_t" }
  int16_t  { TokReserved "int16_t" }
  int32_t  { TokReserved "int32_t" }
  int64_t  { TokReserved "int64_t" }

  uint8_t  { TokReserved "uint8_t" }
  uint16_t { TokReserved "uint16_t" }
  uint32_t { TokReserved "uint32_t" }
  uint64_t { TokReserved "uint64_t" }

  S        { TokReserved "S" }
  G        { TokReserved "G" }

  IBool     { TokReserved "IBool" }
  IChar     { TokReserved "IChar" }
  IFloat    { TokReserved "IFloat" }
  IDouble   { TokReserved "IDouble" }

  Sint8     { TokReserved "Sint8" }
  Sint16    { TokReserved "Sint16" }
  Sint32    { TokReserved "Sint32" }
  Sint64    { TokReserved "Sint64" }

  Uint8    { TokReserved "Uint8" }
  Uint16   { TokReserved "Uint16" }
  Uint32   { TokReserved "Uint32" }
  Uint64   { TokReserved "Uint64" }

  Ref      { TokReserved "Ref" }
  ConstRef { TokReserved "ConstRef" }
  Array    { TokReserved "Array" }

  Stored   { TokReserved "Stored" }

  Stack    { TokReserved "Stack" }
  Global   { TokReserved "Global" }

--------------------------------------------------------------------------------
-- Precedence

%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%right '*' '~' '!' '-'
%right
  abs
  signum
  expOp
  sqrt
  log
  pow
  sin
  cos
  tan
  asin
  acos
  atan
  sinh
  cosh
  tanh
  asinh
  acosh
  atanh
  isnan
  isinf
  round
  ceil
  floor
  const

%%

----------------------------------------
-- Procs

procs :: { [ProcDef] }
procs : procs proc         { $2 : $1 }
      | {- empty -}            { [] }

proc :: { ProcDef }
proc : type ident '(' args ')'
         '{' stmts '}'         { ProcDef $1 $2 (reverse $4) (reverse $7) }

tyArg :: { (Type, Var) }
tyArg : type ident { ($1, $2) }

-- Zero or more typed arguments, separated by arbitrary many ','s.
args :: { [(Type, Var)] }
args :  args ',' tyArg         { $3 : $1 }
      | args ','               { $1 }
      | tyArg                  { [$1] }
      | {- empty -}            { [] }

----------------------------------------
-- Statements

simpleStmt :: { Stmt }
simpleStmt :
    assert exp                    { Assert $2 }
  | assume exp                    { Assume $2 }
  | assign ident '=' exp          { Assign $2 $4 }
  | return                        { ReturnVoid }
  | return exp                    { Return $2 }
  | alloc '*' ident '=' exp       { AllocRef (AllocBase $3 $5) }
  | alloc ident '[' ']' '='
      '{' exps '}'                { AllocRef (AllocArr $2 (reverse $7)) }
  | refCopy ident ident           { RefCopy (ExpVar $2) (ExpVar $3) }
  | '*' ident '=' exp             { Store (RefVar $2) $4 }
  | ident '[' exp ']' '=' exp     { Store (ArrIx $1 $3) $6 }
  | ident '(' exps ')'            { Call Nothing $1 (reverse $3) }
  | ident '=' ident '(' exps ')'  { Call (Just $1) $3 (reverse $5) }

blkStmt :: { Stmt }
blkStmt :
    loop ident '{' stmts '}'         { Loop $2 (reverse $4) }
  | forever '{' stmts '}'            { Forever (reverse $3) }
  | if exp '{' stmts '}'
      else '{' stmts '}'             { IfTE $2 (reverse $4) (reverse $8) }

-- 1 or more statements.
stmts :: { [Stmt] }
stmts : stmts simpleStmt ';'   { $2 : $1 }
      | stmts blkStmt          { $2 : $1 }
      | simpleStmt ';'         { [$1] }
      | blkStmt                { [$1] }

----------------------------------------
-- Initializers

-- Zero or more expressions, separated by arbitrary many ','s.
exps :: { [Exp] }
exps :  exps ',' exp           { $3 : $1 }
      | exps ','               { $1 }
      | exp                    { [$1] }
      | {- empty -}            { [] }

----------------------------------------
-- Expressions
exp :: { Exp }
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
    | abs          exp { ExpOp AbsOp [$2] }
    | signum       exp { ExpOp SignumOp [$2] }
    | expOp        exp { ExpOp FExpOp [$2] }
    | sqrt         exp { ExpOp FSqrtOp [$2] }
    | log          exp { ExpOp FLogOp [$2] }
    | pow          exp { ExpOp FPowOp [$2] }
    | sin          exp { ExpOp FSinOp [$2] }
    | cos          exp { ExpOp FCosOp [$2] }
    | tan          exp { ExpOp FTanOp [$2] }
    | asin         exp { ExpOp FAsinOp [$2] }
    | acos         exp { ExpOp FAcosOp [$2] }
    | atan         exp { ExpOp FAtanOp [$2] }
    | sinh         exp { ExpOp FSinhOp [$2] }
    | cosh         exp { ExpOp FCoshOp [$2] }
    | tanh         exp { ExpOp FTanhOp [$2] }
    | asinh        exp { ExpOp FAsinhOp [$2] }
    | acosh        exp { ExpOp FAcoshOp [$2] }
    | atanh        exp { ExpOp FAtanhOp [$2] }
    | isnan        exp { ExpOp IsNanOp [$2] }
    | isinf        exp { ExpOp IsInfOp [$2] }
    | round        exp { ExpOp RoundFOp [$2] }
    | ceil         exp { ExpOp CeilFOp [$2] }
    | floor        exp { ExpOp FloorFOp [$2] }
    | const        exp { ExpOp ConstRefOp [$2] }

    -- Binary operators
    | exp '||'  exp      { ExpOp OrOp [$1, $3] }
    | exp '&&'  exp      { ExpOp AndOp [$1, $3] }
    | exp '|'   exp      { ExpOp BitOrOp [$1, $3] }
    | exp '^'   exp      { ExpOp BitXorOp [$1, $3] }
    | exp '&'   exp      { ExpOp BitAndOp [$1, $3] }
    | exp '<<'  exp      { ExpOp BitShiftLOp [$1, $3] }
    | exp '>>'  exp      { ExpOp BitShiftROp [$1, $3] }

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

----------------------------------------
-- Types

type :: { Type }
type :
    cType     { $1 }
  | hsType    { $1 }

-- C-style types

cType :: { Type }
cType :
    baseTypeC     { $1 }
  | refTypeC      { $1 }

baseTypeC :: { Type }
baseTypeC :
    bool     { TyBool }
  | char     { TyChar }
  | float    { TyFloat }
  | double   { TyDouble }
  | void     { TyVoid }

  | int8_t   { TyInt Int8 }
  | int16_t  { TyInt Int16 }
  | int32_t  { TyInt Int32 }
  | int64_t  { TyInt Int64 }

  | uint8_t  { TyWord Word8 }
  | uint16_t { TyWord Word16 }
  | uint32_t { TyWord Word32 }
  | uint64_t { TyWord Word64 }

  | baseTypeC '[' integer ']' { TyArr $1 $3 }

  | struct ident { TyStruct $2 }

refTypeC :: { Type }
refTypeC :
          scopeC '*' baseTypeC { TyRef      $1 $3 }
  | const scopeC '*' baseTypeC { TyConstRef $2 $4 }

scopeC :: { MemArea }
scopeC :
    S           { Stack Nothing }
  | G           { Global }
  | ident       { PolyMem (Just $1) }
  | {- empty -} { PolyMem Nothing }

-- Haskell-style types

hsType :: { Type }
hsType :
    baseTypeHS   { $1 }
  | refTypeHS    { $1 }

baseTypeHS :: { Type }
baseTypeHS :
    IBool     { TyBool }
  | IChar     { TyChar }
  | IFloat    { TyFloat }
  | IDouble   { TyDouble }
  | '(' ')'   { TyVoid }

  | Sint8     { TyInt Int8 }
  | Sint16    { TyInt Int16 }
  | Sint32    { TyInt Int32 }
  | Sint64    { TyInt Int64 }

  | Uint8    { TyWord Word8 }
  | Uint16   { TyWord Word16 }
  | Uint32   { TyWord Word32 }
  | Uint64   { TyWord Word64 }

storedTypeHS :: { Type }
storedTypeHS :
    Stored baseTypeHS          { $2 }
  | struct ident               { TyStruct $2 }
  | Array integer storedTypeHS { TyArr $3 $2 }

refTypeHS :: { Type }
refTypeHS :
    Ref      scopeHS storedTypeHS { TyRef      $2 $3 }
  | ConstRef scopeHS storedTypeHS { TyConstRef $2 $3 }

scopeHS :: { MemArea }
scopeHS :
    Stack ident { Stack (Just $2) }
  | Global      { Global }

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
    AlexError (AlexPn addr l c, _, _, _) ->
      alexError $ "Lexer error at line " ++ show l ++ " col. " ++ show c
               ++ " and chars preceding token " ++ show addr
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

runParser :: String -> [ProcDef]
runParser s = case runAlex s ivoryParser of
  Left err    -> error err
  Right procs -> procs

-- XXX testing

parseFileTest :: FilePath -> IO (Either String [ProcDef])
parseFileTest fp = do
  cs <- readFile fp
  return (parseTest cs)
  where
--  parseTest :: String -> Either String Type
  parseTest s = runAlex s ivoryParser


}
