-- # -*- mode: haskell -*-
{
--
-- Ivory parser.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

-- TODO
-- types for allocs
-- neg numbers in expressions

module Ivory.Language.Syntax.Concrete.Parser where

import Data.Monoid

import Ivory.Language.Syntax.Concrete.ParseCore
import Ivory.Language.Syntax.Concrete.ParseAST
import Ivory.Language.Syntax.Concrete.Lexer
import Ivory.Language.Syntax.Concrete.Lexeme ( Token(..), Lexeme )
import Ivory.Language.Syntax.Concrete.Location

}

%name      ivoryParser
%tokentype { Lexeme }
%monad     { Parser } { (>>=) } { return }
%lexer     { lexer } { Located mempty TokEOF }
%error     { parseError }
%token

  integer      { $$@Located { locValue = TokInteger _ } }
  floatlit     { $$@Located { locValue = TokFloat   _ } }
  bitlit       { $$@Located { locValue = TokBitLit _  } }
  identifier   { $$@Located { locValue = TokIdent _   } }
  tyidentifier { $$@Located { locValue = TokTyIdent _ } }
  str          { $$@Located { locValue = TokString _  } }

  -- Statements
  if       { Located $$ (TokReserved "if") }
  else     { Located $$ (TokReserved "else") }
  assert   { Located $$ (TokReserved "assert") }
  assume   { Located $$ (TokReserved "assume") }
  pre      { Located $$ (TokReserved "pre") }
  post     { Located $$ (TokReserved "post") }
  assign   { Located $$ (TokReserved "let") }
  return   { Located $$ (TokReserved "return") }
  alloc    { Located $$ (TokReserved "alloc") }
  store    { Located $$ (TokReserved "store") }
  refCopy  { Located $$ (TokReserved "memcpy") }
  mapArr   { Located $$ (TokReserved "map") }
  upTo     { Located $$ (TokReserved "upTo") }
  forever  { Located $$ (TokReserved "forever") }

  -- Start of Ivory macros
  iMacro   { Located $$ (TokSym "$") }

  -- Expressions
  abs          { Located $$ (TokReserved "abs") }
  signum       { Located $$ (TokReserved "signum") }
  expOp        { Located $$ (TokReserved "exp") }
  sqrt         { Located $$ (TokReserved "sqrt") }
  log          { Located $$ (TokReserved "log") }
  pow          { Located $$ (TokReserved "pow") }

  sin          { Located $$ (TokReserved "sin") }
  cos          { Located $$ (TokReserved "cos") }
  tan          { Located $$ (TokReserved "tan") }

  asin         { Located $$ (TokReserved "asin") }
  acos         { Located $$ (TokReserved "acos") }
  atan         { Located $$ (TokReserved "atan") }

  sinh         { Located $$ (TokReserved "sinh") }
  cosh         { Located $$ (TokReserved "cosh") }
  tanh         { Located $$ (TokReserved "tanh") }

  asinh        { Located $$ (TokReserved "asinh") }
  acosh        { Located $$ (TokReserved "acosh") }
  atanh        { Located $$ (TokReserved "atanh") }

  isnan        { Located $$ (TokReserved "isnan") }
  isinf        { Located $$ (TokReserved "isinf") }
  round        { Located $$ (TokReserved "round") }
  ceil         { Located $$ (TokReserved "ceil") }
  floor        { Located $$ (TokReserved "floor") }
  const        { Located $$ (TokReserved "const") }

  -- Casting
  safeCast         { Located $$ (TokReserved "safeCast") }
  bitCast          { Located $$ (TokReserved "bitCast") }
  castWith         { Located $$ (TokReserved "castWith") }
  twosCompCast     { Located $$ (TokReserved "twosCompCast") }
  twosCompRep      { Located $$ (TokReserved "twosCompRep") }

  -- Other internals

  fromIx           { Located $$ (TokReserved "fromIx") }
  ixSize           { Located $$ (TokReserved "ixSize") }
  toIx             { Located $$ (TokReserved "toIx") }
  toCArray         { Located $$ (TokReserved "toCArray") }
  arrayLen         { Located $$ (TokReserved "arrayLen") }

  sizeOf           { Located $$ (TokReserved "sizeOf") }
  nullPtr          { Located $$ (TokReserved "nullPtr") }
  refToPtr         { Located $$ (TokReserved "refToPtr") }

  -- Type
  '::'      { Located $$ (TokSym "::") }

  '?'       { Located $$ (TokSym "?") }
  ':'       { Located $$ (TokSym ":") }

  -- Struct field dereference
  '.'       { Located $$ (TokSym ".") }
  '->'      { Located $$ (TokSym "->") }

  '=='      { Located $$ (TokSym "==") }
  '!='      { Located $$ (TokSym "!=") }

  -- Used for deref and mult
  '*'       { Located $$ (TokSym "*") }

  '/'       { Located $$ (TokSym "/") }

  '+'       { Located $$ (TokSym "+") }
  '-'       { Located $$ (TokSym "-") }
  '%'       { Located $$ (TokSym "%") }

  '='       { Located $$ (TokSym "=") }

  '<'       { Located $$ (TokSym "<") }
  '<='      { Located $$ (TokSym "<=") }
  '>='      { Located $$ (TokSym ">=") }
  '>'       { Located $$ (TokSym ">") }

  '|'       { Located $$ (TokSym "|") }
  '&'       { Located $$ (TokSym "&") }
  '^'       { Located $$ (TokSym "^") }
  '~'       { Located $$ (TokSym "~") }

  '!'       { Located $$ (TokSym "!") }
  '&&'      { Located $$ (TokSym "&&") }
  '||'      { Located $$ (TokSym "||") }
  '<<'      { Located $$ (TokSym "<<") }
  '>>'      { Located $$ (TokSym ">>") }

-- Other symbols
  '('       { Located $$ (TokBrack "(") }
  ')'       { Located $$ (TokBrack ")") }
  '{'       { Located $$ (TokBrack "{") }
  '}'       { Located $$ (TokBrack "}") }
  '['       { Located $$ (TokBrack "[") }
  ']'       { Located $$ (TokBrack "]") }

  ';'       { Located $$ (TokSep ";") }
  ','       { Located $$ (TokSep ",") }

  '@'       { Located $$ (TokSym "@") }
  '<-'      { Located $$ (TokSym "<-") }

  -- Types
  bool     { Located $$ (TokReserved "bool") }
  char     { Located $$ (TokReserved "char") }
  float    { Located $$ (TokReserved "float") }
  double   { Located $$ (TokReserved "double") }
  void     { Located $$ (TokReserved "void") }

  int8_t   { Located $$ (TokReserved "int8_t") }
  int16_t  { Located $$ (TokReserved "int16_t") }
  int32_t  { Located $$ (TokReserved "int32_t") }
  int64_t  { Located $$ (TokReserved "int64_t") }

  uint8_t  { Located $$ (TokReserved "uint8_t") }
  uint16_t { Located $$ (TokReserved "uint16_t") }
  uint32_t { Located $$ (TokReserved "uint32_t") }
  uint64_t { Located $$ (TokReserved "uint64_t") }

  S        { Located $$ (TokReserved "S") }
  G        { Located $$ (TokReserved "G") }

  IBool     { Located $$ (TokReserved "IBool") }
  IChar     { Located $$ (TokReserved "IChar") }
  IFloat    { Located $$ (TokReserved "IFloat") }
  IDouble   { Located $$ (TokReserved "IDouble") }
  IString   { Located $$ (TokReserved "IString") }

  Sint8     { Located $$ (TokReserved "Sint8") }
  Sint16    { Located $$ (TokReserved "Sint16") }
  Sint32    { Located $$ (TokReserved "Sint32") }
  Sint64    { Located $$ (TokReserved "Sint64") }

  Uint8    { Located $$ (TokReserved "Uint8") }
  Uint16   { Located $$ (TokReserved "Uint16") }
  Uint32   { Located $$ (TokReserved "Uint32") }
  Uint64   { Located $$ (TokReserved "Uint64") }

  Ix       { Located $$ (TokReserved "Ix") }
  ix_t     { Located $$ (TokReserved "ix_t") }

  Ref      { Located $$ (TokReserved "Ref") }
  ConstRef { Located $$ (TokReserved "ConstRef") }
  Array    { Located $$ (TokReserved "Array") }
  Struct   { Located $$ (TokReserved "Struct") }
  Stored   { Located $$ (TokReserved "Stored") }

  Stack    { Located $$ (TokReserved "Stack") }
  Global   { Located $$ (TokReserved "Global") }

  -- Keywords
  struct   { Located $$ (TokReserved "struct") }
  abstract { Located $$ (TokReserved "abstract") }
  string   { Located $$ (TokReserved "string") }

  ty       { Located $$ (TokReserved "type") }
  include  { Located $$ (TokReserved "include") }

  -- Bit data
  bitdata  { Located $$ (TokReserved "bitdata") }
  Bit      { Located $$ (TokReserved "Bit") }
  Bits     { Located $$ (TokReserved "Bits") }
  BitArray { Located $$ (TokReserved "BitArray") }
  as       { Located $$ (TokReserved "as") }
  '_'      { Located $$ (TokSym "_") }
  '#'      { Located $$ (TokSym "#") }

--------------------------------------------------------------------------------
-- Precedence

%right '#'
%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%right '*' '~' '!' '-'
-- '[' assumed to be followed by ']'
%left '.' '@' '->' '['
-- Tighter than normal binding
%right '&'
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
-- Top-level definitions

defs :: { [GlobalSym] }
defs : defs procDef       { GlobalProc     $2 : $1 }
     | defs structDef     { GlobalStruct   $2 : $1 }
     | defs bdDef         { GlobalBitData  $2 : $1 }
     | defs typeDef       { GlobalTypeDef  $2 : $1 }
     | defs constDef      { GlobalConstDef $2 : $1 }
     | defs includeDef    { GlobalInclude  $2 : $1 }
     | {- empty -}        { [] }

----------------------------------------
-- Include other modules (Ivory's "depend")

includeDef :: { IncludeDef }
includeDef : include ident { IncludeDef (unLoc $2) ($1 <> getLoc $2)  }

----------------------------------------
-- Constant definitions

constDef :: { ConstDef }
constDef :
         ident '=' exp ';' { ConstDef (unLoc $1) $3 Nothing   (getLoc $1 <> getLoc $3) }
  | type ident '=' exp ';' { ConstDef (unLoc $2) $4 (Just $1) (mconcat [ getLoc $1
                                                                       , getLoc $2
                                                                       , getLoc $4]) }

----------------------------------------
-- Procs

procDef :: { ProcDef }
procDef :
  type ident '(' args ')' '{' stmts '}' prePostBlk
    { ProcDef $1 (unLoc $2) (reverse $4) (reverse $7) $9 (mconcat [ getLoc $1
                                                                  , getLoc $2
                                                                  , getLoc $7
                                                                  , getLoc $9
                                                                  ]) }

tyArg :: { (Type, Var) }
tyArg : type ident { ($1, unLoc $2) }

-- Zero or more typed arguments, separated by arbitrary many ','s.
args :: { [(Type, Var)] }
args :  args ',' tyArg         { $3 : $1 }
      | args ','               { $1 }
      | tyArg                  { [$1] }
      | {- empty -}            { [] }

-- pre/post conditions
prePostBlk :: { [PrePost] }
prePostBlk :
    '{' prePosts '}'       { reverse $2 }
  | {- empty -}            { [] }

prePosts :: { [PrePost] }
prePosts :
    prePosts prePost ';' { $2 : $1 }
  | prePost ';'          { [$1] }

prePost :: { PrePost }
prePost :
    pre  '(' exp ')' { PreCond  $3 }
  | post '(' exp ')' { PostCond $3 }

----------------------------------------
-- Statements

simpleStmt :: { Stmt }
simpleStmt :
    assert exp                    { LocStmt (atBin (Assert $2) $1 $2) }
  | assume exp                    { LocStmt (atBin (Assume $2) $1 $2) }
  | assign      ident '=' exp     { LocStmt (atList (Assign (unLoc $2) $4 Nothing)
                                      [ $1, getLoc $2, getLoc $4 ]) }
  | assign type ident '=' exp     { LocStmt (atList (Assign (unLoc $3) $5 (Just $2))
                                                    [ $1, getLoc $2, getLoc $3, getLoc $5]) }

  | return                        { LocStmt (ReturnVoid `at` $1) }
  | return exp                    { LocStmt (atBin (Return $2) $1 $2) }

  -- Allocation
  | alloc '*' ident               { LocStmt (atBin (AllocRef (AllocBase (unLoc $3) Nothing))
                                               $1 $3) }
  | alloc '*' ident '=' exp       { LocStmt (atList (AllocRef (AllocBase (unLoc $3) (Just $5)))
                                               [$1, getLoc $3, getLoc $5]) }

  | alloc ident '[' ']'           { LocStmt (atBin (AllocRef (AllocArr (unLoc $2) []))
                                               $1 $2) }
  | alloc ident '[' ']' '='
      '{' exps '}'                { LocStmt (atList (AllocRef (AllocArr (unLoc $2) (reverse $7)))
                                               [ $1, getLoc $2, getLoc $7]) }

  | alloc ident '{' '}'           { LocStmt (atBin (AllocRef (AllocStruct (unLoc $2) []))
                                               $1 $2) }
  | alloc ident '='
      '{' fieldAssigns '}'        { LocStmt (atBin (AllocRef (AllocStruct (unLoc $2) (reverse $5)))
                                               $1 $2) }

  | refCopy ident ident           { LocStmt (atList (RefCopy (ExpVar (unLoc $2)) (ExpVar (unLoc $3)))
                                               [$1, getLoc $2, getLoc $3]) }

  -- Storing
  | store exp as exp              { LocStmt (atList (Store $2 $4) [$1, getLoc $2, getLoc $4]) }

  -- Function calls
  | ident expArgs                 { LocStmt (atBin (NoBindCall (unLoc $1) $2) $1 $2) }

  | ivoryMacro                    { LocStmt ((IvoryMacroStmt Nothing (unLoc $1)) `at` getLoc $1) }
  | ident '<-' ivoryMacro         { LocStmt (atBin (IvoryMacroStmt (Just (unLoc $1)) (unLoc $3))
                                               $1 $3) }

ivoryMacro :: { Located (String, [Exp]) }
ivoryMacro : iMacro ident          { atBin (unLoc $2, []) $1 (getLoc $2) }
           | iMacro ident expArgs  { atList (unLoc $2, $3) [$1, getLoc $2, getLoc $3] }

blkStmt :: { Stmt }
blkStmt :
    mapArr ident '{' stmts '}'       { LocStmt (atList (MapArr (unLoc $2) (reverse $4))
                                                  [ $1, getLoc $2, getLoc $4 ]) }
  | upTo exp ident '{' stmts '}'     { LocStmt (atList (UpTo $2 (unLoc $3) (reverse $5))
                                                  [$1, getLoc $2, getLoc $3, getLoc $5]) }
  | forever '{' stmts '}'            { LocStmt (atBin (Forever (reverse $3)) $1 $2) }

  | if exp '{' stmts '}'
      else '{' stmts '}'             { LocStmt (atList (IfTE $2 (reverse $4) (reverse $8))
                                                  [ getLoc $2, getLoc $4, getLoc $8 ]) }

-- Zero or more statements.
stmts :: { [Stmt] }
stmts : stmts simpleStmt ';'   { $2 : $1 }
      | stmts blkStmt          { $2 : $1 }
      | {- empty -}            { [] }

expArgs :: { [Exp] }
expArgs : '(' exps ')' { reverse $2 }

-- Zero or more expressions, separated by arbitrary many ','s.
exps :: { [Exp] }
exps : exps ',' exp           { $3 : $1 }
     | exps ','               { $1 }
     | exp                    { [$1] }
     | {- empty -}            { [] }

fieldAssigns :: { [(FieldNm, Exp)] }
fieldAssigns :
    fieldAssigns ',' fieldAssign { $3 : $1 }
  | fieldAssigns ','             { $1 }
  | fieldAssign                  { [$1] }
  | {- empty -}                  { [] }

fieldAssign :: { (FieldNm, Exp) }
fieldAssign : ident '=' exp { (unLoc $1, $3) }

----------------------------------------
-- Expressions
exp :: { Exp }
exp : integer            { let TokInteger i = unLoc $1 in
                           LocExp (ExpLit (LitInteger i) `at` $1) }

    | str                { let TokString s = unLoc $1 in
                           LocExp (ExpLit (LitString s) `at` $1) }

    | floatlit           { let TokFloat f = unLoc $1 in
                           LocExp (ExpLit (LitFloat f) `at` $1) }

    -- Works for Haskell values, too!
    | ident              { LocExp ((ExpVar (unLoc $1)) `at` $1) }

    -- Used only in post-conditions (otherwise, it's a statement).
    | return             { LocExp (ExpRet `at` $1) }

    | '(' exp ')'        { $2 }

    -- Areas
    | '*' exp              { LocExp (atBin (ExpDeref $2) $1 $2) }
    | exp '@' exp          { LocExp (atBin (ExpArray $1 $3) $1 $3) }
    | exp '[' exp ']'      { LocExp (atBin (ExpDeref (ExpArray $1 $3)) $1 $3) }
     | exp '.' exp         { LocExp (atBin (ExpStruct $1 $3) $1 $3) }
    | exp '->' exp         { LocExp (atBin (ExpDeref (ExpStruct $1 $3)) $1 $3) }
    | '&' ident            { LocExp (atBin (ExpAddrOf (unLoc $2)) $1 $2) }

    | libFuncExp           { $1 }

    -- Ivory expression macros
    | ivoryMacro           { LocExp (IvoryMacroExp `fmap` $1) }

    -- Function calls
    | ident expArgs        { LocExp (atBin (ExpCall (unLoc $1) $2) $1 $2) }

    -- Unary operators
    | '!'       exp      { LocExp (atBin (ExpOp NotOp [$2]) $1 $2) }
    | '-'       exp      { LocExp (atBin (ExpOp NegateOp [$2]) $1 $2) }
    | '~'       exp      { LocExp (atBin (ExpOp BitComplementOp [$2]) $1 $2) }

    -- Binary operators
    | exp '||'  exp      { LocExp (atBin (ExpOp OrOp [$1, $3]) $1 $3) }
    | exp '&&'  exp      { LocExp (atBin (ExpOp AndOp [$1, $3]) $1 $3) }
    | exp '|'   exp      { LocExp (atBin (ExpOp BitOrOp [$1, $3]) $1 $3) }
    | exp '^'   exp      { LocExp (atBin (ExpOp BitXorOp [$1, $3]) $1 $3) }
    | exp '&'   exp      { LocExp (atBin (ExpOp BitAndOp [$1, $3]) $1 $3) }
    | exp '<<'  exp      { LocExp (atBin (ExpOp BitShiftLOp [$1, $3]) $1 $3) }
    | exp '>>'  exp      { LocExp (atBin (ExpOp BitShiftROp [$1, $3]) $1 $3) }

    | exp '=='  exp      { LocExp (atBin (ExpOp EqOp [$1, $3]) $1 $3) }
    | exp '!='  exp      { LocExp (atBin (ExpOp NeqOp [$1, $3]) $1 $3) }

    | exp '<'   exp      { LocExp (atBin (ExpOp (LtOp False) [$1, $3]) $1 $3) }
    | exp '<='  exp      { LocExp (atBin (ExpOp (LtOp True) [$1, $3] ) $1 $3)}
    | exp '>'   exp      { LocExp (atBin (ExpOp (GtOp False) [$1, $3]) $1 $3) }
    | exp '>='  exp      { LocExp (atBin (ExpOp (GtOp True) [$1, $3] ) $1 $3)}

    | exp '+'   exp      { LocExp (atBin (ExpOp AddOp [$1, $3]) $1 $3) }
    | exp '-'   exp      { LocExp (atBin (ExpOp SubOp [$1, $3]) $1 $3) }

    | exp '*'   exp      { LocExp (atBin (ExpOp MulOp [$1, $3]) $1 $3) }
    | exp '/'   exp      { LocExp (atBin (ExpOp DivOp [$1, $3]) $1 $3) }
    | exp '%'   exp      { LocExp (atBin (ExpOp ModOp [$1, $3]) $1 $3) }

    -- Tertiary operators
    | exp '?' exp ':' exp { LocExp ((ExpOp CondOp [$1, $3, $5]) `at` (getLoc [$1, $3, $5]))  }

libFuncExp :: { Exp }
libFuncExp :
      abs          expArgs { LocExp (atBin (ExpOp AbsOp $2) $1 $2) }
    | signum       expArgs { LocExp (atBin (ExpOp SignumOp $2) $1 $2) }
    | expOp        expArgs { LocExp (atBin (ExpOp FExpOp $2) $1 $2) }
    | sqrt         expArgs { LocExp (atBin (ExpOp FSqrtOp $2) $1 $2) }
    | log          expArgs { LocExp (atBin (ExpOp FLogOp $2) $1 $2) }
    | pow          expArgs { LocExp (atBin (ExpOp FPowOp $2) $1 $2) }
    | sin          expArgs { LocExp (atBin (ExpOp FSinOp $2) $1 $2) }
    | cos          expArgs { LocExp (atBin (ExpOp FCosOp $2) $1 $2) }
    | tan          expArgs { LocExp (atBin (ExpOp FTanOp $2) $1 $2) }
    | asin         expArgs { LocExp (atBin (ExpOp FAsinOp $2) $1 $2) }
    | acos         expArgs { LocExp (atBin (ExpOp FAcosOp $2) $1 $2) }
    | atan         expArgs { LocExp (atBin (ExpOp FAtanOp $2) $1 $2) }
    | sinh         expArgs { LocExp (atBin (ExpOp FSinhOp $2) $1 $2) }
    | cosh         expArgs { LocExp (atBin (ExpOp FCoshOp $2) $1 $2) }
    | tanh         expArgs { LocExp (atBin (ExpOp FTanhOp $2) $1 $2) }
    | asinh        expArgs { LocExp (atBin (ExpOp FAsinhOp $2) $1 $2) }
    | acosh        expArgs { LocExp (atBin (ExpOp FAcoshOp $2) $1 $2) }
    | atanh        expArgs { LocExp (atBin (ExpOp FAtanhOp $2) $1 $2) }
    | isnan        expArgs { LocExp (atBin (ExpOp IsNanOp $2) $1 $2) }
    | isinf        expArgs { LocExp (atBin (ExpOp IsInfOp $2) $1 $2) }
    | round        expArgs { LocExp (atBin (ExpOp RoundFOp $2) $1 $2) }
    | ceil         expArgs { LocExp (atBin (ExpOp CeilFOp $2) $1 $2) }
    | floor        expArgs { LocExp (atBin (ExpOp FloorFOp $2) $1 $2) }
    | const        expArgs { LocExp (atBin (ExpOp ConstRefOp $2) $1 $2) }

    | castWith     expArgs { LocExp (atBin (ExpOp CastWith $2) $1 $2) }
    | safeCast     expArgs { LocExp (atBin (ExpOp SafeCast $2) $1 $2) }
    | bitCast      expArgs { LocExp (atBin (ExpOp BitCast $2) $1 $2) }
    | twosCompCast expArgs { LocExp (atBin (ExpOp TwosCompCast $2) $1 $2) }
    | twosCompRep  expArgs { LocExp (atBin (ExpOp TwosCompRep $2) $1 $2) }

    | toIx         expArgs { LocExp (atBin (ExpOp ToIx $2) $1 $2) }
    | fromIx       expArgs { LocExp (atBin (ExpOp FromIx $2) $1 $2) }
    | ixSize       expArgs { LocExp (atBin (ExpOp IxSize $2) $1 $2) }
    | arrayLen     expArgs { LocExp (atBin (ExpOp ArrayLen $2) $1 $2) }
    | sizeOf       expArgs { LocExp (atBin (ExpOp SizeOf $2) $1 $2) }
    | nullPtr      expArgs { LocExp (atBin (ExpOp NullPtr $2) $1 $2) }
    | refToPtr     expArgs { LocExp (atBin (ExpOp RefToPtr $2) $1 $2) }
    | toCArray     expArgs { LocExp (atBin (ExpOp ToCArray $2) $1 $2) }

----------------------------------------
-- Types

typeDef :: { TypeDef }
typeDef :
  ty tyident '=' type ';' { TypeDef (unLoc $2) $4 (mconcat [$1, getLoc $2, getLoc $4]) }

type :: { Type }
type :
    simpleCType      { $1 }
  | cType            { $1 }
  | tyident          { LocTy (TySynonym (unLoc $1) `at` $1) }
  | '(' type ')'     { $2 }

-- C-style types

simpleCType :: { Type }
simpleCType :
    bool                      { LocTy (TyBool `at` getLoc $1) }
  | char                      { LocTy (TyChar `at` getLoc $1) }
  | float                     { LocTy (TyFloat `at` getLoc $1) }
  | double                    { LocTy (TyDouble `at` getLoc $1) }
  | string                    { LocTy (TyString `at` getLoc $1) }
  | void                      { LocTy (TyVoid `at` getLoc $1) }

  | int8_t                    { LocTy ((TyInt Int8) `at` getLoc $1) }
  | int16_t                   { LocTy ((TyInt Int16) `at` getLoc $1) }
  | int32_t                   { LocTy ((TyInt Int32) `at` getLoc $1) }
  | int64_t                   { LocTy ((TyInt Int64) `at` getLoc $1) }

  | uint8_t                   { LocTy ((TyWord Word8) `at` getLoc $1) }
  | uint16_t                  { LocTy ((TyWord Word16) `at` getLoc $1) }
  | uint32_t                  { LocTy ((TyWord Word32) `at` getLoc $1) }
  | uint64_t                  { LocTy ((TyWord Word64) `at` getLoc $1) }

  | ix_t integer              { let TokInteger i = unLoc $2 in
                                LocTy (atBin (TyIx i) $1 $2) }

cType :: { Type }
cType :
          scopeC '*' type        { LocTy (atBin (TyRef (unLoc $1) $3) $1 $3)  }
  | const scopeC '*' type        { LocTy (atList (TyConstRef (unLoc $2) $4)
                                                 [$1, getLoc $2, getLoc $4]) }
  | type '[' integer ']'         { let TokInteger i = unLoc $3 in
                                   LocTy (atBin (TyArray $1 i) $1 $3) }
  | struct structName            { LocTy (atBin (TyStruct (unLoc $2)) $1 $2) }
  | '&' type                     { LocTy (atBin (TyStored $2) $1 $2) }

scopeC :: { Located Scope }
scopeC :
    S           { Stack Nothing `at` $1 }
  | G           { Global `at` $1 }
  | ident       { PolyMem (Just (unLoc $1)) `at` (getLoc $1) }
  | {- empty -} { PolyMem Nothing `at` NoLoc }

typeHS :: { Type }
typeHS :
    simpleHSType     { $1 }
  | hsType           { $1 }
  | tyident          { LocTy (TySynonym (unLoc $1) `at` $1) }
  | '(' typeHS ')'   { $2 }

-- Haskell-style types
simpleHSType :: { Type }
simpleHSType :
    IBool                   { LocTy (TyBool `at` getLoc $1) }
  | IChar                   { LocTy (TyChar `at` getLoc $1) }
  | IFloat                  { LocTy (TyFloat `at` getLoc $1) }
  | IDouble                 { LocTy (TyDouble `at` getLoc $1) }
  | IString                 { LocTy (TyString `at` getLoc $1) }
  | '(' ')'                 { LocTy (TyVoid `at` getLoc $1) }

  | Sint8                   { LocTy ((TyInt Int8) `at` getLoc $1) }
  | Sint16                  { LocTy ((TyInt Int16) `at` getLoc $1) }
  | Sint32                  { LocTy ((TyInt Int32)`at` getLoc $1) }
  | Sint64                  { LocTy ((TyInt Int64) `at` getLoc $1) }

  | Uint8                   { LocTy ((TyWord Word8) `at` getLoc $1) }
  | Uint16                  { LocTy ((TyWord Word16) `at` getLoc $1) }
  | Uint32                  { LocTy ((TyWord Word32) `at` getLoc $1) }
  | Uint64                  { LocTy ((TyWord Word64) `at` getLoc $1) }

  | Ix integer              { let TokInteger i = unLoc $2 in
                              LocTy (atBin (TyIx i) $1 $2) }

hsType :: { Type }
hsType :
    Ref      scopeHS typeHS { LocTy (atList (TyRef (unLoc $2) $3) [ getLoc $1
                                                                  , getLoc $2
                                                                  , getLoc $3 ]) }
  | ConstRef scopeHS typeHS { LocTy (atList (TyConstRef (unLoc $2) $3) [ getLoc $1
                                                                       , getLoc $2
                                                                       , getLoc $3 ]) }
  | Array    integer typeHS { let TokInteger i = unLoc $2 in
                              LocTy (atList (TyArray $3 i) [ getLoc $1
                                                           , getLoc $2
                                                           , getLoc $3]) }
  | Struct   structName     { LocTy (atBin (TyStruct (unLoc $2)) $1 $2) }
  | Stored   typeHS         { LocTy (atBin (TyStored $2) $1 $2) }

scopeHS :: { Located Scope }
scopeHS : Stack tyident { atBin (Stack (Just (unLoc $2))) $1 $2 }
        | Global        { Global `at` $1 }

-- Bit types
bitType :: { BitTy }
bitType :
    Bit                        { LocBitTy (Bit `at` $1) }
  | Bits integer               { let TokInteger i = unLoc $2 in
                                 LocBitTy (atBin (Bits i) $1 $2) }
  | BitArray integer bitType   { let TokInteger i = unLoc $2 in
                                 LocBitTy (atList (BitArray i $3) [ getLoc $1
                                                                  , getLoc $2
                                                                  , getLoc $3 ]) }
  | '(' bitType ')'            { $2 }
  | tyident                    { LocBitTy (BitTySynonym `fmap` $1) }

----------------------------------------
-- Struct definitions

structDef :: { StructDef }
structDef :
    struct structName '{' fields '}' { StructDef (unLoc $2) (reverse $4) (getLoc $2) }
  -- Remove parsed quotes first
  | abstract struct structName str   { let TokString f = unLoc $4 in
                                       AbstractDef (unLoc $3) (filter (/= '\"') f) (getLoc $3) }
  | string struct structName integer { let TokInteger i = unLoc $4 in
                                       StringDef (unLoc $3) i (getLoc $3) }

structName :: { Located String }
structName :
    tyident { $1 }
  | ident   { $1 }

field :: { Field }
field :
  -- Haskell style
    ident '::' typeHS  { Field (unLoc $1) $3 (getLoc $1 <> getLoc $3) }
  -- C style
  | type ident         { Field (unLoc $2) $1 (getLoc $1 <> getLoc $2)}

-- 1 or more fields, separated (but optionally ending with) ';'.
fields :: { [Field] }
fields :
    fields ';' field   { $3 : $1 }
  | fields ';'         { $1 }
  | field              { [$1] }

--------------------------------------------------------------------------------
-- Bit data

-- Bitdata definition
bdDef :: { BitDataDef }
bdDef : bitdata tyident '::' bitType
    '=' bdConstrs         { BitDataDef (unLoc $2) $4 (reverse $6)
                            (mconcat [ getLoc $1, getLoc $2, getLoc $3, getLoc $6 ]) }

-- One or more bitdata constructors, separated by '|'
bdConstrs :: { [Constr] }
bdConstrs :
    bdConstrs '|' bdConstr { $3 : $1 }
  | bdConstr               { [$1] }

bdConstr :: { Constr }
bdConstr : ident bdRecord bdLayout { Constr (unLoc $1) $2 $3
                                       (mconcat [ getLoc $1, getLoc $2 ]) }

-- Zero or more fields.
bdRecord :: { [BitField] }
bdRecord :
    '{' bdFields '}' { reverse $2 }
  | {- empty -}      { [] }

bdFields :: { [BitField] }
bdFields :
    bdFields ',' bdField { $3 : $1 }
  | bdField              { [$1] }

bdField :: { BitField }
bdField :
    ident '::' bitType { BitField (Just (unLoc $1)) $3 (getLoc $1 <> getLoc $3) }
  | '_'   '::' bitType { BitField Nothing   $3 ($1 <> getLoc $3) }

bdLayout :: { [LayoutItem] }
bdLayout :
    as bdItems   { reverse $2 }
  | {- empty -}  { [] }

-- One or more items, separated by #
bdItems :: { [LayoutItem] }
bdItems :
    bdItems '#' bdItem { $3 : $1 }
  | bdItem             { [$1] }

bdItem :: { LayoutItem }
bdItem :
    ident      { LayoutField (unLoc $1) }
  | integer    { let TokInteger i = unLoc $1 in
                 LayoutConst (BitLitUnknown i) }
  | bitLiteral { LayoutConst $1 }

-- Parse n-bit natural, e.g.,
--
-- 8b0 -- 8 0-bits
--
-- 2b01 -- 01
-- First field is width, second is "b[0,1]+"
bitLiteral :: { BitLiteral }
bitLiteral : bitlit { let TokBitLit bl = unLoc $1 in
                      BitLitKnown (fst bl) (snd bl) }

--------------------------------------------------------------------------------
-- Namespaces

ident :: { Located String }
ident :
    identifier                  { let TokIdent i = unLoc $1 in
                                  i `at` $1 }
  | tyidentifier '.' identifier { let TokTyIdent t = unLoc $1 in
                                  let TokIdent i   = unLoc $3 in
                                  atBin (t ++ '.':i) $1 $3 }

tyident :: { Located String }
tyident :
    tyidentifier                  { let TokTyIdent t = unLoc $1 in
                                    t `at` $1 }
  | tyidentifier '.' tyidentifier { let TokTyIdent t0 = unLoc $1 in
                                    let TokTyIdent t1 = unLoc $3 in
                                    atBin (t0 ++ '.':t1) $1 $3 }

--------------------------------------------------------------------------------
