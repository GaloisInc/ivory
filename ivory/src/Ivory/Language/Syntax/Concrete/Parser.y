-- # -*- mode: haskell -*-
{
--
-- Ivory lexer.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--
-- Parser.hs file is generated!

-- TODO
-- explicit types for lets
-- remove constRef (const covers it)

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
  integer      { TokInteger $$ }
  bitlit       { TokBitLit $$ }
  identifier   { TokIdent $$ }
  tyidentifier { TokTyIdent $$ }
  fp           { TokFilePath $$ }

  -- Statements
  if       { TokReserved "if" }
  else     { TokReserved "else" }
  assert   { TokReserved "assert" }
  assume   { TokReserved "assume" }
  pre      { TokReserved "pre" }
  post     { TokReserved "post" }
  assign   { TokReserved "let" }
  return   { TokReserved "return" }
  alloc    { TokReserved "alloc" }
  store    { TokReserved "store" }
  refCopy  { TokReserved "memcpy" }
  mapArr   { TokReserved "map" }
  upTo     { TokReserved "upTo" }
  forever  { TokReserved "forever" }

  -- Start of Ivory macros
  iMacro   { TokSym "$" }

 -- Expressions
  abs          { TokReserved "abs" }
  signum       { TokReserved "signum" }
  expOp        { TokReserved "exp" }
  sqrt         { TokReserved "sqrt" }
  log          { TokReserved "log" }
  pow          { TokReserved "pow" }

  sin          { TokReserved "sin" }
  cos          { TokReserved "cos" }
  tan          { TokReserved "tan" }

  asin         { TokReserved "asin" }
  acos         { TokReserved "acos" }
  atan         { TokReserved "atan" }

  sinh         { TokReserved "sinh" }
  cosh         { TokReserved "cosh" }
  tanh         { TokReserved "tanh" }

  asinh        { TokReserved "asinh" }
  acosh        { TokReserved "acosh" }
  atanh        { TokReserved "atanh" }

  isnan        { TokReserved "isnan" }
  isinf        { TokReserved "isinf" }
  round        { TokReserved "round" }
  ceil         { TokReserved "ceil" }
  floor        { TokReserved "floor" }
  const        { TokReserved "const" }

  -- Casting
  safeCast         { TokReserved "safeCast" }
  bitCast          { TokReserved "bitCast" }
  castWith         { TokReserved "castWith" }
  twosCompCast     { TokReserved "twosCompCast" }
  twosCompRep      { TokReserved "twosCompRep" }

  -- Other internals

  fromIx           { TokReserved "fromIx" }
  ixSize           { TokReserved "ixSize" }
  toIx             { TokReserved "toIx" }
  toCArray         { TokReserved "toCArray" }
  arrayLen         { TokReserved "arrayLen" }

  sizeOf           { TokReserved "sizeOf" }
  nullPtr          { TokReserved "nullPtr" }
  refToPtr         { TokReserved "refToPtr" }

  -- Type
  '::'      { TokSym "::" }

  '?'       { TokSym "?" }
  ':'       { TokSym ":" }

  -- Struct field dereference
  '.'       { TokSym "." }
  '->'      { TokSym "->" }

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

-- Other symbols
  '('       { TokBrack "(" }
  ')'       { TokBrack ")" }
  '{'       { TokBrack "{" }
  '}'       { TokBrack "}" }
  '['       { TokBrack "[" }
  ']'       { TokBrack "]" }

  ';'       { TokSep ";" }
  ','       { TokSep "," }

  '@'       { TokSym "@" }
  '<-'      { TokSym "<-" }

  -- Types
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

  Ix       { TokReserved "Ix" }
  ix_t     { TokReserved "ix_t" }

  Ref      { TokReserved "Ref" }
  ConstRef { TokReserved "ConstRef" }
  Array    { TokReserved "Array" }
  Struct   { TokReserved "Struct" }
  Stored   { TokReserved "Stored" }

  Stack    { TokReserved "Stack" }
  Global   { TokReserved "Global" }

  -- Keywords
  struct   { TokReserved "struct" }
  abstract { TokReserved "abstract" }
  string   { TokReserved "string" }

  ty       { TokReserved "type" }
  include  { TokReserved "include" }

  -- Bit data
  bitdata  { TokReserved "bitdata" }
  Bit      { TokReserved "Bit" }
  Bits     { TokReserved "Bits" }
  BitArray { TokReserved "BitArray" }
  as       { TokReserved "as" }
  '_'      { TokSym "_" }
  '#'      { TokSym "#" }

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
includeDef : include ident { IncludeDef $2 }

----------------------------------------
-- Constant definitions

constDef :: { ConstDef }
constDef :
         ident '=' exp ';' { ConstDef $1 $3 Nothing   }
  | type ident '=' exp ';' { ConstDef $2 $4 (Just $1) }

----------------------------------------
-- Procs

procDef :: { ProcDef }
procDef :
  type ident '(' args ')'
    '{' stmts '}' prePostBlk { ProcDef $1 $2 (reverse $4) (reverse $7) $9 }

tyArg :: { (Type, Var) }
tyArg : type ident { ($1, $2) }

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
    assert exp                    { Assert $2 }
  | assume exp                    { Assume $2 }
  | assign      ident '=' exp     { Assign $2 $4 Nothing   }
  | assign type ident '=' exp     { Assign $3 $5 (Just $2) }
  | return                        { ReturnVoid }
  | return exp                    { Return $2 }

  -- Allocation
  | alloc '*' ident               { AllocRef (AllocBase $3 Nothing) }
  | alloc '*' ident '=' exp       { AllocRef (AllocBase $3 (Just $5)) }

  | alloc ident '[' ']'           { AllocRef (AllocArr $2 []) }
  | alloc ident '[' ']' '='
      '{' exps '}'                { AllocRef (AllocArr $2 (reverse $7)) }

  | alloc ident                   { AllocRef (AllocStruct $2 []) }
  | alloc ident '='
      '{' fieldAssigns '}'        { AllocRef (AllocStruct $2 (reverse $5)) }

  | refCopy ident ident           { RefCopy (ExpVar $2) (ExpVar $3) }

  -- Storing
  | store exp as exp              { Store $2 $4 }

  -- Function calls
  | ident expArgs                 { NoBindCall $1 $2 }

  | ivoryMacro                    {IvoryMacroStmt Nothing $1 }
  | ident '<-' ivoryMacro         {IvoryMacroStmt (Just $1) $3 }

ivoryMacro :: { (String, [Exp]) }
ivoryMacro : iMacro ident          { ($2, []) }
           | iMacro ident expArgs  { ($2, $3) }

blkStmt :: { Stmt }
blkStmt :
    mapArr ident '{' stmts '}'       { MapArr $2 (reverse $4) }
  | upTo exp ident '{' stmts '}'     { UpTo   $2 $3 (reverse $5) }
  | forever '{' stmts '}'            { Forever (reverse $3) }
  | if exp '{' stmts '}'
      else '{' stmts '}'             { IfTE $2 (reverse $4) (reverse $8) }

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
fieldAssign : ident '=' exp { ($1, $3) }

----------------------------------------
-- Expressions
exp :: { Exp }
exp : integer            { ExpLit (LitInteger $1) }

    -- Works for Haskell values, too!
    | ident              { ExpVar $1 }

    -- Used only in post-conditions (otherwise, it's a statement).
    | return             { ExpRet }

    | '(' exp ')'        { $2 }

    -- Areas
    | '*' exp              { ExpDeref $2 }
    | exp '@' exp          { ExpArray $1 $3 }
    | exp '[' exp ']'      { ExpDeref (ExpArray $1 $3) }
    | exp '.' exp          { ExpStruct $1 $3 }
    | exp '->' exp         { ExpDeref (ExpStruct $1 $3) }
    | '&' ident            { ExpAddrOf $2 }

    | libFuncExp           { $1 }

    -- Ivory expression macros
    | ivoryMacro           { IvoryMacroExp $1 }

    -- Function calls
    | ident expArgs        { ExpCall $1 $2 }

    -- Unary operators
    | '!'       exp      { ExpOp NotOp [$2] }
    | '-'       exp      { ExpOp NegateOp [$2] }
    | '~'       exp      { ExpOp BitComplementOp [$2] }

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

libFuncExp :: { Exp }
libFuncExp :
      abs          expArgs { ExpOp AbsOp        $2 }
    | signum       expArgs { ExpOp SignumOp     $2 }
    | expOp        expArgs { ExpOp FExpOp       $2 }
    | sqrt         expArgs { ExpOp FSqrtOp      $2 }
    | log          expArgs { ExpOp FLogOp       $2 }
    | pow          expArgs { ExpOp FPowOp       $2 }
    | sin          expArgs { ExpOp FSinOp       $2 }
    | cos          expArgs { ExpOp FCosOp       $2 }
    | tan          expArgs { ExpOp FTanOp       $2 }
    | asin         expArgs { ExpOp FAsinOp      $2 }
    | acos         expArgs { ExpOp FAcosOp      $2 }
    | atan         expArgs { ExpOp FAtanOp      $2 }
    | sinh         expArgs { ExpOp FSinhOp      $2 }
    | cosh         expArgs { ExpOp FCoshOp      $2 }
    | tanh         expArgs { ExpOp FTanhOp      $2 }
    | asinh        expArgs { ExpOp FAsinhOp     $2 }
    | acosh        expArgs { ExpOp FAcoshOp     $2 }
    | atanh        expArgs { ExpOp FAtanhOp     $2 }
    | isnan        expArgs { ExpOp IsNanOp      $2 }
    | isinf        expArgs { ExpOp IsInfOp      $2 }
    | round        expArgs { ExpOp RoundFOp     $2 }
    | ceil         expArgs { ExpOp CeilFOp      $2 }
    | floor        expArgs { ExpOp FloorFOp     $2 }
    | const        expArgs { ExpOp ConstRefOp   $2 }

    | castWith     expArgs { ExpOp CastWith     $2 }
    | safeCast     expArgs { ExpOp SafeCast     $2 }
    | bitCast      expArgs { ExpOp BitCast      $2 }
    | twosCompCast expArgs { ExpOp TwosCompCast $2 }
    | twosCompRep  expArgs { ExpOp TwosCompRep  $2 }

    | toIx         expArgs { ExpOp ToIx         $2 }
    | fromIx       expArgs { ExpOp FromIx       $2 }
    | ixSize       expArgs { ExpOp IxSize       $2 }
    | arrayLen     expArgs { ExpOp ArrayLen     $2 }
    | sizeOf       expArgs { ExpOp SizeOf       $2 }
    | nullPtr      expArgs { ExpOp NullPtr      $2 }
    | refToPtr     expArgs { ExpOp RefToPtr     $2 }
    | toCArray     expArgs { ExpOp ToCArray     $2 }

----------------------------------------
-- Types

typeDef :: { TypeDef }
typeDef :
  ty tyident '=' type ';' { TypeDef $2 $4 }

type :: { Type }
type :
    simpleCType      { $1 }
  | cType            { $1 }
  | tyident          { TySynonym $1 }
  | '(' type ')'     { $2 }

-- C-style types

simpleCType :: { Type }
simpleCType :
    bool                      { TyBool }
  | char                      { TyChar }
  | float                     { TyFloat }
  | double                    { TyDouble }
  | void                      { TyVoid }

  | int8_t                    { TyInt Int8 }
  | int16_t                   { TyInt Int16 }
  | int32_t                   { TyInt Int32 }
  | int64_t                   { TyInt Int64 }

  | uint8_t                   { TyWord Word8 }
  | uint16_t                  { TyWord Word16 }
  | uint32_t                  { TyWord Word32 }
  | uint64_t                  { TyWord Word64 }

  | ix_t integer              { TyIx $2 }

cType :: { Type }
cType :
          scopeC '*' type        { TyRef      $1 $3 }
  | const scopeC '*' type        { TyConstRef $2 $4 }
  | type  '[' integer ']'        { TyArray    $1 $3 }
  | struct structName            { TyStruct   $2 }
  | '&' type                     { TyStored   $2 }

scopeC :: { Scope }
scopeC :
    S           { Stack Nothing }
  | G           { Global }
  | ident       { PolyMem (Just $1) }
  | {- empty -} { PolyMem Nothing }

typeHS :: { Type }
typeHS :
    simpleHSType     { $1 }
  | hsType           { $1 }
  | tyident          { TySynonym $1 }
  | '(' typeHS ')'     { $2 }

-- Haskell-style types
simpleHSType :: { Type }
simpleHSType :
    IBool                   { TyBool }
  | IChar                   { TyChar }
  | IFloat                  { TyFloat }
  | IDouble                 { TyDouble }
  | '(' ')'                 { TyVoid }

  | Sint8                   { TyInt Int8 }
  | Sint16                  { TyInt Int16 }
  | Sint32                  { TyInt Int32 }
  | Sint64                  { TyInt Int64 }

  | Uint8                   { TyWord Word8 }
  | Uint16                  { TyWord Word16 }
  | Uint32                  { TyWord Word32 }
  | Uint64                  { TyWord Word64 }

  | Ix integer              { TyIx $2 }

hsType :: { Type }
hsType :
    Stored   typeHS         { TyStored      $2 }
  | Struct   structName     { TyStruct      $2 }
  | Array    integer typeHS { TyArray    $3 $2 }
  | Ref      scopeHS typeHS { TyRef      $2 $3 }
  | ConstRef scopeHS typeHS { TyConstRef $2 $3 }

scopeHS :: { Scope }
scopeHS : Stack tyident { Stack (Just $2) }
        | Global        { Global }

-- Bit types
bitType :: { BitTy }
bitType :
    Bit                        { Bit }
  | Bits integer               { Bits $2 }
  | BitArray integer bitType   { BitArray $2 $3 }
  | '(' bitType ')'            { $2 }
  | tyident                    { BitTySynonym $1 }

----------------------------------------
-- Struct definitions

structDef :: { StructDef }
structDef :
    struct structName '{' fields '}' { StructDef $2 (reverse $4) }
  -- Remove parsed quotes first
  | abstract struct structName fp    { AbstractDef $3 (filter (/= '\"') $4) }
  | string struct structName integer { StringDef $3 $4 }

structName :: { String }
structName :
    tyident { $1 }
  | ident   { $1 }

field :: { Field }
field :
  -- Haskell style
    ident '::' typeHS  { Field $1 $3 }
  -- C style
  | type ident         { Field $2 $1 }

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
    '=' bdConstrs                    { BitDataDef $2 $4 (reverse $6) }

-- One or more bitdata constructors, separated by '|'
bdConstrs :: { [Constr] }
bdConstrs :
    bdConstrs '|' bdConstr { $3 : $1 }
  | bdConstr               { [$1] }

bdConstr :: { Constr }
bdConstr : ident bdRecord bdLayout { Constr $1 $2 $3 }

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
    ident '::' bitType { BitField (Just $1) $3 }
  | '_'   '::' bitType { BitField Nothing   $3 }

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
    ident      { LayoutField $1 }
  | integer    { LayoutConst (BitLitUnknown $1) }
  | bitLiteral { LayoutConst $1 }

-- Parse n-bit natural, e.g.,
--
-- 8b0 -- 8 0-bits
--
-- 2b01 -- 01
-- First field is width, second is "b[0,1]+"
bitLiteral :: { BitLiteral }
bitLiteral : bitlit { BitLitKnown (fst $1) (snd $1) }

--------------------------------------------------------------------------------
-- Namespaces

ident :: { String }
ident :
    identifier                  { $1 }
  | tyidentifier '.' identifier { $1 ++ '.':$3 }

tyident :: { String }
tyident :
    tyidentifier                  { $1 }
  | tyidentifier '.' tyidentifier { $1 ++ '.':$3 }

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

runParser :: String -> [GlobalSym]
runParser s = case runAlex s ivoryParser of
  Left err    -> error err
  Right procs -> procs

-- XXX testing

parseFileTest :: FilePath -> IO (Either String [GlobalSym])
parseFileTest fp = do
  cs <- readFile fp
  return (parseTest cs)
  where
--  parseTest :: String -> Either String Type
  parseTest s = runAlex s ivoryParser

}
