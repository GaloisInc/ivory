-- # -*- mode: haskell -*-
{

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

--
-- Ivory lexer.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--
-- Lexer.hs is generated!

module Ivory.Language.Syntax.Concrete.Lexer where

import Prelude hiding (lex, id)
import Data.Char (digitToInt)
import Numeric (readInt)


}

%wrapper "monad"

--------------------------------------------------------------------------------

$digit       = 0-9
$hexdig      = [0-9A-Fa-f]
$alpha       = [a-zA-Z]
$lowerletter = [a-z]
$capletter   = [A-Z]

@sym         = [\/ \* \+ \- \= \< \> \! \% \| \& \^ \~ \? \: \# \_ \. \$ \@]+
@tyident     = $capletter   [$alpha $digit [_ \']]*
@ident       = $lowerletter [$alpha $digit [_ \']]*
@brack       = [\( \) \[ \] \{ \}]
@sep         = [\, \;]
@filepath    = \" [$printable # \" # $white]+ \"
@bitlit      = $digit+ b [0 1]+
@hexlit      = 0x $hexdig+

--------------------------------------------------------------------------------

tokens :-
  $white+ ;
  "--".*  ;

  $digit+      { lex (TokInteger . read) }
  @hexlit      { lex (TokInteger . read) }
  @bitlit      { lex readBitLit }

-- Reserved words: statements
  if       { lexReserved }
  else     { lexReserved }
  assert   { lexReserved }
  assume   { lexReserved }
  pre      { lexReserved }
  post     { lexReserved }
  let      { lexReserved }
  return   { lexReserved }
  alloc    { lexReserved }
  store    { lexReserved }
  as       { lexReserved }
  map      { lexReserved }
  upTo     { lexReserved }
  forever  { lexReserved }

-- Reserved words: expressions
  abs              { lexReserved }
  signum           { lexReserved }
  exp              { lexReserved }
  sqrt             { lexReserved }
  log              { lexReserved }
  pow              { lexReserved }

  sin              { lexReserved }
  cos              { lexReserved }
  tan              { lexReserved }

  asin             { lexReserved }
  acos             { lexReserved }
  atan             { lexReserved }

  sinh             { lexReserved }
  cosh             { lexReserved }
  tanh             { lexReserved }

  asinh            { lexReserved }
  acosh            { lexReserved }
  atanh            { lexReserved }

  isnan            { lexReserved }
  isinf            { lexReserved }
  round            { lexReserved }
  ceil             { lexReserved }
  floor            { lexReserved }
  const            { lexReserved }

  memcpy           { lexReserved }

  safeCast         { lexReserved }
  bitCast          { lexReserved }
  castWith         { lexReserved }
  twosCompCast     { lexReserved }
  twosCompRep      { lexReserved }

  fromIx           { lexReserved }
  ixSize           { lexReserved }
  toIx             { lexReserved }
  toCArray         { lexReserved }
  arrayLen         { lexReserved }

  constRef         { lexReserved }
  sizeOf           { lexReserved }
  nullPtr          { lexReserved }
  refToPtr         { lexReserved }

-- Reserved words
  struct   { lexReserved }
  abstract { lexReserved }
  string   { lexReserved }
  type     { lexReserved }
  include  { lexReserved }

  -- C style
  bool     { lexReserved }
  char     { lexReserved }
  float    { lexReserved }
  double   { lexReserved }
  void     { lexReserved }

  int8_t   { lexReserved }
  int16_t  { lexReserved }
  int32_t  { lexReserved }
  int64_t  { lexReserved }

  uint8_t  { lexReserved }
  uint16_t { lexReserved }
  uint32_t { lexReserved }
  uint64_t { lexReserved }

  S        { lexReserved }
  G        { lexReserved }

  -- Haskell style
  IBool    { lexReserved }
  IChar    { lexReserved }
  IFloat   { lexReserved }
  IDouble  { lexReserved }

  Sint8    { lexReserved }
  Sint16   { lexReserved }
  Sint32   { lexReserved }
  Sint64   { lexReserved }

  Uint8    { lexReserved }
  Uint16   { lexReserved }
  Uint32   { lexReserved }
  Uint64   { lexReserved }

  Ref      { lexReserved }
  ConstRef { lexReserved }
  Array    { lexReserved }
  Struct   { lexReserved }
  Stored   { lexReserved }

  Ix       { lexReserved }

  Stack    { lexReserved }
  Global   { lexReserved }

  -- Bit data
  bitdata  { lexReserved }
  Bit      { lexReserved }
  Bits     { lexReserved }
  BitArray { lexReserved }

-- Identifiers
  @ident    { lex TokIdent }
-- Type Identifiers
  @tyident  { lex TokTyIdent }
-- Symbols (match if it's not a reserved word)
  @sym      { lex TokSym }
-- Brackets
  @brack    { lex TokBrack }
-- Separators
  @sep      { lex TokSep }
-- Filepath
  @filepath { lex TokFilePath }

--------------------------------------------------------------------------------

{ -- Haskell code below

alexEOF :: Alex Token
alexEOF = return TokEOF

lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

lexReserved :: AlexAction Token
lexReserved = lex TokReserved

readBitLit :: String -> Token
readBitLit s =
  let (width, val) = break (== 'b') s in
  TokBitLit (read width, readBin (tail val))

-- If Alex calls readBin, a lex error should be impossible.
readBin :: (Show a, Eq a, Num a) => String -> a
readBin s =
  case readInt 2 (`elem` "01") digitToInt s of
    [(v,"")] -> v
    ls       -> error $ "Impossible lex error on " ++ show ls

-- Token types:
data Token =
    TokInteger Integer
  | TokHex Integer
  | TokBitLit (Integer, Integer) -- width, value (e.g., 5b0101)
  | TokIdent String
  | TokTyIdent String
  | TokReserved String
  | TokSym String
  | TokBrack String
  | TokSep String
  | TokFilePath String
  | TokEOF
  deriving (Show, Read, Eq)

}

