-- # -*- mode: haskell -*-
{
--
-- Ivory lexer.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--
-- Lexer.hs is generated!

module Ivory.Language.CSyntax.Lexer where

import Prelude hiding (lex)

}

%wrapper "monad"

--------------------------------------------------------------------------------

$digit       = 0-9
$alpha       = [a-zA-Z]
$lowerletter = [a-z]
$capletter   = [A-Z]

@sym         = [\/ \* \+ \- \= \< \> \! \% \| \& \^ \~ \? \:]+
@ident       = [_ $lowerletter] [$alpha $digit [_ \']]*
@brack       = [\( \) \[ \] \{ \}]
@sep         = [\, \;]

--------------------------------------------------------------------------------

tokens :-
  $white+ ;
  "--".*  ;

  $digit+ { lex (TokInteger . read) }

-- Reserved words: statements
  if      { lexReserved }
  else    { lexReserved }
  assert  { lexReserved }
  assume  { lexReserved }
  let     { lexReserved }
  return  { lexReserved }
  alloc   { lexReserved }
  memcpy  { lexReserved }
  map     { lexReserved }
  forever { lexReserved }

-- Reserved words: expressions
  abs     { lexReserved }
  signum  { lexReserved }
  exp     { lexReserved }
  sqrt    { lexReserved }
  log     { lexReserved }
  pow     { lexReserved }

  sin     { lexReserved }
  cos     { lexReserved }
  tan     { lexReserved }

  asin    { lexReserved }
  acos    { lexReserved }
  atan    { lexReserved }

  sinh    { lexReserved }
  cosh    { lexReserved }
  tanh    { lexReserved }

  asinh   { lexReserved }
  acosh   { lexReserved }
  atanh   { lexReserved }

  isnan   { lexReserved }
  isinf   { lexReserved }
  round   { lexReserved }
  ceil    { lexReserved }
  floor   { lexReserved }
  const   { lexReserved }

  memcpy  { lexReserved }

-- Reserved words: types

  bool     { lexReserved }
  char     { lexReserved }
  float    { lexReserved }
  double   { lexReserved }
  void     { lexReserved }

  Bool     { lexReserved }
  Char     { lexReserved }
  Float    { lexReserved }
  Double   { lexReserved }

  int8_t   { lexReserved }
  int16_t  { lexReserved }
  int32_t  { lexReserved }
  int64_t  { lexReserved }

  Int8     { lexReserved }
  Int16    { lexReserved }
  Int32    { lexReserved }
  Int64    { lexReserved }

  uint8_t  { lexReserved }
  uint16_t { lexReserved }
  uint32_t { lexReserved }
  uint64_t { lexReserved }

  Word8    { lexReserved }
  Word16   { lexReserved }
  Word32   { lexReserved }
  Word64   { lexReserved }

  struct   { lexReserved }

  Stack    { lexReserved }
  S        { lexReserved }
  Global   { lexReserved }
  G        { lexReserved }

  Ref      { lexReserved }
  ConstRef { lexReserved }

-- Identifiers
  @ident  { lex TokIdent }
-- Symbols (match if it's not a reserved word)
  @sym    { lex TokSym }
-- Brackets
  @brack  { lex TokBrack }
-- Separators
  @sep    { lex TokSep }

--------------------------------------------------------------------------------

{ -- Haskell code below

alexEOF = return TokEOF

lex :: (String -> a) -> AlexAction a
lex f = \(_,_,_,s) i -> return (f (take i s))

lexReserved :: AlexAction Token
lexReserved = lex TokReserved

-- Token types:
data Token =
    TokInteger Integer
  | TokIdent String
  | TokReserved String
  | TokSym String
  | TokBrack String
  | TokSep String
  | TokEOF
  deriving (Show, Read, Eq)

}

