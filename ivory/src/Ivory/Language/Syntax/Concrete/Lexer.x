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

import Prelude hiding (lex)

}

%wrapper "monad"

--------------------------------------------------------------------------------

$digit       = 0-9
$alpha       = [a-zA-Z]
$lowerletter = [a-z]
$capletter   = [A-Z]

@sym         = [\/ \* \+ \- \= \< \> \! \% \| \& \^ \~ \? \:]+
-- XXX This is more general than Haskell identifiers.  Parser should check
-- context.
@ident       = $alpha [$alpha $digit [_ \']]*
@brack       = [\( \) \[ \] \{ \}]
@sep         = [\, \;]
@filepath    = \" [$printable # \" # $white]+ \"

--------------------------------------------------------------------------------

tokens :-
  $white+ ;
  "--".*  ;

  $digit+ { lex (TokInteger . read) }

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
  memcpy   { lexReserved }
  map      { lexReserved }
  forever  { lexReserved }

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

  struct   { lexReserved }
  abstract { lexReserved }

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

  Stored   { lexReserved }

  Stack    { lexReserved }
  Global   { lexReserved }

-- Identifiers
  @ident    { lex TokIdent }
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

-- Token types:
data Token =
    TokInteger Integer
  | TokIdent String
  | TokTyIdent String
  | TokReserved String
  | TokSym String
  | TokBrack String
  | TokSep String
  | TokQuote
  | TokFilePath String
  | TokEOF
  deriving (Show, Read, Eq)

}

