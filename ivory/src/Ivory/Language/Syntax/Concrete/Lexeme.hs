--
-- Ivory concrete lexemes.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.Lexeme where

import Numeric (readInt)
import Data.Char (digitToInt)

import Ivory.Language.Syntax.Concrete.Location

--------------------------------------------------------------------------------

type Lexeme = Located Token

-- | Token types
data Token =
    TokInteger Integer
  | TokFloat Rational -- represents floats and doubles
  | TokString String
  | TokHex Integer
  | TokBitLit (Integer, Integer) -- width, value (e.g., 5b0101)
  | TokIdent String
  | TokTyIdent String
  | TokReserved String
  | TokSym String
  | TokBrack String
  | TokSep String
  | TokEOF
  | TokError String
  deriving (Show, Read, Eq)

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




