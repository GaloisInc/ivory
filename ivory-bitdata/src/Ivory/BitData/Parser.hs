--
-- Parser.hs --- HW quasiquoter parser.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BitData.Parser (
  parseDefs, parseBitLiteral
) where

import Control.Applicative
import Control.Monad (when)
import Data.Char (isUpper, isLower, toLower, digitToInt)
import Data.Maybe (listToMaybe)
import Numeric (readDec, readHex, readInt)
import Text.Parsec ((<?>), chainl1, many1, sepBy, sepBy1, eof,
                    oneOf, option, digit, hexDigit,
                    unexpected, notFollowedBy, letter, alphaNum)
import Text.Parsec.String (Parser)

import Ivory.BitData.AST
import Ivory.BitData.TokenParser

--
-- Bit Literal Syntax
-- ------------------
--
-- We generalize the C syntax for hexadecimal numbers.
--
-- An n-bit binary natural number literal:
--
--   <n>[bB]{0,1}+
--
-- An n-bit decimal natural number literal:
--
--   <n>[dD]{0..9}+
--
-- An n-bit hexadecimal natural number literal:
--
--   <n>[xX]{0..9a..fA..F}+
--

-- | Convert a "ReadS" style parser to a Parsec parser.
liftReadS :: ReadS a -> String -> Parser a
liftReadS f = maybe (unexpected "no parse") (return . fst) .
              listToMaybe . filter (null . snd) . f

-- | Parse a binary digit.
binDigit :: Parser Char
binDigit = oneOf "01"

-- | Convert a string containing a base 2 number to a number.
readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (`elem` "01") digitToInt

-- | Parse a list of digits given the bit literal base character.
digitParser :: Char -> Parser Int
digitParser 'b' = many1 binDigit >>= liftReadS readBin
digitParser 'd' = many1 digit    >>= liftReadS readDec
digitParser 'x' = many1 hexDigit >>= liftReadS readHex
digitParser _   = fail "invalid bit literal base character"

-- | Parse the start of a bit literal (see description above).
bitLiteral :: Parser BitLiteral
bitLiteral = (lexeme $ do
  size <- many1 digit >>= liftReadS readDec
  (bitLiteralTail size <|> return (BitLitUnknown size))
    <* notFollowedBy alphaNum) <?> "bit literal"

-- | Parse the optional tail of a bit literal.
bitLiteralTail :: Int -> Parser BitLiteral
bitLiteralTail size = do
  -- The use of "letter" rather than "oneOf" here is a little subtle.
  --
  -- We don't want the parser to fail without consuming input if there
  -- is a bad base character, because then the "BitLitUnknown"
  -- alternative in the caller will be used.  We want to make sure we
  -- generate the right error in "digitParser", so accept any letter
  -- even if we know it's wrong at this time.
  baseChar <- toLower <$> letter -- oneOf "bBdDxX"
  value    <- digitParser baseChar
  if size == 0
    then return $ BitLitUnknown value
    else do
      when (value >= 2 ^ size) $
        fail "bit literal out of range"
      return $ BitLitKnown size value

-- | Standalone parser for a bit literal.
parseBitLiteral :: Parser BitLiteral
parseBitLiteral = whiteSpace *> bitLiteral <* eof

-- | Parse a Haskell type or type constructor name.
typeName :: Parser String
typeName = do
  x <- identifier
  if not (isUpper (head x))
    then fail "expecting type or type constructor name"
    else return x

-- | Parse a Haskell value name or "_".
valueName :: Parser String
valueName = valueIdentifier <|> (symbol "_")
  where valueIdentifier = do
          x <- identifier
          if not (isLower (head x))
             then fail "expecting value name"
             else return x

-- | Parse a type (except application).
parseType1 :: Parser BitTy
parseType1 = numTyLit <|> conT <|> parens parseType
  where numTyLit = TyNat <$> natural
        conT     = TyCon <$> typeName

-- | Parse a type that can appear in a bitdata definition.
parseType :: Parser BitTy
parseType = parseType1 `chainl1` (whiteSpace >> return TyApp)

-- | Parse a set of bit data definitions.  This is the top-level
-- parser that is called from the quasiquoter.
parseDefs :: Parser [Def]
parseDefs = whiteSpace *> some parseDef <* eof

-- | Parse a single bit data definition.
parseDef :: Parser Def
parseDef =
  Def <$  symbol "bitdata"
      <*> typeName
      <*  reservedOp "::"
      <*> parseType
      <*  reservedOp "="
      <*> sepBy1 parseConstr (reservedOp "|")

-- | Parse one constructor of a bit data definition.
parseConstr :: Parser Constr
parseConstr =
  Constr <$> valueName
         <*> parseFields
         <*> parseLayout

-- | Parse zero or more fields in a bit data constructor.
parseFields :: Parser [Field]
parseFields = option [] (braces (sepBy parseField comma))

-- | Parse an optional "as" clause for a bit data constructor.
parseLayout :: Parser Layout
parseLayout = option [] (symbol "as" *> body)
  where body = sepBy1 item (reservedOp "#")
        item =  LayoutConst <$> bitLiteral
            <|> LayoutField <$> valueName

-- | Parse one bit data field in a bit data constructor.
parseField :: Parser Field
parseField =
  Field <$> valueName
        <*  reservedOp "::"
        <*> parseType
