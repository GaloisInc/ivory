{-# LANGUAGE OverloadedStrings #-}

module Ivory.Language.Struct.Parser (
    parseStructDefs

  , StructDef(..), Field(..), Type(..)
  ) where

import Control.Applicative ((<$>),(<$),(<*>),(*>),(<*),many,(<|>),some)
import Control.Monad (void)
import Data.Char (isLower,isAscii,isAlphaNum,isLetter)
import Text.Parsec.Char (char,satisfy,spaces,string,digit)
import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (try)


data StructDef
  = StructDef String [Field]
  | AbstractDef String String
  | StringDef String Integer
    deriving (Show)

data Field = Field
  { fieldName :: String
  , fieldType :: Type
  } deriving (Show)

data Type
  = TApp Type Type
  | TCon String
  | TNat Integer
  | TSym String
    deriving (Show)


parseStructDefs :: Parser [StructDef]
parseStructDefs  = comments *> some parseStructDef

token' :: Parser a -> Parser a
token' body = try body <* comments
  where

comments :: Parser ()
comments  = spaces *> void (many comment)
  where
  comment = string "--" *> many (satisfy (not . newline)) *> spaces
  newline = (== '\n')


token :: String -> Parser ()
token str = void (token' (string str))

braces :: Parser a -> Parser a
braces body = token "{" *> body <* token "}"

parens :: Parser a -> Parser a
parens body = token "(" *> body <* token ")"

semi :: Parser ()
semi  = token ";"

parseStructDef :: Parser StructDef
parseStructDef  = structDef <|> abstractDef <|> stringDef

structDef :: Parser StructDef
structDef  = StructDef <$  token "struct"
                       <*> parseName
                       <*> braces (parseField `sepBy1` semi)

abstractDef :: Parser StructDef
abstractDef  = AbstractDef <$  token "abstract"
                           <*  token "struct"
                           <*> parseName
                           <*> parseString

stringDef :: Parser StructDef
stringDef  = StringDef <$  token "string"
                       <*> parseName
                       <*> number

parseName :: Parser String
parseName  = token' ((:) <$> satisfy isLetter <*> following)

parseIdent :: Parser String
parseIdent  = token' ((:) <$> satisfy isLower <*> following)

following :: Parser String
following  = many (satisfy (\c -> isAscii c && (isAlphaNum c || c == '_')))

parseField :: Parser Field
parseField  = Field <$> parseIdent
                    <*  token "::"
                    <*> parseType

parseType :: Parser Type
parseType  = foldl1 TApp <$> some parseAType

parseAType :: Parser Type
parseAType  = (TNat <$> number)
          <|> (TCon <$> parseName)
          <|> (TSym <$> parseString)
          <|> parens parseType

number :: Parser Integer
number  = read <$> token' (some digit)

parseString :: Parser String
parseString  = char '"' *> (many (satisfy (/= '"'))) <* token' (char '"')
