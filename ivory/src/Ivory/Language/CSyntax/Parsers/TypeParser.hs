--
-- Type parsers.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.Parsers.TypeParser where

import           Text.Parsec

import           Control.Applicative hiding ((<|>), many)

import           Ivory.Language.CSyntax.Parsers.Common
import           Ivory.Language.CSyntax.ParseAST
import qualified Ivory.Language.CSyntax.TokenParser as T

--------------------------------------------------------------------------------
-- Helpers

go :: String -> a -> P a
go tyStr tyConstr = try (T.symbol tyStr *> pure tyConstr)

--------------------------------------------------------------------------------

intSzP :: P IntSize
intSzP = go "int8_t"  Int8
     <|> go "int16_t" Int16
     <|> go "int32_t" Int32
     <|> go "int64_t" Int64

uintSzP :: P WordSize
uintSzP = go "uint8_t" Word8
      <|> go "uint16_t" Word16
      <|> go "uint32_t" Word32
      <|> go "uint64_t" Word64

-- | Parse a memory area designator: "g *" (global), "s *" (stack), "*"
-- (either).
memAreaP :: P MemArea
memAreaP = try (T.symbol "s" *> go "*" Stack)
       <|> try (T.symbol "g" *> go "*" Global)
       <|> go "*" PolyMem
       <?> noParse "memArea parser"

-- | reference type parser.
refTyP :: P Type
refTyP = TyRef <$> memAreaP <*> baseTypeP

-- -- | Array type parser.
-- arrTyP :: P Type
-- arrTyP = TyArr <$> T.integer <*> baseTypeP

-- -- | Struct type parser.
-- structTyP :: P Type
-- structTyP = TyStruct <$> T.identifier

-- | Base (non-reference) types.
baseTypeP :: P Type
baseTypeP =
     go "void" TyVoid
 <|> liftTy TyInt intSzP
 <|> liftTy TyWord uintSzP
 <|> go "bool" TyBool
 <|> go "char" TyChar
 <|> go "float" TyFloat
 <|> go "double" TyDouble
 -- <|> try arrTyP
 -- <|> try structTyP
 <?> noParse "baseTypes"
  where
  liftTy constr p = try (constr <$> p)

-- | Type parser.
typeP :: P Type
typeP = baseTypeP
  <|> try refTyP
  <?> noParse "type parser"
