{-# LANGUAGE TemplateHaskell #-}


--
-- QuasiQuoter for Ivory statements.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.QQ
  ( c
  ) where

import Prelude hiding (exp)

import           Language.Haskell.TH       hiding (Stmt)
import qualified Language.Haskell.TH as T
import           Language.Haskell.TH.Quote

import Language.Haskell.Meta.Parse (parseExp)

import Ivory.Language

import Ivory.Language.CSyntax.Parser

--------------------------------------------------------------------------------

-- | Quasiquoter for defining Ivory statements in C-like syntax.
c :: QuasiQuoter
c = QuasiQuoter
  { quoteExp  = \str -> return . fromProgram =<< ivoryCParser str
  , quotePat  = err "quotePat"
  , quoteDec  = err "quotePat"
  , quoteType = err "quoteType"
  }
  where
  err str = error $ str ++ " not implemented for c quasiquoter."

fromProgram :: [Stmt] -> Exp
fromProgram program = DoE (map fromStmt program)

fromStmt :: Stmt -> T.Stmt
fromStmt stmt = case stmt of
  (Assign var ivoryExp) ->
    let exp = hsExp ivoryExp in
    let v   = mkName var in
    BindS (VarP v) (AppE (VarE 'assign) exp)
  (Return ivoryExp)     ->
    let exp = hsExp ivoryExp in
    NoBindS (AppE (VarE 'ret) exp)

-- | Parse a Haskell expression and die on errors.
hsExp :: String -> Exp
hsExp str = case parseExp str of
  Left  err -> error err
  Right exp -> exp
