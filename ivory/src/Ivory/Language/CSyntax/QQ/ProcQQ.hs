{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Ivory procedure quasiquoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.QQ.ProcQQ where

import           Prelude hiding (exp, init)
import qualified Prelude as P

import Ivory.Language.CSyntax.QQ.StmtQQ
import Ivory.Language.CSyntax.QQ.TypeQQ

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language as I

import Ivory.Language.CSyntax.ParseAST

--------------------------------------------------------------------------------

fromProc :: ProcDef -> Q [Dec]
fromProc pd@(ProcDef _ procName args body) = do
  procTy <- fromProcType pd
  pb     <- procBody
  let imp = ValD (VarP $ mkName procName)
                 (NormalB pb)
                 []
  return [procTy, imp]
  where
  args' = snd (unzip args)
  procBody  = do
    vars <- mapM newName args'
    let lams = (map VarP vars)
    prog <- fromProgram body
    let nm = AppE (VarE 'I.proc) (LitE $ StringL procName)
    let fn = LamE lams (AppE (VarE 'I.body) prog)
    return (AppE nm fn)
