{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

--
-- Ivory procedure quasiquoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.ProcQQ where

import           Prelude hiding (exp, init)

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language.Proc as I

import Ivory.Language.Syntax.Concrete.ParseAST

#if __GLASGOW_HASKELL__ >= 709
import Ivory.Language.Syntax.Concrete.QQ.Common
#endif
import Ivory.Language.Syntax.Concrete.QQ.CondQQ
import Ivory.Language.Syntax.Concrete.QQ.StmtQQ
import Ivory.Language.Syntax.Concrete.QQ.TypeQQ

--------------------------------------------------------------------------------

-- | Turn our proc AST value into a Haskell type declaration and definition.
fromProc :: ProcDef -> Q [Dec]
fromProc pd = case pd of
#if __GLASGOW_HASKELL__ >= 709
  ProcDef retTy procName args body prePosts srcloc -> do
    ty <- fromProcType retTy procName args
    pb <- procBody
    let imp = ValD (VarP $ mkName procName)
                   (NormalB pb)
                   []
    lnPrag <- lnPragma srcloc
    return (lnPrag ++ [ty, imp])
#else
  ProcDef retTy procName args body prePosts _srcloc -> do
    ty <- fromProcType retTy procName args
    pb <- procBody
    let imp = ValD (VarP $ mkName procName)
                   (NormalB pb)
                   []
    return [ty, imp]
#endif
    where
    args' = snd (unzip args)
    procBody = do
      let vars = map mkName args'
      let lams = map VarP vars
      prog    <- fromProgram body
      let bd   = AppE (VarE 'I.body) prog
      full    <- mkPrePostConds prePosts bd
      let nm   = AppE (VarE 'I.proc) (LitE $ StringL procName)
      return (AppE nm (LamE lams full))

-- | Turn our importProc AST value into a Haskell type declaration and
-- definition.
fromInclProc :: IncludeProc -> Q [Dec]
fromInclProc pd = case pd of
#if __GLASGOW_HASKELL__ >= 709
  IncludeProc retTy procName args (file, sym) srcloc -> do
    ty <- fromProcType retTy procName args
    lnPrag <- lnPragma srcloc
    pb <- procDef
    let imp = ValD (VarP $ mkName procName)
                   (NormalB pb)
                   []
    return (lnPrag ++ [ty, imp])
#else
  IncludeProc retTy procName args (file, sym) _srcloc -> do
    ty <- fromProcType retTy procName args
    pb <- procDef
    let imp = ValD (VarP $ mkName procName)
                   (NormalB pb)
                   []
    return [ty, imp]
#endif
    where
    procDef = do
      let nm   = AppE (VarE 'I.importProc) (LitE $ StringL sym)
      return (AppE nm (LitE $ StringL file))

