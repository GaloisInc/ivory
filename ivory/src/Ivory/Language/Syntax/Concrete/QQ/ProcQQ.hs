{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Ivory procedure quasiquoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.ProcQQ where

import           Prelude hiding (exp, init)
import qualified Prelude as P

import           Control.Monad

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH       as T
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language as I

import Ivory.Language.Syntax.Concrete.ParseAST

import Ivory.Language.Syntax.Concrete.QQ.Common
import Ivory.Language.Syntax.Concrete.QQ.StmtQQ
import Ivory.Language.Syntax.Concrete.QQ.ExprQQ
import Ivory.Language.Syntax.Concrete.QQ.TypeQQ
import Ivory.Language.Syntax.Concrete.QQ.Types

--------------------------------------------------------------------------------

-- We use a state monad over the Q monad to keep track of expressions in the
-- parsed language that we'll turn into checkStored calls in Ivory.
type TCondM a = QStM T.Exp a

--------------------------------------------------------------------------------

-- | Turn our proc AST value into a Haskell type declaration and definition.
fromProc :: ProcDef -> Q [Dec]
fromProc pd@(ProcDef _ procName args body prePosts) = do
  ty <- fromProcType pd
  pb <- procBody
  let imp = ValD (VarP $ mkName procName)
                 (NormalB pb)
                 []
  return [ty, imp]

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

mkPrePostConds :: [PrePost] -> T.Exp -> Q T.Exp
mkPrePostConds conds bd = do
  exps <- mapM mkPrePostCond conds
  return (foldl go bd exps)

  where
  go :: T.Exp -> T.Exp -> T.Exp
  go e0 e1 = AppE e0 e1

  mkPrePostCond :: PrePost -> Q T.Exp
  mkPrePostCond cond = case cond of
    PreCond exp  -> runExp exp
    PostCond exp -> runExp exp

  runExp :: Exp -> Q T.Exp
  runExp exp = undefined --runToSt (fromExp exp)

--------------------------------------------------------------------------------

-- Insert dereferences (checkStored)

-- Collect up dereference expressions, which turn into checkStored calls.  We
-- only need one dereference statement for each unique dereferenced equation.
fromExp :: Exp -> TCondM T.Exp
fromExp exp = do
  env <- mkConds exp
  return (toExp env exp)

mkConds :: Exp -> TCondM DerefVarEnv
mkConds exp = do
  envs <- mapM insertConds (collectRefExps exp)
  return (concat envs)

-- For each unique expression that requires a dereference, insert a dereference
-- statement.
insertConds :: DerefExp -> TCondM DerefVarEnv
insertConds dv = case dv of
  RefExp var    -> do
    nm <- liftQ (newName var)
    insert $ undefined -- BindS (VarP nm) (AppE (VarE 'I.deref) (nmVar var))
    return [(dv, nm)]
  RefArrIxExp arr ixExp -> do
    env <- mkConds ixExp
    let e = toExp env ixExp
    nm <- liftQ (newName arr)
    let arrIx = InfixE (Just (nmVar arr)) (VarE '(I.!)) (Just e)
    insert $ undefined --BindS (VarP nm) (AppE (VarE 'I.deref) arrIx)
    return ((dv, nm) : env)
  where
  nmVar = VarE . mkName
