{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Ivory pre/post conditions quasiquoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.CondQQ where

import           Prelude hiding (exp, init)
import qualified Prelude as P

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH       as T
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language.Cond as I

import Ivory.Language.Syntax.Concrete.ParseAST

import Ivory.Language.Syntax.Concrete.QQ.Common
import Ivory.Language.Syntax.Concrete.QQ.ExprQQ
import Ivory.Language.Syntax.Concrete.QQ.Types

--------------------------------------------------------------------------------

-- We use a state monad over the Q monad to keep track of expressions in the
-- parsed language that we'll turn into checkStored calls in Ivory.
type TCondM a = QStM (T.Exp, T.Exp -> T.Exp) a

--------------------------------------------------------------------------------

mkPrePostConds :: [PrePost] -> T.Exp -> Q T.Exp
mkPrePostConds conds procBody = do
  condFns <- mapM mkCond conds
  -- Apply conditions to the proc body in the Q monad.
  return (foldr AppE procBody condFns)

  where
  mkCond :: PrePost -> Q T.Exp
  mkCond cond = case cond of
    PreCond  exp -> appE (varE 'I.requires) (runExp exp)
    PostCond exp -> appE (varE 'I.ensures)  (lamE [varP $ mkName "return"] (runExp exp))

  runExp :: Exp -> Q T.Exp
  runExp exp = do
    (e, derefs) <- runToQ (fromExp exp)
    return (foldr go e derefs)
    where
    go :: (T.Exp, T.Exp -> T.Exp) -> T.Exp -> T.Exp
    go (deref, rst) acc =
      AppE (AppE (VarE 'I.checkStored) deref) (rst acc)
    -- case derefs of
    --   [] -> return e


--------------------------------------------------------------------------------

-- Insert dereferences (checkStored)

-- Collect up dereference expressions, which turn into checkStored calls.  We
-- only need one dereference statement for each unique dereferenced equation.
fromExp :: Exp -> TCondM T.Exp
fromExp exp = do
  env <- mkCheckStored exp
  return (toExp env exp)

mkCheckStored :: Exp -> TCondM DerefVarEnv
mkCheckStored exp = do
  envs <- mapM insertCheckStored (collectRefExps exp)
  return (concat envs)

-- For each unique expression that requires a dereference, insert the TH to
-- place into a checkStored call.
insertCheckStored :: DerefExp -> TCondM DerefVarEnv
insertCheckStored dv = case dv of
  RefExp var    -> do
    nm <- liftQ (newName var)
    mkInsert (VarE (mkName var)) nm
    return [(dv, nm)]
  RefArrIxExp ref ixExp -> do
    env <- mkCheckStored ixExp
    nm  <- liftQ (newName ref)
    mkInsert (toArrIxExp env ref ixExp) nm
    return ((dv, nm) : env)
  RefFieldExp ref fieldNm -> do
    nm <- liftQ (newName ref)
    mkInsert (toFieldExp ref fieldNm) nm
    return [(dv, nm)]
  where
  mkInsert mem nm = insert (mem, LamE [VarP nm])
