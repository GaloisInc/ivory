{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Ivory pre/post conditions quasiquoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.CondQQ where

import           Prelude hiding (exp)

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH       as T
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language.Cond as I

import Ivory.Language.Syntax.Concrete.ParseAST

import Ivory.Language.Syntax.Concrete.QQ.Common
import Ivory.Language.Syntax.Concrete.QQ.ExprQQ
import Ivory.Language.Syntax.Concrete.QQ.Types
import Ivory.Language.Syntax.Concrete.QQ.StmtQQ

--------------------------------------------------------------------------------

-- We use a state monad over the Q monad to keep track of expressions in the
-- parsed language that we'll turn into checkStored calls in Ivory.
type TCondM a = QStM (T.Exp, T.Name) a

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
    PostCond exp -> appE (varE 'I.ensures)  (lamE [varP $ mkName "return"]
                                                  (runExp exp))

  runExp :: Exp -> Q T.Exp
  runExp exp = do
    (e, derefs) <- runToQ (fromExp exp)
    return (foldr go e derefs)
    where
    go :: (T.Exp, T.Name) -> T.Exp -> T.Exp
    go (deref, nm) acc =
      AppE (AppE (VarE 'I.checkStored) deref) (LamE [VarP nm] acc)


--------------------------------------------------------------------------------

-- Insert dereferences (checkStored)
--
-- The code below is morally identical to dereference insertions in StmtQQ,
-- where we turn dereference expressions into statements (TODO: generalize), but
-- here, we're inserting lambdas in this case (for pre and post conditions),
-- e.g.,
--
-- requires (checkStored (pid ~> pid_err) (\err -> err <? 1))
-- ensures  (\res -> checkStored (pid ~> pid_err) (\err -> err <? res))

-- Collect up dereference expressions, which turn into checkStored calls.  We
-- only need one dereference statement for each unique dereferenced equation.
fromExp :: Exp -> TCondM T.Exp
fromExp exp = do
  env <- mkCheckStored exp
  return (toExp env exp)

mkCheckStored :: Exp -> TCondM DerefVarEnv
mkCheckStored exp =
  (return . concat) =<< mapM insertCheckStored (collectRefExps exp)

-- For each unique expression that requires a dereference, insert the TH to
-- place into a checkStored call.
insertCheckStored :: DerefExp -> TCondM DerefVarEnv
insertCheckStored dv = case dv of
  RefExp var    -> do
    nm <- liftQ (freshDeref var)
    mkInsert (VarE (mkName var)) nm
    return [(dv, nm)]
  RefArrIxExp ref ixExp -> do
    env <- mkCheckStored ixExp
    nm  <- liftQ (freshDeref ref)
    mkInsert (toArrIxExp env ref ixExp) nm
    return ((dv, nm) : env)
  RefFieldExp ref fieldNm -> do
    nm <- liftQ (freshDeref ref)
    mkInsert (toFieldExp ref fieldNm) nm
    return [(dv, nm)]
  where
  -- We want to generate a fresh name that won't capture other user-defined
  -- names, since we're inserting these variables.
  freshDeref = newName . ("cond_" ++)
  mkInsert mem nm = insert (mem, nm)
