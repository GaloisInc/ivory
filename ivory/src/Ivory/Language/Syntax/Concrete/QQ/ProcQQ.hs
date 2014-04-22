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
    (e, memAccess) <- runToQ (fromExp exp)
    case memAccess of
      [] -> return e

-- error $ "XXX Finish rep-post conditions!"  --runToSt (fromExp exp)

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

-- For each unique expression that requires a dereference, insert a dereference
-- statement.
insertCheckStored :: DerefExp -> TCondM DerefVarEnv
insertCheckStored dv = return [] --undefined -- case dv of
  -- RefExp var    -> do
  --   nm <- liftQ (newName var)
  --   insertDeref nm (VarE (mkName var))
  --   return [(dv, nm)]
  -- RefArrIxExp ref ixExp -> do
  --   env <- mkDerefStmts ixExp
  --   nm <- liftQ (newName ref)
  --   insertDeref nm (toArrIxExp env ref ixExp)
  --   return ((dv, nm) : env)
  -- RefFieldExp ref fieldNm -> do
  --   nm <- liftQ (newName ref)
  --   insertDeref nm (toFieldExp ref fieldNm)
  --   return [(dv, nm)]
  -- where
  -- insertDeref nm exp = insert $ BindS (VarP nm) (AppE (VarE 'I.deref) exp)
