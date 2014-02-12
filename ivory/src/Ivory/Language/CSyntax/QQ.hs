{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- QuasiQuoter for Ivory statements.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.QQ
  ( c
  ) where

import Prelude hiding (exp, init)

import           Language.Haskell.TH       hiding (Stmt, Exp)
import qualified Language.Haskell.TH as T
import           Language.Haskell.TH.Quote

import Language.Haskell.Meta.Parse (parseExp)

import Ivory.Language hiding (Init)
--import qualified Ivory.Language as I

import Ivory.Language.CSyntax.Parser

import           Data.List (nub)
import           Control.Monad (forM_)
import           MonadLib   (set, get)
import qualified MonadLib   as M
import           Data.Monoid
import qualified Data.DList as D

--------------------------------------------------------------------------------
-- Monad for inserting statements.  Necessary since we'll parse dereferences as
-- expressions but they become Ivory/Haskell statements.

newtype StmtM a b = StmtM
  { unStmtM :: M.StateT (D.DList a) T.Q b
  } deriving (Functor, Monad)

instance M.StateM (StmtM a) (D.DList a) where
  get = StmtM M.get
  set = StmtM . M.set

insert :: a -> StmtM a ()
insert a = do
  st <- get
  set (D.snoc st a)

-- resetState :: StmtM a ()
-- resetState = set D.empty

runToQ :: StmtM a b -> Q (b, D.DList a)
runToQ m = M.runStateT mempty (unStmtM m)

liftQ :: Q b -> StmtM a b
liftQ = StmtM . M.lift

type TStmtM a = StmtM T.Stmt a

runToStmts :: TStmtM a -> Q [T.Stmt]
runToStmts m = do
  (_, st) <- runToQ m
  return (D.toList st)

--------------------------------------------------------------------------------

-- | Quasiquoter for defining Ivory statements in C-like syntax.
c :: QuasiQuoter
c = QuasiQuoter
  { quoteExp  = \str -> ivoryCParser str >>= fromProgram
  , quotePat  = err "quotePat"
  , quoteDec  = err "quotePat"
  , quoteType = err "quoteType"
  }
  where
  err str = error $ str ++ " not implemented for c quasiquoter."

fromProgram :: [Stmt] -> Q T.Exp
fromProgram program = do
  st <- runToStmts $ forM_ program fromStmt
  return (DoE st)

fromBlock :: [Stmt] -> TStmtM T.Exp
fromBlock = liftQ . fromProgram

fromStmt :: Stmt -> TStmtM ()
fromStmt stmt = case stmt of
  IfTE cond blk0 blk1
    -> do
    cd <- fromExp cond
    b0 <- fromBlock blk0
    b1 <- fromBlock blk1
    insert $ NoBindS (AppE (AppE (AppE (VarE 'ifte_) cd) b0) b1)
  Return ivoryExp
    -> do
    exp <- fromExp ivoryExp
    insert $ NoBindS (AppE (VarE 'ret) exp)
  ReturnVoid
    ->
    insert $ NoBindS (VarE 'retVoid)
  Store ptr ivoryExp
    -> do
    let p = iVar ptr
    exp <- fromExp ivoryExp
    insert $ NoBindS (AppE (AppE (VarE 'store) p) exp)
  Assign var ivoryExp
    -> do
    exp <- fromExp ivoryExp
    let v = mkName var
    insert $ BindS (VarP v) (AppE (VarE 'assign) exp)
  AllocRef ptr ivoryInit
    -> do
    let init = iInit ivoryInit
    let p    = mkName ptr
    insert $ BindS (VarP p) (AppE (VarE 'local) init)

--------------------------------------------------------------------------------
-- Expressions

fromLit :: Literal -> T.Exp
fromLit lit = case lit of
  LitInteger int -> LitE (IntegerL int)

fromOpExp :: DerefVarEnv -> ExpOp -> [Exp] -> T.Exp
fromOpExp env op args = case op of
  AddOp -> InfixE (mkArg 0) (VarE '(+)) (mkArg 1)
  where
  mkArg i = Just $ toExp env $ args !! i

type DerefVarEnv = [(RefVar, Name)]

lookupDerefVar :: RefVar -> DerefVarEnv -> Name
lookupDerefVar refVar env =
  case lookup refVar env of
    Nothing -> error "Internal error in lookupDerefVar"
    Just rv -> rv

toExp :: DerefVarEnv -> Exp -> T.Exp
toExp env exp = case exp of
  ExpLit lit
    -> fromLit lit
  ExpVar v
    -> VarE (mkName v)
  ExpDeref refVar
    -> VarE (lookupDerefVar refVar env)
  ExpOp op args
    -> fromOpExp env op args

-----------------------------------------
-- Insert dereference statements

-- Collect up dereference expressions, which turn into Ivory statements.  We
-- only need one dereference statement for each dereferenced variable.
fromExp :: Exp -> TStmtM T.Exp
fromExp exp = do
  refVars <- collectRefVars exp
  newVars <- mapM insertDeref refVars
  let env = zip refVars newVars
  return (toExp env exp)

collectRefVars :: Exp -> TStmtM [RefVar]
collectRefVars exp = do
  refVars <- collectRefVars'
  return (nub refVars)
  where
  collectRefVars' :: TStmtM [RefVar]
  collectRefVars' = case exp of
    ExpLit _           -> return []
    ExpVar _           -> return []
    ExpDeref refVar    -> return [refVar]
    ExpOp _ args       -> do refVars <- mapM collectRefVars args
                             return (concat refVars)

insertDeref :: RefVar -> TStmtM Name
insertDeref refVar = do
  v <- liftQ (newName refVar)
  insert $ BindS (VarP v) (AppE (VarE 'deref) (VarE (mkName refVar)))
  return v

--------------------------------------------------------------------------------

  -- case exp of
  -- ExpLit lit ->
  -- LitInteger int -> undefined
  -- Deref e -> InfixE AppE (VarE '=<<) (AppE (VarE 'deref), e)
  -- Exp e   -> (id, e)

-- -- | Parse an Ivory expression.
-- iExp :: Exp -> T.Exp
-- iExp exp =
--   let (th, e) = fromExp exp in
--   th (iParser e)

-- | Parse a Ivory variable.
iVar :: String -> T.Exp
iVar = iParser

fromInit :: Init -> String
fromInit (Init init) = init

-- | Parse a Ivory initializer.
iInit :: Init -> T.Exp
iInit = iParser . fromInit

iParser :: String -> T.Exp
iParser str = case parseExp str of
  Left err -> error err
  Right e  -> e

