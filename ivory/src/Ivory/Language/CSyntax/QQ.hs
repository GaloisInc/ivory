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

import Ivory.Language.CSyntax.ParseAST

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
  Return exp
    -> do
    e <- fromExp exp
    insert $ NoBindS (AppE (VarE 'ret) e)
  ReturnVoid
    ->
    insert $ NoBindS (VarE 'retVoid)
  Store ptr exp
    -> do
    let p = iVar ptr
    e <- fromExp exp
    insert $ NoBindS (AppE (AppE (VarE 'store) p) e)
  Assign var exp
    -> do
    e <- fromExp exp
    let v = mkName var
    insert $ BindS (VarP v) (AppE (VarE 'assign) e)
  AllocRef alloc
    -> fromAlloc alloc

--------------------------------------------------------------------------------
-- Initializers

fromAlloc :: AllocRef -> TStmtM ()
fromAlloc alloc = case alloc of
  AllocBase ref exp
    -> do e <- fromExp exp
          let p = mkName ref
          insert $ BindS (VarP p)
                         (AppE (VarE 'local) (AppE (VarE 'ival) e))
  AllocArr arr exps
    -> do es <- mapM fromExp exps
          let mkIval = AppE (VarE 'ival)
          let init = ListE (map mkIval es)
          let p = mkName arr
          insert $ BindS (VarP p)
                         (AppE (VarE 'local) (AppE (VarE 'iarray) init))

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

toExp :: DerefVarEnv -> Exp -> T.Exp
toExp env exp = case exp of
  ExpLit lit
    -> fromLit lit
  ExpVar v
    -> VarE (mkName v)
  ExpDeref refVar
    -> lookupV (RefExp refVar)
  ExpOp op args
    -> fromOpExp env op args
  ExpArrIx arr ix
    -> lookupV (ArrIxExp arr ix)
  ExpAnti str
    -> VarE (mkName str)
  where
  lookupV de = VarE (lookupDerefVar de env)

-----------------------------------------
-- Dereference expression environment

type DerefVarEnv = [(DerefExp, Name)]

-- Returns the fresh variable that is the do-block binding from the dereference
-- statement.
lookupDerefVar :: DerefExp -> DerefVarEnv -> Name
lookupDerefVar refVar env =
  case lookup refVar env of
    Nothing -> error "Internal error in lookupDerefVar"
    Just rv -> rv

-----------------------------------------
-- Insert dereference statements

-- Collect up dereference expressions, which turn into Ivory statements.  We
-- only need one dereference statement for each unique dereferenced equation.
fromExp :: Exp -> TStmtM T.Exp
fromExp exp = do
  env <- mkDerefStmts exp
  return (toExp env exp)

mkDerefStmts :: Exp -> TStmtM DerefVarEnv
mkDerefStmts exp = do
  envs <- mapM insertDerefStmt (collectRefExps exp)
  return (concat envs)

data DerefExp
  = RefExp RefVar
  | ArrIxExp RefVar Exp
  deriving (Eq, Show)

-- For each unique expression that requires a dereference, insert a dereference
-- statement.
insertDerefStmt :: DerefExp -> TStmtM DerefVarEnv
insertDerefStmt dv = case dv of
  RefExp var    -> do
    nm <- liftQ (newName var)
    insert $ BindS (VarP nm) (AppE (VarE 'deref) (nmVar var))
    return [(dv, nm)]
  ArrIxExp arr ixExp -> do
    env <- mkDerefStmts ixExp
    let e = toExp env ixExp
    nm <- liftQ (newName arr)
    let arrIx = InfixE (Just (nmVar arr)) (VarE '(!)) (Just e)
    insert $ BindS (VarP nm) (AppE (VarE 'deref) arrIx)
    return ((dv, nm) : env)
  where
  nmVar = VarE . mkName

collectRefExps :: Exp -> [DerefExp]
collectRefExps exp = nub $ case exp of
  ExpLit _           -> []
  ExpVar _           -> []
  ExpDeref refVar    -> [RefExp refVar]
  ExpOp _ args       -> concatMap collectRefExps args
  -- ix is an expression that is processed when the statement is inserted.
  ExpArrIx arr ix    -> [ArrIxExp arr ix]
  ExpAnti _          -> []

--------------------------------------------------------------------------------

-- | Parse a Ivory variable.
iVar :: String -> T.Exp
iVar = iParser

-- fromInit :: Init -> String
-- fromInit (Init init) = init

-- -- | Parse a Ivory initializer.
-- iInit :: Init -> T.Exp
-- iInit = iParser . fromInit

iParser :: String -> T.Exp
iParser str = case parseExp str of
  Left err -> error err
  Right e  -> e

--------------------------------------------------------------------------------
-- Testing

{-
dump :: QuasiQuoter
dump = QuasiQuoter
  { quoteExp  = \str -> return $ LitE (StringL str)
  }
-}
