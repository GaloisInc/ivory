{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Fold over expressions that collect up assertions about the expressions.

module Ivory.Opts.AssertFold where

import           MonadLib hiding (collect)
import           Control.Applicative
import           Data.Monoid
import qualified Data.DList as D
import           Ivory.Opts.Utils
import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------
-- Monad and types.

-- | A monad that holds our transformed program.
newtype FolderM a b = FolderM
  { unFolderM :: StateT (D.DList a) Id b
  } deriving (Functor, Monad,Applicative)

instance StateM (FolderM a) (D.DList a) where
  get = FolderM get
  set = FolderM . set

insert :: a -> FolderM a ()
insert a = do
  st <- get
  set (D.snoc st a)

inserts :: D.DList a -> FolderM a ()
inserts ds = do
  st <- get
  set (st <++> ds)

insertList :: [a] -> FolderM a ()
insertList = inserts . D.fromList

resetState :: FolderM a ()
resetState = set D.empty

runFolderM :: FolderM a b -> (b, D.DList a)
runFolderM m = runId $ runStateT mempty (unFolderM m)

--------------------------------------------------------------------------------
-- Specialization for statements

type Stmts = D.DList I.Stmt

type FolderStmt a = FolderM I.Stmt a

-- Return a list of assertions from an expression's subexpressions.
type ExpFold = I.Type -> I.Expr -> [I.Expr]

insertAssert :: I.Expr -> FolderStmt ()
insertAssert = insert . I.CompilerAssert

insertAsserts :: [I.Expr] -> FolderStmt ()
insertAsserts = insertList . map I.CompilerAssert

--------------------------------------------------------------------------------

runEmptyState :: ExpFold -> [I.Stmt] -> [I.Stmt]
runEmptyState ef stmts =
  let m = mapM_ (stmtFold ef) stmts in
  D.toList $ snd (runFolderM m)

procFold :: ExpFold -> I.Proc -> I.Proc
procFold ef p =
  let body' = runEmptyState ef (I.procBody p) in
  p { I.procBody = body' }

--------------------------------------------------------------------------------

stmtFold :: ExpFold -> I.Stmt -> FolderStmt ()
stmtFold ef stmt = case stmt of
  I.IfTE e b0 b1                  -> do insertAsserts (ef I.TyBool e)
                                        let b0' = runEmptyState ef b0
                                        let b1' = runEmptyState ef b1
                                        insert (I.IfTE e b0' b1')
  I.Assert e                      -> do insertAsserts (ef I.TyBool e)
                                        insert stmt
  I.CompilerAssert e              -> do insertAsserts (ef I.TyBool e)
                                        insert stmt
  I.Assume e                      -> do insertAsserts (ef I.TyBool e)
                                        insert stmt
  I.Return (I.Typed ty e)         -> do insertAsserts (ef ty e)
                                        insert stmt
  I.ReturnVoid                    -> insert stmt
  I.Deref ty _v e                 -> do insertAsserts (ef ty e)
                                        insert stmt
  I.Store ty ptrExp e             -> do insertAsserts (ef (I.TyRef ty) ptrExp)
                                        insertAsserts (ef ty e)
                                        insert stmt
  I.Assign ty _v e                -> do insertAsserts (ef ty e)
                                        insert stmt
  I.Call _ty _mv _nm args         -> do insertAsserts (concatMap efTyped args)
                                        insert stmt
  I.Loop v e incr blk             -> do insertAsserts (ef (I.TyInt I.Int32) e)
                                        insertAsserts (efIncr incr)
                                        let blk' = runEmptyState ef blk
                                        insert (I.Loop v e incr blk')
  I.Break                         -> insert stmt
  I.Local _ty _v init'            -> do insertAsserts (efInit init')
                                        insert stmt
  I.RefCopy ty e0 e1              -> do insertAsserts (ef ty e0)
                                        insertAsserts (ef ty e1)
                                        insert stmt
  I.AllocRef{}                    -> insert stmt
  I.Forever blk                   -> do let blk' = runEmptyState ef blk
                                        insert (I.Forever blk')
  I.Comment _                     -> insert stmt
  where
  efTyped (I.Typed ty e) = ef ty e
  efIncr incr = case incr of
    I.IncrTo e -> ef ty e
    I.DecrTo e -> ef ty e
    where ty = I.TyInt I.Int32
  efInit init' = case init' of
    I.InitZero          -> []
    I.InitExpr ty e     -> ef ty e
    I.InitStruct inits  -> concatMap (efInit . snd) inits
    I.InitArray inits   -> concatMap efInit inits

--------------------------------------------------------------------------------
-- Specialization for expressions

type Exprs = D.DList I.Expr

type FolderExpr a = FolderM I.Expr a

-- | Default expression folder that performs the recursion for an asserter.
expFoldDefault :: ExpFold -> I.Type -> I.Expr -> [I.Expr]
expFoldDefault ef ty e =
  let (_, ds) = runFolderM (expFoldDefault' ef ty e) in
  D.toList ds

--------------------------------------------------------------------------------

expFoldDefault' :: ExpFold ->  I.Type -> I.Expr -> FolderExpr ()
expFoldDefault' asserter ty e = case e of
  I.ExpSym{}                     -> go e
  I.ExpVar{}                     -> go e
  I.ExpLit{}                     -> go e
  I.ExpLabel ty' e0 _str         -> do go e
                                       expFold ty' e0
  I.ExpIndex tIdx eIdx tArr eArr -> do go e
                                       expFold tIdx eIdx
                                       expFold tArr eArr
  I.ExpToIx e0 _i                -> do go e
                                       expFold ty e0
  I.ExpSafeCast ty' e0           -> do go e
                                       expFold ty' e0
  I.ExpOp op args                -> do go e
                                       expFoldOps asserter ty (op, args)
  I.ExpAddrOfGlobal{}            -> go e
  I.ExpMaxMin{}                  -> go e
  where
  go = insertList . asserter ty
  expFold = expFoldDefault' asserter

--------------------------------------------------------------------------------

-- We need a special case for expressions that may affect control flow:
--
--   ExpCond
--   ExpOr
--   ExpAnd
--
-- (The later two can introduce shortcutting.)  ExpCond is in the spirit of an
-- expression by implementing control-flow.  We need a pre-condition here, since
-- we might have expressions like
--
-- x /= 0 ? 3.0/x : 0.0
--
expFoldOps :: ExpFold -> I.Type -> (I.ExpOp, [I.Expr]) -> FolderExpr ()
expFoldOps asserter ty (op, args) = case (op, args) of

  (I.ExpCond, [cond, texp, fexp])
    -> do
    fold ty cond
    preSt <- get

    tSt <- runBranch cond texp
    fSt <- runBranch (neg cond) fexp
    resetState

    inserts preSt
    inserts tSt
    inserts fSt

  (I.ExpAnd, [exp0, exp1])
    -> runBool exp0 exp1 id

  (I.ExpOr, [exp0, exp1])
    -> runBool exp0 exp1 neg

  _ -> mapM_ (fold $ expOpType ty op) args

  where
  fold = expFoldDefault' asserter

  runBranch cond e = do
    resetState
    fold ty e
    withEnv cond
    get

  runBool exp0 exp1 f = do
    preSt <- get

    st0 <- runCase exp0
    st1 <- runBranch (f exp0) exp1
    resetState

    inserts preSt
    inserts st0
    inserts st1

    where
    runCase e = do
      resetState
      fold ty e
      get

--------------------------------------------------------------------------------
-- Helpers

(<++>) :: Monoid a => a -> a -> a
a <++> b = a `mappend` b

-- Add a precondition to a conditional expression.
withEnv :: I.Expr -> FolderExpr ()
withEnv pre = do
  st <- get
  let assts = D.map (pre ==>) st
  set assts

infixr 0 ==>
(==>) :: I.Expr -> I.Expr -> I.Expr
(==>) e0 e1 = I.ExpOp I.ExpOr [neg e0, e1]

neg :: I.Expr -> I.Expr
neg e = I.ExpOp I.ExpNot [e]

--------------------------------------------------------------------------------
