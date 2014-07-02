{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Fold over expressions that collect up assertions about the expressions.

module Ivory.Opts.AssertFold
  ( procFold
  , expFoldDefault
  , insert
  , FolderStmt()
  , freshVar
  ) where

import           MonadLib hiding (collect)
import           Control.Applicative
import           Data.Monoid
import qualified Data.DList as D
import           Ivory.Opts.Utils
import qualified Ivory.Language.Syntax.AST   as I
import qualified Ivory.Language.Syntax.Type  as I

--------------------------------------------------------------------------------
-- Monad and types.

data St a = St
  { dlst :: D.DList a -- ^ State (statements)
  , int  :: Integer   -- ^ Counter for fresh names
  , pass :: String    -- ^ Base for fresh names
  } deriving (Show, Read, Eq)

-- | A monad that holds our transformed program.
newtype FolderM a b = FolderM
  { unFolderM :: StateT (St a) Id b
  } deriving (Functor, Monad, Applicative)

instance StateM (FolderM a) (St a) where
  get = FolderM get
  set = FolderM . set

extract :: FolderM a (D.DList a)
extract = do
  st <- get
  return (dlst st)

insert :: a -> FolderM a ()
insert a = do
  st <- get
  set $ st { dlst = D.snoc (dlst st) a }

inserts :: D.DList a -> FolderM a ()
inserts ds = do
  st <- get
  set $ st { dlst = dlst st <++> ds }

runFolderM :: String -> FolderM a b -> D.DList a
runFolderM ps m =
  dlst $ snd $ runId $ runStateT st (unFolderM m)
  where
  st = St D.empty 0 ps

resetSt :: FolderM a ()
resetSt = do
  st <- get
  set st { dlst = D.empty }

-- | Create a fresh variable and update the variable store.
freshVar :: FolderM a String
freshVar = do
  st <- get
  let i = int st
  set st { int = i + 1 }
  return (pass st ++ show i)

--------------------------------------------------------------------------------
-- Specialization for statements

type FolderStmt a = FolderM I.Stmt a

-- Return a list of assertions from an expression's subexpressions.
type ExpFold = I.Type -> I.Expr -> FolderStmt ()

runEmptyState :: String -> ExpFold -> [I.Stmt] -> [I.Stmt]
runEmptyState ps ef stmts =
  let m = mapM_ (stmtFold ef) stmts in
  D.toList (runFolderM ps m)

-- | Run with fresh statements, returning them, but reseting the statement
-- state.
runFreshStmts :: ExpFold -> [I.Stmt] -> FolderStmt [I.Stmt]
runFreshStmts ef stmts = do
  st <- get
  set st { dlst = D.empty }
  mapM_ (stmtFold ef) stmts
  st' <- get
  set st' { dlst = dlst st, int = int st' }
  return (D.toList (dlst st'))

-- | Update a procedure's statements with its compiler assertions (and local
-- variable declarations, as needed).
procFold :: String -> ExpFold -> I.Proc -> I.Proc
procFold ps ef p =
  let body' = runEmptyState ps ef (I.procBody p) in
  p { I.procBody = body' }

--------------------------------------------------------------------------------

stmtFold :: ExpFold -> I.Stmt -> FolderStmt ()
stmtFold ef stmt = case stmt of
  I.IfTE e b0 b1                  -> do ef I.TyBool e
                                        b0' <- runFreshStmts ef b0
                                        b1' <- runFreshStmts ef b1
                                        insert (I.IfTE e b0' b1')
  I.Assert e                      -> do ef I.TyBool e
                                        insert stmt
  I.CompilerAssert e              -> do ef I.TyBool e
                                        insert stmt
  I.Assume e                      -> do ef I.TyBool e
                                        insert stmt
  I.Return (I.Typed ty e)         -> do ef ty e
                                        insert stmt
  I.ReturnVoid                    -> insert stmt
  I.Deref ty _v e                 -> do ef ty e
                                        insert stmt
  I.Store ty ptrExp e             -> do ef (I.TyRef ty) ptrExp
                                        ef ty e
                                        insert stmt
  I.Assign ty _v e                -> do ef ty e
                                        insert stmt
  I.Call _ty _mv _nm args         -> do mapM_ efTyped args
                                        insert stmt
  I.Loop v e incr blk             -> do ef (I.TyInt I.Int32) e
                                        efIncr incr
                                        blk' <- runFreshStmts ef blk
                                        insert (I.Loop v e incr blk')
  I.Break                         -> insert stmt
  I.Local _ty _v init'            -> do efInit init'
                                        insert stmt
  I.RefCopy ty e0 e1              -> do ef ty e0
                                        ef ty e1
                                        insert stmt
  I.AllocRef{}                    -> insert stmt
  I.Forever blk                   -> do blk' <- runFreshStmts ef blk
                                        insert (I.Forever blk')
  I.Comment _                     -> insert stmt
  where
  efTyped (I.Typed ty e) = ef ty e
  efIncr incr = case incr of
    I.IncrTo e -> ef ty e
    I.DecrTo e -> ef ty e
    where ty = I.TyInt I.Int32
  efInit init' = case init' of
    I.InitZero          -> return ()
    I.InitExpr ty e     -> ef ty e
    I.InitStruct inits  -> mapM_ (efInit . snd) inits
    I.InitArray inits   -> mapM_ efInit inits

--------------------------------------------------------------------------------

expFoldDefault :: ExpFold ->  I.Type -> I.Expr -> FolderStmt ()
expFoldDefault asserter ty e = case e of
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
  go = asserter ty
  expFold = expFoldDefault asserter

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
-- Otherwise, map over the expression with the asserter.
expFoldOps :: ExpFold -> I.Type -> (I.ExpOp, [I.Expr]) -> FolderStmt ()
expFoldOps asserter ty (op, args) = case (op, args) of

  (I.ExpCond, [cond, texp, fexp])
    -> do
    fold ty cond
    preSt <- extract

    tSt <- runBranch cond texp
    fSt <- runBranch (neg cond) fexp
    resetSt

    inserts preSt
    inserts tSt
    inserts fSt

  (I.ExpAnd, [exp0, exp1])
    -> runBool exp0 exp1 id

  (I.ExpOr, [exp0, exp1])
    -> runBool exp0 exp1 neg

  _ -> mapM_ (fold $ expOpType ty op) args

  where
  fold = expFoldDefault asserter

  runBranch cond e = do
    resetSt
    expFoldDefault (withCond cond asserter) ty e
    extract

  runBool exp0 exp1 f = do
    preSt <- extract

    st0 <- runCase exp0
    st1 <- runBranch (f exp0) exp1
    resetSt

    inserts preSt
    inserts st0
    inserts st1

    where
    runCase e = do
      resetSt
      fold ty e
      extract

--------------------------------------------------------------------------------
-- Helpers

(<++>) :: Monoid a => a -> a -> a
a <++> b = a `mappend` b

infixr 0 ==>
(==>) :: I.Expr -> I.Expr -> I.Expr
(==>) e0 e1 = I.ExpOp I.ExpOr [neg e0, e1]

neg :: I.Expr -> I.Expr
neg e = I.ExpOp I.ExpNot [e]

-- | Modify the ExpFold function to take a precondition.
withCond :: I.Expr -> ExpFold -> ExpFold
withCond cond f ty e = f ty (cond ==> e)

--------------------------------------------------------------------------------
