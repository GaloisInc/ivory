{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Fold over expressions that collect up assertions about the expressions.

module Ivory.Opts.AssertFold
  ( procFold
  , expFoldDefault
  , runFolderExpr
  , insertExpr
  , FolderExpr()
  ) where

import           MonadLib hiding (collect)
import           Control.Applicative
import           Data.Monoid
import qualified Data.DList as D
import           Ivory.Opts.Utils
import qualified Ivory.Language.Syntax.AST   as I
import qualified Ivory.Language.Syntax.Type  as I
--import qualified Ivory.Language.Syntax.Names as I

--------------------------------------------------------------------------------
-- Monad and types.  The inner monad keeps fresh local variables throughout an
-- entire procedure, while the outer stores compiler assertions.

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

insert :: a -> FolderM a ()
insert a = do
  st <- get
  set $ st { dlst = D.snoc (dlst st) a }

runFolderM :: String -> FolderM a b -> D.DList a
runFolderM ps m =
  dlst $ snd $ runId $ runStateT st (unFolderM m)
  where
  st = St D.empty 0 ps

--------------------------------------------------------------------------------
-- Specialization for statements

type FolderStmt a = FolderM I.Stmt a

-- Return a list of assertions from an expression's subexpressions.
type ExpFold = I.Type -> I.Expr -> [I.Expr]

insertAsserts :: [I.Expr] -> FolderStmt ()
insertAsserts = mapM_ mkAssert
   -- =<< mapM mkLocalsAsserts es
  where
  mkAssert :: I.Expr -> FolderStmt ()
  mkAssert = insert . I.CompilerAssert

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
  I.IfTE e b0 b1                  -> do insertAsserts (ef I.TyBool e)
                                        b0' <- runFreshStmts ef b0
                                        b1' <- runFreshStmts ef b1
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
                                        blk' <- runFreshStmts ef blk
                                        insert (I.Loop v e incr blk')
  I.Break                         -> insert stmt
  I.Local _ty _v init'            -> do insertAsserts (efInit init')
                                        insert stmt
  I.RefCopy ty e0 e1              -> do insertAsserts (ef ty e0)
                                        insertAsserts (ef ty e1)
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
    I.InitZero          -> []
    I.InitExpr ty e     -> ef ty e
    I.InitStruct inits  -> concatMap (efInit . snd) inits
    I.InitArray inits   -> concatMap efInit inits

--------------------------------------------------------------------------------
-- Expression folding
--------------------------------------------------------------------------------
-- Simple State monad for syntactic convenience for folding over expressions.

type Exprs = D.DList I.Expr

newtype FolderExpr a = FolderExpr
  { unFolderE :: StateT Exprs Id a
  } deriving (Functor, Monad, Applicative)

instance StateM (FolderExpr) Exprs where
  get = FolderExpr get
  set = FolderExpr . set

resetExpr :: FolderExpr ()
resetExpr = set D.empty

insertExpr :: I.Expr -> FolderExpr ()
insertExpr e = do
  st <- get
  set (D.snoc st e)

insertsExpr :: Exprs -> FolderExpr ()
insertsExpr ds = do
  st <- get
  set (st <++> ds)

runFolderExpr :: FolderExpr a -> Exprs
runFolderExpr m = snd $ runId $ runStateT mempty (unFolderE m)

-- | Default expression folder that performs the recursion for an asserter.
expFoldDefault :: ExpFold -> I.Type -> I.Expr -> [I.Expr]
expFoldDefault ef ty e =
  let ds = runFolderExpr (expFoldDefault' ef ty e) in
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
  go = insertsExpr . D.fromList . asserter ty
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
    resetExpr

    insertsExpr preSt
    insertsExpr tSt
    insertsExpr fSt

  (I.ExpAnd, [exp0, exp1])
    -> runBool exp0 exp1 id

  (I.ExpOr, [exp0, exp1])
    -> runBool exp0 exp1 neg

  _ -> mapM_ (fold $ expOpType ty op) args

  where
  fold = expFoldDefault' asserter

  runBranch cond e = do
    resetExpr
    fold ty e
    withEnv cond
    get

  runBool exp0 exp1 f = do
    preSt <- get

    st0 <- runCase exp0
    st1 <- runBranch (f exp0) exp1
    resetExpr

    insertsExpr preSt
    insertsExpr st0
    insertsExpr st1

    where
    runCase e = do
      resetExpr
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
