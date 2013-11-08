{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NamedFieldPuns #-}

-- | Fold over expressions that collect up assertions about the expressions.

module Ivory.Opts.AssertFold where

import           MonadLib hiding (collect)
import           Data.Monoid
import qualified Data.DList as D
import           Ivory.Opts.Utils
import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------

type ExpFolder = I.Type -> I.Expr -> Assert ()

type Asserter = I.Type -> I.Expr -> Maybe I.Expr

type Exprs = D.DList I.Expr

-- XXX There used to be another field that I deprecated, so the data type is a
-- bit superfluous, but I'm leaving it in case we add something later.
newtype AssertSt = AssertSt
  { unasserts :: Exprs -- Actional assertions
  }

instance Monoid AssertSt where
  mempty = AssertSt { unasserts = D.empty }
  (AssertSt a0) `mappend` (AssertSt a1) =
    AssertSt { unasserts = a0 `D.append` a1 }

-- | Monad for accumulating expressions for asserting during evaluation.
newtype Assert a = Assert
  { unAssert :: StateT AssertSt Id a }
  deriving (Monad)

instance StateM Assert AssertSt where
  get = Assert get
  set = Assert . set

-- | Takes a 'Maybe' expression e and if writes e into the 'Assert' monad.
putExpr :: Maybe I.Expr -> Assert ()
putExpr = maybe (return ()) new
  where
  new e = do st <- get
             set (st <++> AssertSt (D.singleton e))

-- Return the current state and reset the state.
reset :: Assert AssertSt
reset = do
  st <- get
  set mempty
  return st

run :: Assert a -> (a, AssertSt)
run = runId . runStateT mempty . unAssert

--------------------------------------------------------------------------------

type ExpFold = I.Type -> I.Expr -> Assert ()

--------------------------------------------------------------------------------

procFold :: ExpFold -> I.Proc -> I.Proc
procFold ef p =
  let body' = stmtFold ef (D.fromList $ I.procBody p) in
  p { I.procBody = D.toList body' }

--------------------------------------------------------------------------------

type Stmts = D.DList I.Stmt

stmtFold :: ExpFold -> Stmts -> Stmts
stmtFold ef ss
  -- DList is empty
  | D.foldr (\_ len -> len+1) 0 ss == (0 :: Integer) = D.empty
  | otherwise     =
  case stmt of
    I.IfTE e b0 b1 ->
      let asserts = collect I.TyBool e in
      toAsserts asserts <++>
        (I.IfTE e (toFoldBlck b0) (toFoldBlck b1)
          `D.cons` stmtFold ef stmts)
    I.Assert{}              -> noAsserts
    I.Assume{}              -> noAsserts
    I.Return (I.Typed ty e) -> let asserts = collect ty e in
                               goAsserts asserts
    I.ReturnVoid            -> noAsserts
    I.Deref ty _ e          -> let asserts = collect ty e in
                               goAsserts asserts
    I.Store ty e0 e1        -> let asserts0 = collect ty e0 in
                               let asserts1 = collect ty e1 in
                               goAsserts (asserts0 <++> asserts1)
    I.Assign ty _ e         -> let asserts = collect ty e in
                               goAsserts asserts
    I.Call{}                -> noAsserts
    I.Loop v e incr b ->
      let ty       = I.TyInt I.Int32 in
      let asserts  = collect ty e in
      let asserts0 = fromIncr ty incr in
      let assts    = asserts <++> asserts0 in
      toAsserts assts <++>
        (I.Loop v e incr (toFoldBlck b)
          `D.cons` stmtFold ef stmts)
    I.Break                -> noAsserts
    I.Local{}              -> noAsserts
    I.RefCopy ty e0 e1     -> let asserts0 = collect ty e0 in
                              let asserts1 = collect ty e1 in
                              goAsserts (asserts0 <++> asserts1)
    I.AllocRef{}           -> noAsserts
    I.Forever b            -> I.Forever (toFoldBlck b) `D.cons`
                                stmtFold ef stmts

  where
  stmt  = D.head ss
  stmts = D.tail ss

  fromIncr t incr = case incr of
    I.IncrTo e0 -> cs e0
    I.DecrTo e0 -> cs e0
    where
    cs e0 = collect t e0

  collect :: I.Type -> I.Expr -> AssertSt
  collect t = snd . run . ef t

  toFoldBlck = D.toList . stmtFold ef . D.fromList

  goAsserts :: AssertSt -> Stmts
  goAsserts as = toAsserts as <++> (stmt `D.cons` stmtFold ef stmts)

  noAsserts :: Stmts
  noAsserts = goAsserts mempty

--------------------------------------------------------------------------------
-- Default expression fold.  Specific optimizations may use custom folders
-- (e.g., Index checks).

expFoldDefault :: Asserter -> ExpFolder
expFoldDefault asserter ty e = case e of
  I.ExpSym{}                     -> return ()
  I.ExpVar{}                     -> return ()
  I.ExpLit{}                     -> return ()
  I.ExpOp{}                      -> expOps expFold asserter ty e
  I.ExpLabel ty' e0 _            -> expFold ty' e0
  I.ExpIndex tIdx eIdx tArr eArr -> do expFold tIdx eIdx
                                       expFold tArr eArr
  I.ExpSafeCast ty' e0           -> expFold ty' e0
  I.ExpToIx e0 _                 -> expFold (I.TyInt I.Int32) e0
  I.ExpAddrOfGlobal{}            -> return ()
  where expFold = expFoldDefault asserter

--------------------------------------------------------------------------------

-- We need one special case for one expression, ExpCond, which really violates
-- the spirit of an expression by implementing control-flow.  We need a
-- pre-condition here, since we might have expressions like
--
-- x /= 0 ? 3.0/x : 0.0
--
expOps :: ExpFolder -> Asserter ->  ExpFolder
expOps ef mkAssert ty e = case e of
  I.ExpOp op args -> case (op, args) of
    (I.ExpCond, [cond, texp, fexp]) -> do
      ef I.TyBool cond
      preSt <- runNew cond texp
      onTSt <- runNew (neg cond) fexp
      onFSt <- get
      set (preSt <++> onTSt <++> onFSt)

    _                               -> do
      -- See if this is a mkAssert-causing expression (check-dependent)
      putExpr (mkAssert ty e)
      -- Run on subexpressions
      mapM_ (ef $ expOpType ty op) args
  _ -> error $ "Ivory opts: expOps should not be called with "
            ++ "a non-ExpCond expression!"

  where
  -- Run the monad with a fresh state over an expression, returning the original
  -- context.
  runNew :: I.Expr -> I.Expr -> Assert AssertSt
  runNew pre e' = do
    preSt <- reset
    -- Run on the branch and see if we got any asserts.
    ef ty e'
    -- If so, add the precondition.
    maybeWithEnv pre
    return preSt

--------------------------------------------------------------------------------
-- Helpers

(<++>) :: Monoid a => a -> a -> a
a <++> b = a `mappend` b

maybeWithEnv :: I.Expr -> Assert ()
maybeWithEnv pre = do
  st <- get
  let assts = unasserts st
  if null (D.toList  assts)
    -- Nothing in this branch.  Just ignore.
    then return ()
    -- Map the preconditions.
    else do let assts' = D.map (pre ==>) assts
            let st' = AssertSt assts'
            set st'

toAsserts :: AssertSt -> Stmts
toAsserts asstW = D.map I.Assert (unasserts asstW)

infixr 0 ==>
(==>) :: I.Expr -> I.Expr -> I.Expr
(==>) e0 e1 = I.ExpOp I.ExpOr [neg e0, e1]

neg :: I.Expr -> I.Expr
neg e = I.ExpOp I.ExpNot [e]

--------------------------------------------------------------------------------
