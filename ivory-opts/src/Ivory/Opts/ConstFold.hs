{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

--
-- Constant folder.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Opts.ConstFold
  ( constFold
  ) where

import           Ivory.Opts.ConstFoldComp
import qualified Ivory.Opts.HashCons as H

import qualified Ivory.Language.Syntax as I
import           Ivory.Language.Cast (toMaxSize, toMinSize)

import           Prelude hiding (init)
import           Data.Functor
import qualified Data.IntMap.Strict as I
import qualified Data.IntSet as S
import qualified Data.DList as D
import           MonadLib
import           Control.Applicative (Applicative())

--------------------------------------------------------------------------------
-- XXX testing

-- import Debug.Trace

-- import Ivory.Language
-- import Ivory.Language.Proc

-- foo :: Def ('[IBool, Uint8] :-> Uint8)
-- foo = proc "foo" $ \bb x -> body $ do
--   v <- assign 3
--   v0 <- assign 4
--   b <- assign true
--   ifte_ bb (ret 2) (ret 8)
--   ret (b ? (v,v0))

-- run =
--   case foo of
--     DefProc p -> constFold p

--------------------------------------------------------------------------------
-- Monad

type FreshIx = Int

data OptSt = OptSt
  { freshIx   :: FreshIx
  -- ^ Fresh vars for local bindings for shared expressions.
  , assignVar :: I.IntMap FreshIx
  -- ^ Keys to vars map for finding which (hash-consed) expressions are bound to
  -- which vars.
  , stmts     :: D.DList I.Stmt
  -- ^ Statements constructed from an unoptimized statement (new statements
  -- added, for example, when adding local bindings).
  } deriving (Show, Eq)

initOptSt :: OptSt
initOptSt = OptSt 0 I.empty D.empty

-- | Optimization monad.
newtype OptM m a = OptM
  { unOptM :: StateT OptSt m a
  } deriving (Functor, Applicative, Monad)

instance Monad m => StateM (OptM m) OptSt where
  get = OptM get
  set = OptM . set

instance MonadT OptM where
  lift = OptM . lift

type StmtM = OptM H.HashExprM

runStmtM :: StmtM a -> (a, I.Block)
runStmtM optM =
   let (((a,st),_),_) = H.runHashMap (runStateT initOptSt (unOptM optM)) in
   (a, D.toList (stmts st))

-- | Append (`D.snoc`) a new statement.
addStmt :: Monad m => I.Stmt -> OptM m ()
addStmt stmt = do
  s <- get
  set s { stmts = stmts s `D.snoc` stmt }

-- | Get the current statements.
getStmts :: Monad m => OptM m (D.DList I.Stmt)
getStmts = stmts <$> get

-- | Get the assignment map.
getAssignMap :: Monad m => OptM m (I.IntMap FreshIx)
getAssignMap = assignVar <$> get

-- | Create a fresh var to assign an expression. Update the set of assignments.
freshVar :: Monad m => I.Key -> OptM m FreshIx
freshVar key = do
  s <- get
  let fresh = freshIx s
  set $! s { freshIx   = fresh + 1
           , assignVar = I.insert key fresh (assignVar s)
           }
  return fresh

mkHashVar :: FreshIx -> String
mkHashVar n = "hc_" ++ show n

-- -- | Update the assignment map with a new mapping from a key to an index.
-- updateAssignMap :: Monad m => I.Key -> FreshIx -> OptM m ()
-- updateAssignMap key ix = do
--   s <- get
--   set s { assignVar = I.insert key ix (assignVar s) }

-- | Find an assignment for a key.
findAssign :: Monad m => I.Key -> OptM m (Maybe FreshIx)
findAssign key = I.lookup key <$> getAssignMap

--------------------------------------------------------------------------------
-- Hash-consing

-- | Un hash-cons an expression. This entails finding the expression in the
-- map. If the expression is non-recursive or only appears once, return
-- it. Otherwise, see if a new local binding needs to be created for it, and do
-- so recursively for subexpressions. Then replace keys in the expressions with
-- variable references.
unHash :: I.Expr -> StmtM I.Expr
unHash (I.ExpHash key) = do
  b <- lift (H.lookupSeen key)
  if b -- Is the exp used twice/recursive?
    then go
    else lift (H.flattenExp key) -- Reconstruct the original exp
  where
  mkVar = I.VarName . mkHashVar
  go = do
    mVar <- findAssign key -- Is there a var binding fo the exp?
    case mVar of
      Nothing
        -> do n <- freshVar key -- Make a new var and put it in the map.
              -- Extract the expression, recurisvely replacing assignments for
              -- hash references.
              keys <- lift (H.subKeys key)
              mapM_ unHash (map I.ExpHash keys)
              tye <- lift (H.unHash key)
              addStmt (I.Assign (I.tType tye) (mkVar n) (I.tValue tye))
              return $ I.ExpVar $ mkVar n
      Just n
        -> return $ I.ExpVar $ mkVar n
unHash e =
  error $ "Unexpected non hash-consed expression in local bind in unhash: "
       ++ show e

--------------------------------------------------------------------------------
-- Constant folding

constFold :: I.Proc -> I.Proc
constFold p =
  let cxt   = I.procSym p in
  let blk   = I.procBody p in
  let body' = snd
            $ runStmtM
            $ do ss <- lift (mapM H.hashStmt blk)
                 lift H.inlineAssigns
                 constFoldExprs
                 mapM_ (stmtFold cxt) (concat ss)
  in p { I.procBody = body' }
  where
  constFoldExprs = do
    hm1 <- lift (runConstFold optExprs)
    lift (H.newHashMap hm1)

-- | Fold over a block, collecting constant-folded statements that will replace
-- the original block.
runFreshStmts :: String -> I.Block -> StmtM [I.Stmt]
runFreshStmts cxt blk = do
  st <- get
  set st { stmts = D.empty }
  mapM_ (stmtFold cxt) blk
  st' <- get
  set st { freshIx = freshIx st' }
  return (D.toList (stmts st'))

-- | Each statement may be turned into zero or more statements after constant
-- folding. Expressions are assumed to be hash-consed and constant-folded.
stmtFold :: String -> I.Stmt -> StmtM ()
stmtFold cxt stmt = case stmt of
  I.IfTE _ [] []
    -> return ()
  I.IfTE e [] b1
    -> stmtFold cxt (I.IfTE (I.ExpOp I.ExpNot [e]) b1 [])
  I.IfTE b b0 b1
    -> case b of
         I.ExpLit (I.LitBool b')
            -> mapM_ (stmtFold cxt) (if b' then b0 else b1)
         _  -> do b'  <- unHash b
                  b0' <- runFreshStmts cxt b0
                  b1' <- runFreshStmts cxt b1
                  addStmt (I.IfTE b' b0' b1')
  I.CompilerAssert e
    -> case e of
         -- It's OK to have false but unreachable compiler asserts.
         I.ExpLit (I.LitBool b)
           | b -> return ()
         _     -> do e' <- unHash e
                     addStmt (I.CompilerAssert e')
  I.Assert e
    -> assertErr I.Assert e
  I.Assume e
    -> assertErr I.Assume e
  I.Return (I.Typed ty e)
    -> do e' <- unHash e
          addStmt (I.Return (I.Typed ty e'))
  I.ReturnVoid
    -> addStmt I.ReturnVoid
  I.Deref t var e
    -> do e' <- unHash e
          addStmt (I.Deref t var e')
  I.Store t e0 e1
    -> do e0' <- unHash e0
          e1' <- unHash e1
          addStmt $ I.Store t e0' e1'
  I.Assign t v e
    -> addStmt =<< (I.Assign t v <$> unHash e)
  I.Call t mv c tyes
    -> do let tys = map I.tType tyes
          es <- mapM (unHash . I.tValue) tyes
          let go (_,e) = I.Typed t e
          addStmt $ I.Call t mv c (map go (zip tys es))
  I.Local t var i
    -> do i' <- constFoldInits i
          addStmt (I.Local t var i')
  I.RefCopy t e0 e1
    -> do e0' <- unHash e0
          e1' <- unHash e1
          addStmt (I.RefCopy t e0' e1')
  I.AllocRef{}
    -> addStmt stmt
  I.Loop v e incr blk
    -> mkLoop cxt v e incr blk
  I.Break
    -> addStmt stmt
  I.Forever blk
    -> do blk' <- runFreshStmts cxt blk
          addStmt (I.Forever blk')
  I.Comment{}
    -> addStmt stmt
  where
  assertErr constr e =
    case e of
      I.ExpLit (I.LitBool b)
         ->
         if b then return ()
           else error $ "Constant folding evaluated a False assume()/assert()"
                      ++ " in evaluating expression " ++ show e
                      ++ " of function " ++ cxt
      _  -> do e' <- unHash e
               addStmt (constr e')

mkLoop :: String -> I.Var -> I.Expr -> I.LoopIncr -> I.Block -> OptM H.HashExprM ()
mkLoop cxt v e incr blk =
  case e of
    I.ExpLit (I.LitBool b)
      ->
      if b then error $ "Constant folding evaluated True expression "
                      ++ "in a loop bound.  The loop will never terminate!"
           else error $ "Constant folding evaluated False expression "
                      ++ "in a loop bound.  The loop will never execute!"
    _
      -> do e'    <- unHash e
            blk'  <- runFreshStmts cxt blk
            incr' <- case incr of
                       I.IncrTo i -> I.IncrTo <$> unHash i
                       I.DecrTo i -> I.DecrTo <$> unHash i
            addStmt (I.Loop v e' incr' blk')

constFoldInits :: I.Init -> StmtM I.Init
constFoldInits init = case init of
  I.InitZero
    -> return I.InitZero
  I.InitExpr ty e
    -> I.InitExpr ty <$> unHash e
  I.InitStruct inits
    -> I.InitStruct <$> mapM go inits
       where
       go (s,i) = do i' <- constFoldInits i
                     return (s,i')
  I.InitArray inits
    -> I.InitArray <$> mapM constFoldInits inits

--------------------------------------------------------------------------------
-- Constant folding

-- Define a state monad for constant folding the hash-cons map.

-- | State monad: have we processed the expression (mapped from a key) already?
newtype ConstFoldExpT m a = ConstFoldExp
  { unConstFold :: StateT S.IntSet m a }
  deriving (Functor, Applicative, Monad)

instance MonadT ConstFoldExpT where
  lift = ConstFoldExp . lift

type ConstFoldExp = ConstFoldExpT H.HashExprM

instance StateM ConstFoldExp S.IntSet where
  get = ConstFoldExp get
  set = ConstFoldExp . set

-- | Run the monad.
runConstFold :: ConstFoldExp a -> H.HashExprM a
runConstFold s =
  fst <$> runStateT S.empty (unConstFold s)
  where

seenKey :: I.Key -> ConstFoldExp ()
seenKey key = do
  st <- get
  set (key `S.insert` st)

--------------------------------------------------------------------------------

-- | Constant folding over expressions. Runs through the hash-cons map.
optExprs :: ConstFoldExp H.HashMap
optExprs = do
  allkeys <- lift H.getKeys
  mapM_ optKey allkeys
  return =<< lift H.getHashMap

-- | Optimize an expression in the map, given a key. Replace the expression with
-- its optimized version. Record that the expression has been visited.
optKey :: I.Key -> ConstFoldExp I.Expr
optKey k = do
  optkeys <- get
  tye <- lift (H.unHash k)
  let e = I.tValue tye
  if k `S.member` optkeys
    then return e
    else do e' <- runCf k tye
            seenKey k -- mark as seen
            lift (H.forceHashKey k (I.Typed (I.tType tye) e'))
            return e'

-- | Run the actual constant folding. Returns either a @ExpHash key@ or an
-- true expression.
runCf :: I.Key -> I.Typed I.Expr -> ConstFoldExp I.Expr
runCf k tye =
  go (I.tValue tye)
  where
  go :: I.Expr -> ConstFoldExp I.Expr
  go e = case e of
    I.ExpSym{}
      -> return (H.keyToExp k)
    I.ExpVar v
      -> lift (H.findCopy v e)
    I.ExpLit{}
      -> return e
    -- For expressions of the form @3 + (b ? (x,y))@, lift the conditional to
    -- see if we can optimize.
    I.ExpOp op args
      -> do args' <- mapM go args
            liftChoice (I.tType tye) op args'
    I.ExpLabel t e0 s
      -> do e0' <- go e0
            return (I.ExpLabel t e0' s)
    I.ExpIndex t e0 t1 e1
      -> do e0' <- go e0
            e1' <- go e1
            return (I.ExpIndex t e0' t1 e1')
    I.ExpSafeCast t e0
      -> do e0' <- go e0
            case destLit e0' of
              Just _  -> return e0'
              Nothing -> return (I.ExpSafeCast t e0')
    I.ExpToIx e0 maxSz
      -> do e0' <- go e0
            case destIntegerLit e0' of
              Just i  -> return $ I.ExpLit $ I.LitInteger $ i `mod` maxSz
              Nothing -> return (I.ExpToIx e0' maxSz)
    I.ExpAddrOfGlobal{}
      -> return (H.keyToExp k)
    I.ExpMaxMin{}
      -> return (H.keyToExp k)
    I.ExpHash key
      -> optKey key

arg0 :: [a] -> a
arg0 = flip (!!) 0

arg1 :: [a] -> a
arg1 = flip (!!) 1

arg2 :: [a] -> a
arg2 = flip (!!) 2

-- | Reconstruct an operator, folding away operations when possible. Args are
-- already optimized.
cfOp :: I.Type -> I.ExpOp -> [I.Expr] -> ConstFoldExp I.Expr
cfOp ty op args = case op of
  I.ExpEq    t -> cfargs t
  I.ExpNeq   t -> cfargs t
  I.ExpGt _  t -> cfargs t
  I.ExpLt _  t -> cfargs t
  I.ExpIsNan t -> cfargs t
  I.ExpIsInf t -> cfargs t
  I.ExpCond    -> do let (cond, rest) = splitAt 1 args
                     let c  = mkCfArgs I.TyBool cond
                     let as = mkCfArgs ty rest
                     return (cfOp' ty op (c ++ as))
  _            -> cfargs ty
  where
  cfargs ty' = do
    let args' = mkCfArgs ty' args
    return (cfOp' ty op args')

-- Already constant-folded arguments passed here.
cfOp' :: I.Type -> I.ExpOp -> [CfVal] -> I.Expr
cfOp' ty op args = case op of
  I.ExpEq _  -> cfOrd
  I.ExpNeq _ -> cfOrd
  I.ExpCond
    | CfBool b <- arg0 args
    -> if b then a1 else a2
    -- If either branch is a boolean literal, reduce to logical AND or OR.
    | ty == I.TyBool && arg1 args == CfBool True
    -> cfOp' ty I.ExpOr [arg0 args, arg2 args]
    | ty == I.TyBool && arg1 args == CfBool False
    -> cfOp' ty I.ExpAnd $ mkCfArgs ty [cfOp' ty I.ExpNot [arg0 args]] ++ [arg2 args]
    | ty == I.TyBool && arg2 args == CfBool True
    -> cfOp' ty I.ExpOr $ mkCfArgs ty [cfOp' ty I.ExpNot [arg0 args]] ++ [arg1 args]
    | ty == I.TyBool && arg2 args == CfBool False
    -> cfOp' ty I.ExpAnd [arg0 args, arg1 args]
    -- If both branches have the same result, we dont care about the branch
    -- condition.  XXX This can be expensive
    | a1 == a2
    -> a1
    | otherwise -> noop
    where a1 = toExpr $ arg1 args
          a2 = toExpr $ arg2 args
  I.ExpGt orEq t
    | orEq      -> goOrd t gteCheck args
    | otherwise -> goOrd t gtCheck args
  I.ExpLt orEq t
    | orEq      -> goOrd t gteCheck (reverse args)
    | otherwise -> goOrd t gtCheck  (reverse args)
  I.ExpNot -> case arg0 args of
    CfBool b -> I.ExpLit (I.LitBool (not b))
    CfExpr (I.ExpOp (I.ExpEq t) args') -> I.ExpOp (I.ExpNeq t) args'
    CfExpr (I.ExpOp (I.ExpNeq t) args') -> I.ExpOp (I.ExpEq t) args'
    CfExpr (I.ExpOp (I.ExpGt orEq t) args') -> I.ExpOp (I.ExpLt (not orEq) t) args'
    CfExpr (I.ExpOp (I.ExpLt orEq t) args') -> I.ExpOp (I.ExpGt (not orEq) t) args'
    _ -> noop
  I.ExpAnd
    | CfBool lb <- arg0 args
    , CfBool rb <- arg1 args
   -> I.ExpLit (I.LitBool (lb && rb))
    | CfBool lb <- arg0 args
   -> if lb then toExpr $ arg1 args else I.ExpLit (I.LitBool False)
    | CfBool rb <- arg1 args
   -> if rb then toExpr $ arg0 args else I.ExpLit (I.LitBool False)
    | otherwise -> noop
  I.ExpOr
    | CfBool lb <- arg0 args
    , CfBool rb <- arg1 args
   -> I.ExpLit (I.LitBool (lb || rb))
    | CfBool lb <- arg0 args
   -> if lb then I.ExpLit (I.LitBool True) else toExpr $ arg1 args
    | CfBool rb <- arg1 args
   -> if rb then I.ExpLit (I.LitBool True) else toExpr $ arg0 args
    | otherwise -> noop

  I.ExpMul
    | isLitValue 0 $ arg0 args -> toExpr $ arg0 args
    | isLitValue 1 $ arg0 args -> toExpr $ arg1 args
    | isLitValue (-1) $ arg0 args -> cfOp' ty I.ExpNegate [arg1 args]
    | CfExpr (I.ExpOp I.ExpNegate [e']) <- arg0 args
   -> cfOp' ty I.ExpNegate $ mkCfArgs ty [cfOp' ty I.ExpMul $ mkCfArgs ty [e'] ++ [arg1 args]]
    | isLitValue 0 $ arg1 args -> toExpr $ arg1 args
    | isLitValue 1 $ arg1 args -> toExpr $ arg0 args
    | isLitValue (-1) $ arg1 args -> cfOp' ty I.ExpNegate [arg0 args]
    | CfExpr (I.ExpOp I.ExpNegate [e']) <- arg1 args
   -> cfOp' ty I.ExpNegate $ mkCfArgs ty [cfOp' ty I.ExpMul $ arg0 args : mkCfArgs ty [e']]
    | otherwise -> goNum

  I.ExpAdd
    | isLitValue 0 $ arg0 args -> toExpr $ arg1 args
    | isLitValue 0 $ arg1 args -> toExpr $ arg0 args
    | CfExpr (I.ExpOp I.ExpNegate [e']) <- arg1 args
   -> cfOp' ty I.ExpSub (arg0 args : mkCfArgs ty [e'])
    | otherwise -> goNum

  I.ExpSub
    | isLitValue 0 $ arg0 args -> cfOp' ty I.ExpNegate [arg1 args]
    | isLitValue 0 $ arg1 args -> toExpr $ arg0 args
    | CfExpr (I.ExpOp I.ExpNegate [e']) <- arg1 args
   -> cfOp' ty I.ExpAdd $ arg0 args : mkCfArgs ty [e']
    | otherwise -> goNum

  I.ExpNegate   -> case arg0 args of
    CfExpr (I.ExpOp I.ExpNegate [e']) -> e'
    CfExpr (I.ExpOp I.ExpSub [e1, e2]) -> cfOp' ty I.ExpSub $ mkCfArgs ty [e2, e1]
    _ -> goNum

  I.ExpAbs      -> goNum
  I.ExpSignum   -> goNum

  I.ExpDiv      -> goI2
  I.ExpMod      -> goI2
  I.ExpRecip    -> goF

  I.ExpIsNan _  -> goFB
  I.ExpIsInf _  -> goFB

  I.ExpFExp     -> goF
  I.ExpFSqrt    -> goF
  I.ExpFLog     -> goF
  I.ExpFPow     -> goF
  I.ExpFLogBase -> goF
  I.ExpFSin     -> goF
  I.ExpFCos     -> goF
  I.ExpFTan     -> goF
  I.ExpFAsin    -> goF
  I.ExpFAcos    -> goF
  I.ExpFAtan    -> goF
  I.ExpFSinh    -> goF
  I.ExpFCosh    -> goF
  I.ExpFTanh    -> goF
  I.ExpFAsinh   -> goF
  I.ExpFAcosh   -> goF
  I.ExpFAtanh   -> goF

  I.ExpBitAnd        -> toExpr (cfBitAnd ty args)
  I.ExpBitOr         -> toExpr (cfBitOr ty args)

  -- Unimplemented right now
  I.ExpRoundF        -> noop
  I.ExpCeilF         -> noop
  I.ExpFloorF        -> noop
  I.ExpBitXor        -> noop
  I.ExpBitComplement -> noop
  I.ExpBitShiftL     -> noop
  I.ExpBitShiftR     -> noop

  where
  noop          = I.ExpOp op $ map toExpr args
  goI2          = toExpr (cfIntOp2 ty op args)
  goF           = toExpr (cfFloating op args)
  goFB          = toExpr (cfFloatingB op args)
  cfOrd         = toExpr (cfOrd2 op args)
  goOrd ty' chk args' = fromOrdChecks cfOrd (chk ty' args')
  goNum         = toExpr (cfNum ty op args)

--------------------------------------------------------------------------------

-- | Lift nondeterministic choice up see see if we can further optimize. Args
-- have already been optimized.
liftChoice :: I.Type -> I.ExpOp -> [I.Expr] -> ConstFoldExp I.Expr
liftChoice ty op args = case op of
  I.ExpEq{}   -> go2
  I.ExpNeq{}  -> go2
  -- I.ExpCond --unnecessary
  I.ExpGt{}   -> go2
  I.ExpLt{}   -> go2

  I.ExpNot{}  -> go1
  I.ExpAnd{}  -> go2
  I.ExpOr{}   -> go2

  I.ExpMul    -> go2
  I.ExpAdd    -> go2
  I.ExpSub    -> go2
  I.ExpNegate -> go1
  I.ExpAbs    -> go1
  I.ExpSignum -> go1

  -- NOT SAFE TO LIFT!
  -- I.ExpDiv      -> --NO!

  -- Unimplemented currently: add as needed
  -- I.ExpMod           ->
  -- I.ExpRecip         ->
  -- I.ExpIsNan{}       ->
  -- I.ExpIsInf{}       ->
  -- I.ExpFExp          ->
  -- I.ExpFSqrt         ->
  -- I.ExpFLog          ->
  -- I.ExpFPow          ->
  -- I.ExpFLogBase      ->
  -- I.ExpFSin          ->
  -- I.ExpFCos          ->
  -- I.ExpFTan          ->
  -- I.ExpFAsin         ->
  -- I.ExpFAcos         ->
  -- I.ExpFAtan         ->
  -- I.ExpFSinh         ->
  -- I.ExpFCosh         ->
  -- I.ExpFTanh         ->
  -- I.ExpFAsinh        ->
  -- I.ExpFAcosh        ->
  -- I.ExpFAtanh        ->
  -- I.ExpBitAnd        ->
  -- I.ExpBitOr         ->
  -- I.ExpRoundF        ->
  -- I.ExpCeilF         ->
  -- I.ExpFloorF        ->
  -- I.ExpBitXor        ->
  -- I.ExpBitComplement ->
  -- I.ExpBitShiftL     ->
  -- I.ExpBitShiftR     ->

  _ -> cfOp ty op args
  where
  go1 = unOpLift  ty op args
  go2 = binOpLift ty op args

-- Args already optimized.
runCond :: I.Type
        -> I.ExpOp
        -> [I.Expr]
        -> I.Expr
        -> I.Expr
        -> ConstFoldExp I.Expr
runCond ty op args x1 x2 = do
  -- cheap equality: hash-consed
  if x1 == x2 then return x1 else cfOp ty op args

-- Args already optimized.
unOpLift :: I.Type -> I.ExpOp -> [I.Expr] -> ConstFoldExp I.Expr
unOpLift ty op args = case arg0 args of
  I.ExpOp I.ExpCond [_,x1,x2]
    -> runCond ty op args x1 x2
  _ -> cfOp ty op args

-- Args already optimized.
binOpLift :: I.Type -> I.ExpOp -> [I.Expr] -> ConstFoldExp I.Expr
binOpLift ty op args = case arg0 args of
  I.ExpOp I.ExpCond [_,x1,x2]
    -> runCond ty op args x1 x2
  _ -> case arg1 args of
         I.ExpOp I.ExpCond [_,x1,x2]
           -> runCond ty op args x1 x2
         _ -> cfOp ty op args

--------------------------------------------------------------------------------
-- Constant-folded values

-- | Check if we're comparing the max or min bound for >= and optimize.
-- Assumes args are already folded.
gteCheck :: I.Type -> [CfVal] -> Maybe Bool
gteCheck t [l,r]
  -- forall a. max >= a
  | CfInteger _ x <- l
  , Just s <- toMaxSize t
  , x == s
  = Just True
  -- forall a. a >= min
  | CfInteger _ y <- r
  , Just s <- toMinSize t
  , y == s
  = Just True
  | otherwise
  = Nothing
gteCheck _ _ = err "wrong number of args to gtCheck."

-- | Check if we're comparing the max or min bound for > and optimize.
-- Assumes args are already folded.
gtCheck :: I.Type -> [CfVal] -> Maybe Bool
gtCheck t [l,r]
  -- forall a. not (min > a)
  | CfInteger _ x <- l
  , Just s <- toMinSize t
  , x == s
  = Just False
  -- forall a. not (a > max)
  | CfInteger _ y <- r
  , Just s <- toMaxSize t
  , y == s
  = Just False
  | otherwise
  = Nothing
gtCheck _ _ = err "wrong number of args to gtCheck."

fromOrdChecks :: I.Expr -> Maybe Bool -> I.Expr
fromOrdChecks expr = maybe expr (toExpr . CfBool)

-- | Apply a binary operation that requires an ord instance.
cfOrd2 :: I.ExpOp
       -> [CfVal]
       -> CfVal
cfOrd2 op [l,r] = case (l,r) of
  (CfBool x,   CfBool y)        -> CfBool (op' x y)
  (CfInteger _ x,CfInteger _ y) -> CfBool (op' x y)
  (CfFloat x,  CfFloat y)       -> CfBool (op' x y)
  (CfDouble x, CfDouble y)      -> CfBool (op' x y)
  _                         -> CfExpr (I.ExpOp op [toExpr l, toExpr r])
  where
  op' :: Ord a => a -> a -> Bool
  op' = case op of
    I.ExpEq _     -> (==)
    I.ExpNeq _    -> (/=)
    I.ExpGt orEq _
      | orEq      -> (>=)
      | otherwise -> (>)
    I.ExpLt orEq _
      | orEq      -> (<=)
      | otherwise -> (<)
    _ -> err "bad op to cfOrd2"
cfOrd2 _ _ = err "wrong number of args to cfOrd2"

