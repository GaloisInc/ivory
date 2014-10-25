{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Hash-cons expressions
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Opts.HashCons
  ( HashExprM
  , HashMap
  , CopyMap
  , addCopy
  , findCopy
  , hashStmt
  , lookupSeen
  , flattenExp
  , newHashMap
  , runHashMap
  , getHashMap
  , getKeys
  , forceHashKey
  , hashKey
  , unHash
  , subKeys
  , keyToExp
  , inlineAssigns
  ) where

import           Prelude hiding (exp, init)
import qualified Data.Hashable as H
import           MonadLib
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as I
import           Control.Applicative
import qualified Data.Map as M

import qualified Ivory.Language.Syntax as A
import qualified Ivory.Language.Array  as A

--------------------------------------------------------------------------------

data State = State
  { expMap  :: I.IntMap (A.Typed A.Expr)
  -- ^ Hash-consed values.
  , seenKey :: S.IntSet
  -- ^ Is the a key (for a recursive expression) referenced more than once?
  -- N.B. this can become stale if the map is modified (e.g., after
  -- constant folding).
  , maxSalt :: Salt
  -- ^ Maximum salt used.
  } deriving (Show, Eq)

initSt :: State
initSt = State I.empty S.empty 0

--------------------------------------------------------------------------------
-- Monad definition

-- Map from local vars to expressions. Used to inline simple expressions that
-- were assignments.
type CopyMap = M.Map A.Var A.Expr

-- | State monad to hold the hash-cons map.
newtype HashExprT m a = HashCons
  { unHashCons :: StateT State m a
  } deriving (Functor, Applicative, Monad)

instance MonadT HashExprT where
  lift = HashCons . lift

type HashExprM = HashExprT (StateT CopyMap Id)

type HashMap = I.IntMap (A.Typed A.Expr)

instance StateM HashExprM State where
  get = HashCons get
  set = HashCons . set

-- | Run the monad.
runHashMap :: HashExprM a -> ((a, State), CopyMap)
runHashMap s = runId $ runStateT M.empty $ runStateT initSt (unHashCons s)

--------------------------------------------------------------------------------

-- | Update the copy map (for inlining assignments).
addCopy :: A.Var -> A.Expr -> HashExprM ()
addCopy v e = do
  copies <- lift get
  lift (set (M.insert v e copies))

findCopy :: A.Var -> A.Expr -> HashExprM A.Expr
findCopy v e = do
  copies <- lift get
  return (M.findWithDefault e v copies)

-- | Return all the keys in the map.
getKeys :: HashExprM [I.Key]
getKeys = do
  st <- get
  return (I.keys (expMap st))

keyToExp :: I.Key -> A.Expr
keyToExp = A.ExpHash

getHashMap :: HashExprM HashMap
getHashMap = do
  return . expMap =<< get

-- | Replace the hash map.
newHashMap :: HashMap -> HashExprM ()
newHashMap mp = do
  st <- get
  set st { expMap = mp }

-- | Lookup a key in the seen values.
lookupSeen :: I.Key -> HashExprM Bool
lookupSeen key = do
  st <- get
  return (S.member key (seenKey st))

-- | Extract an expression from the map given its key.
unHash :: I.Key -> HashExprM (A.Typed A.Expr)
unHash key = do
  st <- get
  case I.lookup key (expMap st) of
    Nothing -> error "not in map"
    Just e  -> return e

-- | Leave expression hash-consed or flatten it.
data Hash = Hash | Unhash deriving (Show, Read, Eq)

subKeys :: I.Key -> HashExprM [I.Key]
subKeys key = do
  tye <- unHash key
  let e = A.tValue tye
  go e
  where
  go :: A.Expr -> HashExprM [I.Key]
  go e = case e of
    A.ExpSym{}
      -> return []
    A.ExpVar{}
      -> return []
    A.ExpLit{}
      -> return []
    A.ExpOp _op args
      -> concat <$> mapM go args
    A.ExpLabel _t e0 _s
      -> go e0
    A.ExpIndex _t e0 _t1 e1
      -> liftM2 (++) (go e0) (go e1)
    A.ExpSafeCast _t e0
      -> go e0
    A.ExpToIx e0 _maxSz
      -> go e0
    A.ExpAddrOfGlobal{}
      -> return []
    A.ExpMaxMin{}
      -> return []
    A.ExpHash key'
      -> subKeys key'

-- | Retrieve an expression and inline all subexpressions.
flattenExp :: I.Key -> HashExprM A.Expr
flattenExp key = do
  tye <- unHash key
  let e = A.tValue tye
  go e
  where
  go e = case e of
    A.ExpSym{}
      -> return e
    A.ExpVar{}
      -> return e
    A.ExpLit{}
      -> return e
    A.ExpOp op args
      -> A.ExpOp op <$> mapM go args
    A.ExpLabel t e0 s
      -> do e0' <- go e0
            return (A.ExpLabel t e0' s)
    A.ExpIndex t e0 t1 e1
      -> do e0' <- go e0
            e1' <- go e1
            return (A.ExpIndex t e0' t1 e1')
    A.ExpSafeCast t e0
      -> A.ExpSafeCast t <$> go e0
    A.ExpToIx e0 maxSz
      -> do e0' <- go e0
            return (A.ExpToIx e0' maxSz)
    A.ExpAddrOfGlobal{}
      -> return e
    A.ExpMaxMin{}
      -> return e
    A.ExpHash key'
      -> flattenExp key'

type Salt = Int

-- | Force an overwrite of an expression in the map.
forceHashKey :: I.Key -> A.Typed A.Expr -> HashExprM ()
forceHashKey key tye = do
  st <- get
  set st { expMap = I.insert key tye (expMap st) }

-- | Put an expression in the hash-cons map, returning its key and the
-- hash-consed expression. Guaranteed not to overwrite other keys.
hashKey :: A.Type -> A.Expr -> HashExprM I.Key
hashKey ty e = case e of
  A.ExpSym{}
    -> nonRec
  A.ExpVar{}
    -> nonRec
  A.ExpLit{}
    -> nonRec
  A.ExpOp op args
    -> do args' <- mapM (hashWrap ty) args
          recExp ty (A.ExpOp op args')
  A.ExpLabel t e0 s
    -> do e0' <- hashWrap t e0
          recExp ty (A.ExpLabel t e0' s)
  A.ExpIndex t0 e0 t1 e1
    -> do e0' <- hashWrap t0 e0
          e1' <- hashWrap t1 e1
          recExp ty (A.ExpIndex t0 e0' t1 e1')
  A.ExpSafeCast t e0
    -> do e0' <- hashWrap t e0
          recExp ty (A.ExpSafeCast t e0')
  A.ExpToIx e0 maxSz
    -> do e0' <- hashWrap A.ixRep e0
          recExp ty (A.ExpToIx e0' maxSz)
  A.ExpAddrOfGlobal{}
    -> nonRec
  A.ExpMaxMin{}
    -> nonRec
  A.ExpHash{}
    -> error $ "unexpected ExpHash in hashKey."
  where
  nonRec = hashExp' False 0 ty e
  recExp = hashExp' True 0

-- Hash an expression returning it's result, putting it in the map if
-- necessary. If the expression has been seen previously, update the list of
-- seen keys. Invariant: the expression passed here is non-recursive.
hashExp' :: Bool -> Salt -> A.Type -> A.Expr -> HashExprM I.Key
hashExp' b salt ty e = do
  let key = H.hashWithSalt salt e
  let tyExp = A.Typed ty e
  st <- get
  case I.lookup key (expMap st) of
    Nothing
      -> do set (st { expMap = I.insert key tyExp (expMap st) })
            when (salt > maxSalt st) (set st { maxSalt = salt })
            return key
    Just tyExp' -- Check for collisions
      -> if tyExp == tyExp' -- Not expensive, since no recursion
           then do when b $ set (st { seenKey = key `S.insert` (seenKey st) })
                   return key
           else hashExp' b (salt+1) ty e -- Hash with a new salt

-- | Put an expression in the hash-cons map, returning a wrapped key.
hashWrap :: A.Type -> A.Expr -> HashExprM A.Expr
hashWrap ty exp = keyToExp <$> hashKey ty exp

--------------------------------------------------------------------------------

-- | Hash all the expressions in a statement returning the statement with its
-- hash-consed expressions.
hashStmt :: A.Stmt -> HashExprM [A.Stmt]
hashStmt stmt = case stmt of
  A.IfTE b b0 b1
    -> do b'  <- hashWrap A.TyBool b
          b0' <- mapM hashStmt b0
          b1' <- mapM hashStmt b1
          return [A.IfTE b' (concat b0') (concat b1')]
  A.Assert e
    -> do e' <- hashWrap A.TyBool e
          return [A.Assert e']
  A.CompilerAssert e
    -> do e' <- hashWrap A.TyBool e
          return [A.CompilerAssert e']
  A.Assume e
    -> do e' <- hashWrap A.TyBool e
          return [A.Assume e']
  A.Return (A.Typed ty e)
    -> do e' <- hashWrap ty e
          return [A.Return (A.Typed ty e')]
  A.ReturnVoid
    -> return [A.ReturnVoid]
  A.Deref ty v e
    -> do e' <- hashWrap (A.TyRef ty) e
          return [A.Deref ty v e']
  A.Store ty e0 e1
    -> do e0' <- hashWrap (A.TyRef ty) e0
          e1' <- hashWrap (A.TyRef ty) e1
          return [A.Store ty e0' e1']
  A.Assign ty v e
    -> mkAssign ty v e
  A.Call ty mv nm es
    -> do es' <- mapM go es
          return [A.Call ty mv nm es']
    where
    go (A.Typed ty' exp) = A.Typed ty' <$> hashWrap ty' exp
  A.Local ty v i
    -> do i' <- hashInit i
          return [A.Local ty v i']
  A.RefCopy ty e0 e1
    -> do e0' <- hashWrap (A.TyRef ty) e0
          e1' <- hashWrap (A.TyRef ty) e1
          return [A.RefCopy ty e0' e1']
  A.AllocRef{}
    -> return [stmt]
  A.Loop v e incr blk
    -> do e'    <- hashWrap A.ixRep e
          blk'  <- mapM hashStmt blk
          incr' <- hashIncr
          return [A.Loop v e' incr' (concat blk')]
    where
    hashIncr = case incr of
      A.IncrTo i -> A.IncrTo <$> hashWrap A.ixRep i
      A.DecrTo i -> A.DecrTo <$> hashWrap A.ixRep i
  A.Forever blk
    -> do blk' <- mapM hashStmt blk
          return [A.Forever (concat blk')]
  A.Break
    -> return [A.Break]
  A.Comment{}
    -> return [stmt]

hashInit :: A.Init -> HashExprM A.Init
hashInit init = case init of
  A.InitZero
    -> return A.InitZero
  A.InitExpr ty e
    -> A.InitExpr ty <$> hashWrap ty e
  A.InitStruct inits
    -> A.InitStruct <$> mapM go inits
    where
    go (s,i) = do i' <- hashInit i
                  return (s,i')
  A.InitArray inits
    -> A.InitArray <$> mapM hashInit inits

-- For local assignment @I.Assign t v e@, optimize @e@ to @e'@, and if @e'@ is
-- "simple" (i.e., itself a variable or literal), then place it in the map and
-- remove the assignment statement. In the constant folder ('cf' below), we'll
-- dereference variable directly to inline it.
mkAssign :: A.Type -> A.Var -> A.Expr -> HashExprM [A.Stmt]
mkAssign ty v e = do
  e' <- hashWrap ty e
  let copyProp = addCopy v e' >> return []
  case e of
    A.ExpSym{}          -> copyProp
    A.ExpVar{}          -> copyProp
    A.ExpLit{}          -> copyProp
    A.ExpAddrOfGlobal{} -> copyProp
    A.ExpMaxMin{}       -> copyProp
    _                   -> return [A.Assign ty v e']

-- | Walk the hash-cons map, inlining assignments.
inlineAssigns :: HashExprM ()
inlineAssigns = do
  allkeys <- getKeys
  mapM_ go allkeys
  where
  go key = do
    tye <- unHash key
    let e = A.tValue tye
    case e of
      A.ExpVar v -> do e' <- findCopy v e
                       forceHashKey key (const e' <$> tye)
      _          -> return ()
