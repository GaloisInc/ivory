{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}

--
-- Constant folder.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Opts.ConstFold
  ( constFold
  ) where

import Ivory.Opts.ConstFoldComp

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I
import Ivory.Language.Cast (toMaxSize, toMinSize)

import Control.Arrow (second)
import Data.Maybe
import Data.List
import qualified Data.DList as D

--------------------------------------------------------------------------------
-- Constant folding

-- | Expression to expression optimization.
type ExprOpt = I.Type -> I.Expr -> I.Expr

constFold :: I.Proc -> I.Proc
constFold = procFold cf

procFold :: ExprOpt -> I.Proc -> I.Proc
procFold opt proc =
  let cxt   = I.procSym proc
      body' = D.toList $ foldl' (stmtFold cxt opt) D.empty (I.procBody proc)
   in proc { I.procBody = body' }

stmtFold :: String -> ExprOpt -> D.DList I.Stmt -> I.Stmt -> D.DList I.Stmt
stmtFold cxt opt blk stmt =
  case stmt of
    I.IfTE _ [] []       -> blk
    I.IfTE e [] b1       -> stmtFold cxt opt blk $ I.IfTE (I.ExpOp I.ExpNot [e]) b1 []
    I.IfTE e b0 b1       ->
      let e' = opt I.TyBool e in
      case e' of
        I.ExpLit (I.LitBool b) -> if b then blk `D.append` (newFold' b0)
                                    else blk `D.append` (newFold' b1)
        _                      -> snoc $ I.IfTE e' (newFold b0) (newFold b1)
    I.Assert e           ->
      let e' = opt I.TyBool e in
      case e' of
        I.ExpLit (I.LitBool b) ->
          if b then blk
            else error $ "Constant folding evaluated a False assert()"
                       ++ " in evaluating expression " ++ show e
                       ++ " of function " ++ cxt
        _                      -> snoc (I.Assert e')
    I.CompilerAssert e        ->
      let e' = opt I.TyBool e in
      let go = snoc (I.CompilerAssert e') in
      case e' of
        I.ExpLit (I.LitBool b) ->
          -- It's OK to have false but unreachable compiler asserts.
          if b then blk else go
        _                      -> go
    I.Assume e           ->
      let e' = opt I.TyBool e in
      case e' of
        I.ExpLit (I.LitBool b) ->
          if b then blk
            else error $ "Constant folding evaluated a False assume()"
                       ++ " in evaluating expression " ++ show e
                       ++ " of function " ++ cxt
        _                      -> snoc (I.Assume e')

    I.Return e           -> snoc $ I.Return (typedFold opt e)
    I.ReturnVoid         -> snoc I.ReturnVoid
    I.Deref t var e      -> snoc $ I.Deref t var (opt t e)
    I.Store t e0 e1      -> snoc $ I.Store t (opt t e0) (opt t e1)
    I.Assign t v e       -> snoc $ I.Assign t v (opt t e)
    I.Call t mv c tys    -> snoc $ I.Call t mv c (map (typedFold opt) tys)
    I.Local t var i      -> snoc $ I.Local t var $ constFoldInits i
    I.RefCopy t e0 e1    -> snoc $ I.RefCopy t (opt t e0) (opt t e1)
    I.AllocRef{}         -> snoc stmt
    I.Loop v e incr blk' ->
      let ty = I.TyInt I.Int32 in
      case opt ty e of
        I.ExpLit (I.LitBool b) ->
          if b then error $ "Constant folding evaluated True expression "
                          ++ "in a loop bound.  The loop will never terminate!"
               else error $ "Constant folding evaluated False expression "
                          ++ "in a loop bound.  The loop will never execute!"
        _                      ->
          snoc $ I.Loop v (opt ty e) (loopIncrFold (opt ty) incr)
                        (newFold blk')
    I.Break              -> snoc I.Break
    I.Forever b          -> snoc $ I.Forever (newFold b)
    I.Comment c          -> snoc $ I.Comment c
  where sf       = stmtFold cxt opt
        newFold' = foldl' sf D.empty
        newFold  = D.toList . newFold'
        snoc     = (blk `D.snoc`)

constFoldInits :: I.Init -> I.Init
constFoldInits I.InitZero = I.InitZero
constFoldInits (I.InitExpr ty expr) = I.InitExpr ty $ cf ty expr
constFoldInits (I.InitStruct i) = I.InitStruct $ map (second constFoldInits) i
constFoldInits (I.InitArray i) = I.InitArray $ map constFoldInits i

--------------------------------------------------------------------------------
-- Expressions

-- | Constant folding over expressions.
cf :: ExprOpt
cf ty e =
  case e of
    I.ExpSym{} -> e
    I.ExpVar{} -> e
    I.ExpLit{} -> e

    I.ExpOp op args       -> liftChoice ty op args

    I.ExpLabel t e0 s     -> I.ExpLabel t (cf t e0) s

    I.ExpIndex t e0 t1 e1 -> I.ExpIndex t (cf t e0) t1 (cf t e1)

    I.ExpSafeCast t e0    ->
      let e0' = cf t e0
       in fromMaybe (I.ExpSafeCast t e0') $ do
            _ <- destLit e0'
            return e0'

    I.ExpToIx e0 maxSz    ->
      let ty' = I.TyInt I.Int32 in
      let e0' = cf ty' e0 in
      case destIntegerLit e0' of
        Just i  -> I.ExpLit $ I.LitInteger $ i `rem` maxSz
        Nothing -> I.ExpToIx e0' maxSz

    I.ExpAddrOfGlobal{}   -> e
    I.ExpMaxMin{}         -> e

loopIncrFold :: (I.Expr -> I.Expr) -> I.LoopIncr -> I.LoopIncr
loopIncrFold opt incr =
  case incr of
    I.IncrTo e0 -> I.IncrTo (opt e0)
    I.DecrTo e0 -> I.DecrTo (opt e0)

--------------------------------------------------------------------------------

typedFold :: ExprOpt -> I.Typed I.Expr -> I.Typed I.Expr
typedFold opt tval@(I.Typed ty val) = tval { I.tValue = opt ty val }

arg0 :: [a] -> a
arg0 = flip (!!) 0

arg1 :: [a] -> a
arg1 = flip (!!) 1

arg2 :: [a] -> a
arg2 = flip (!!) 2

-- | Reconstruct an operator, folding away operations when possible.
cfOp :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
cfOp ty op args = cfOp' ty op $ case op of
  I.ExpEq t -> cfargs t args
  I.ExpNeq t -> cfargs t args
  I.ExpCond -> let (cond, rest) = splitAt 1 args in cfargs I.TyBool cond ++ cfargs ty rest
  I.ExpGt _ t -> cfargs t args
  I.ExpLt _ t -> cfargs t args
  I.ExpIsNan t  -> cfargs t args
  I.ExpIsInf t  -> cfargs t args
  _ -> cfargs ty args
  where
  cfargs ty' = mkCfArgs ty' . map (cf ty')

cfOp' :: I.Type -> I.ExpOp -> [CfVal] -> I.Expr
cfOp' ty op args = case op of
  I.ExpEq _  -> cfOrd
  I.ExpNeq _ -> cfOrd
  I.ExpCond
    | CfBool b <- arg0 args
    -> if b then a1 else a2
    -- If either branch is a boolean literal, reduce to logical AND or OR.
    | ty == I.TyBool && arg1 args == CfBool True -> cfOp' ty I.ExpOr [arg0 args, arg2 args]
    | ty == I.TyBool && arg1 args == CfBool False -> cfOp' ty I.ExpAnd $ mkCfArgs ty [cfOp' ty I.ExpNot [arg0 args]] ++ [arg2 args]
    | ty == I.TyBool && arg2 args == CfBool True -> cfOp' ty I.ExpOr $ mkCfArgs ty [cfOp' ty I.ExpNot [arg0 args]] ++ [arg1 args]
    | ty == I.TyBool && arg2 args == CfBool False -> cfOp' ty I.ExpAnd [arg0 args, arg1 args]
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

  I.ExpMul      -> goNum
  I.ExpAdd      -> goNum
  I.ExpSub      -> goNum
  I.ExpNegate   -> goNum
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

-- | Lift nondeterministic choice up see see if we can further optimize.
liftChoice :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
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

  -- -- NOT SAFE TO LIFT!
  -- I.ExpDiv      -> --NO!

  -- Unimplemented currently: add as needed
  -- I.ExpMod      ->
  -- I.ExpRecip    ->
  -- I.ExpIsNan{}  ->
  -- I.ExpIsInf{}  ->
  -- I.ExpFExp     ->
  -- I.ExpFSqrt    ->
  -- I.ExpFLog     ->
  -- I.ExpFPow     ->
  -- I.ExpFLogBase ->
  -- I.ExpFSin     ->
  -- I.ExpFCos     ->
  -- I.ExpFTan     ->
  -- I.ExpFAsin    ->
  -- I.ExpFAcos    ->
  -- I.ExpFAtan    ->
  -- I.ExpFSinh    ->
  -- I.ExpFCosh    ->
  -- I.ExpFTanh    ->
  -- I.ExpFAsinh   ->
  -- I.ExpFAcosh   ->
  -- I.ExpFAtanh   ->
  -- I.ExpBitAnd        ->
  -- I.ExpBitOr         ->
  -- -- Unimplemented right now
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


--XXX the equality comparisons below can be expensive.  Hashmap?  Also, awkward
-- style, but I want sharing of (liftChoice ...) expression in branch condition
-- and result.
unOpLift :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
unOpLift ty op args = case a0 of
  I.ExpOp I.ExpCond [_,x1,x2]
    -> let a = lt x1 in
       if a == lt x2 then a else c
  _ -> c
  where
  a0     = arg0 args
  lt x   = liftChoice ty op [x]
  c      = cfOp ty op args

binOpLift :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
binOpLift ty op args = case a0 of
  I.ExpOp I.ExpCond [_,x1,x2]
    -> let a = lt0 x1 in
       if a == lt0 x2 then a else c
  _ -> case a1 of
         I.ExpOp I.ExpCond [_,x1,x2]
           -> let a = lt1 x1 in
              if a == lt1 x2 then a else c
         _ -> c
  where
  a0     = arg0 args
  a1     = arg1 args
  lt0 x  = lt x a1
  lt1 x  = lt a0 x
  lt a b = liftChoice ty op [a, b]
  c      = cfOp ty op args

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

