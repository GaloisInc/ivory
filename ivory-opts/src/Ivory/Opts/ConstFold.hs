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
    I.Local{}            -> snoc stmt
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

mkCfBool :: [I.Expr] -> [Maybe Bool]
mkCfBool = map destBoolLit

-- | Reconstruct an operator, folding away operations when possible.
cfOp :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
cfOp ty op args = case op of
  I.ExpEq t  -> cfOrd t
  I.ExpNeq t -> cfOrd t
  I.ExpCond
    | Just b <- arg0 goBoolArgs
    -> if b then a1 else a2
    -- If both branches have the same result, we dont care about the branch
    -- condition.
    | a1 == a2
    -> a1
    | otherwise -> noop ty
    where a1 = arg1 (toExpr' ty)
          a2 = arg2 (toExpr' ty)
  I.ExpGt orEq t
    | orEq      -> goOrd t gteCheck args
    | otherwise -> goOrd t gtCheck args
  I.ExpLt orEq t
    | orEq      -> goOrd t gteCheck (reverse args)
    | otherwise -> goOrd t gtCheck  (reverse args)
  I.ExpNot
    | Just b <- arg0 goBoolArgs
    -> I.ExpLit (I.LitBool (not b))
    | otherwise -> noop ty
  I.ExpAnd
    | Just lb <- arg0 goBoolArgs
    , Just rb <- arg1 goBoolArgs
    -> I.ExpLit (I.LitBool (lb && rb))
    | Just lb <- arg0 goBoolArgs
    -> if lb then arg1 (toExpr' ty) else I.ExpLit (I.LitBool False)
    | Just rb <- arg1 goBoolArgs
    -> if rb then arg0 (toExpr' ty) else I.ExpLit (I.LitBool False)
    | otherwise -> noop ty
  I.ExpOr
    | Just lb <- arg0 goBoolArgs
    , Just rb <- arg1 goBoolArgs
    -> I.ExpLit (I.LitBool (lb || rb))
    | Just lb <- arg0 goBoolArgs
    -> if lb then I.ExpLit (I.LitBool True) else arg1 (toExpr' ty)
    | Just rb <- arg1 goBoolArgs
    -> if rb then I.ExpLit (I.LitBool True) else arg0 (toExpr' ty)
    | otherwise -> noop ty

  I.ExpMul      -> goNum
  I.ExpAdd      -> goNum
  I.ExpSub      -> goNum
  I.ExpNegate   -> goNum
  I.ExpAbs      -> goNum
  I.ExpSignum   -> goNum

  I.ExpDiv      -> goI2
  I.ExpMod      -> goI2
  I.ExpRecip    -> goF

  I.ExpIsNan t  -> goFB t
  I.ExpIsInf t  -> goFB t

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

  I.ExpBitAnd        -> toExpr (cfBitAnd ty $ goArgs ty)
  I.ExpBitOr         -> toExpr (cfBitOr ty  $ goArgs ty)

  -- Unimplemented right now
  I.ExpRoundF        -> noop ty
  I.ExpCeilF         -> noop ty
  I.ExpFloorF        -> noop ty
  I.ExpBitXor        -> noop ty
  I.ExpBitComplement -> noop ty
  I.ExpBitShiftL     -> noop ty
  I.ExpBitShiftR     -> noop ty

  where
  goArgs ty'    = mkCfArgs ty' $ mkArgs ty' args
  toExpr'       = map toExpr . goArgs
  goBoolArgs    = mkCfBool $ mkArgs I.TyBool args
  noop          = I.ExpOp op . map toExpr . goArgs
  goI2          = toExpr (cfIntOp2 ty op $ goArgs ty)
  goF           = toExpr (cfFloating op $ goArgs ty)
  goFB ty'      = toExpr (cfFloatingB op $ goArgs ty')
  cfOrd ty'     = toExpr (cfOrd2 op $ goArgs ty')
  goOrd ty' chk args' =
    let args0 = mkCfArgs ty' $ mkArgs ty' args' in
    fromOrdChecks (cfOrd ty') (chk ty' args0)
  goNum         = toExpr (cfNum ty op $ goArgs ty)

  -- The only call to the main cf function, outside of cf itself.
  mkArgs :: I.Type -> [I.Expr] -> [I.Expr]
  mkArgs = map . cf

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

unOpLift :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
unOpLift ty op args
  | I.ExpOp I.ExpCond [_,x1,x2] <- a0
  , lt x1 == lt x2
  = lt x1
  | otherwise
  = cfOp ty op args
  where a0     = arg0 args
        lt x   = liftChoice ty op [x]

binOpLift :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
binOpLift ty op args
  | I.ExpOp I.ExpCond [_,x1,x2] <- a0
  , lt0 x1 == lt0 x2
  = lt0 x1
  | I.ExpOp I.ExpCond [_,x1,x2] <- a1
  , lt1 x1 == lt1 x2
  = lt1 x1
  | otherwise
  = cfOp ty op args
  where a0     = arg0 args
        a1     = arg1 args
        lt0 x  = lt x a1
        lt1 x  = lt a0 x
        lt a b = liftChoice ty op [a, b]

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

