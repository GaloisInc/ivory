{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.Opts.ConstFold
  ( constFold
  ) where

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I
import Ivory.Language.Cast (toMaxSize, toMinSize)

import Control.Monad (mzero,msum)
import Data.Maybe
import Data.List
import Data.Word
import Data.Int
import qualified Data.DList as D

--------------------------------------------------------------------------------
-- Constant folding
--------------------------------------------------------------------------------

constFold :: I.Proc -> I.Proc
constFold = procFold cf

-- | Expression to expression optimization.
type ExprOpt = I.Type -> I.Expr -> I.Expr

-- | Constant folding.
cf :: ExprOpt
cf ty e =
  case e of
    I.ExpSym{} -> e
    I.ExpVar{} -> e
    I.ExpLit{} -> e

    I.ExpOp op args       -> cfOp ty op args

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

loopIncrFold :: (I.Expr -> I.Expr) -> I.LoopIncr -> I.LoopIncr
loopIncrFold opt incr =
  case incr of
    I.IncrTo e0 -> I.IncrTo (opt e0)
    I.DecrTo e0 -> I.DecrTo (opt e0)

typedFold :: ExprOpt -> I.Typed I.Expr -> I.Typed I.Expr
typedFold opt tval@(I.Typed ty val) = tval { I.tValue = opt ty val }

arg0 :: [a] -> a
arg0 = flip (!!) 0

arg1 :: [a] -> a
arg1 = flip (!!) 1

arg2 :: [a] -> a
arg2 = flip (!!) 2

mkArgs :: I.Type -> [I.Expr] -> [I.Expr]
mkArgs ty = map (cf ty)

mkCfArgs :: [I.Expr] -> [CfVal]
mkCfArgs = map toCfVal

mkCfBool :: [I.Expr] -> [Maybe Bool]
mkCfBool = map destBoolLit

-- | Reconstruct an operator, folding away operations when possible.
cfOp :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
cfOp ty op args =
  case op of
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
  goArgs ty'    = mkCfArgs $ mkArgs ty' args
  toExpr'       = map toExpr . goArgs
  goBoolArgs    = mkCfBool $ mkArgs I.TyBool args
  noop          = I.ExpOp op . map toExpr . goArgs
  goI2          = toExpr (cfIntOp2 ty op $ goArgs ty)
  goF           = toExpr (cfFloating op $ goArgs ty)
  goFB ty'      = toExpr (cfFloatingB op $ goArgs ty')
  cfOrd ty'     = toExpr (cfOrd2 op $ goArgs ty')
  goOrd ty' chk args' =
    let args0 = mkCfArgs $ mkArgs ty' args' in
    fromOrdChecks (cfOrd ty') (chk ty' args0)
  goNum         = toExpr (cfNum ty op $ goArgs ty)


cfBitAnd :: I.Type -> [CfVal] -> CfVal
cfBitAnd ty [l,r]
  | ones ty  l = r
  | ones ty  r = l
  | zeros ty l = CfInteger 0
  | zeros ty r = CfInteger 0
  | otherwise  = CfExpr (I.ExpOp I.ExpBitAnd [toExpr l, toExpr r])
cfBitAnd _ _ = err "Wrong number of args to cfBitAnd in constant folder."

cfBitOr :: I.Type -> [CfVal] -> CfVal
cfBitOr ty [l,r]
  | zeros ty l = r
  | zeros ty r = l
  | ones ty  l = CfInteger 1
  | ones ty  r = CfInteger 1
  | otherwise  = CfExpr (I.ExpOp I.ExpBitOr [toExpr l, toExpr r])
cfBitOr _ _ = err "Wrong number of args to cfBitOr in constant folder."

-- Min values for word types.
zeros :: I.Type -> CfVal -> Bool
zeros I.TyWord{} (CfInteger i) = i == 0
zeros _ _ = False

-- Max values for word types.
ones :: I.Type -> CfVal -> Bool
ones ty (CfInteger i) =
  case ty of
    I.TyWord{} -> maybe False (i ==) (toMaxSize ty)
    _          -> False
ones _ _ = False

-- | Literal expression destructor.
destLit :: I.Expr -> Maybe I.Literal
destLit ex = case ex of
  I.ExpLit lit -> return lit
  _            -> mzero

-- | Boolean literal destructor.
destBoolLit :: I.Expr -> Maybe Bool
destBoolLit ex = do
  I.LitBool b <- destLit ex
  return b

-- | Integer literal destructor.
destIntegerLit :: I.Expr -> Maybe Integer
destIntegerLit ex = do
  I.LitInteger i <- destLit ex
  return i

-- | Float literal destructor.
destFloatLit :: I.Expr -> Maybe Float
destFloatLit ex = do
  I.LitFloat i <- destLit ex
  return i

-- | Double literal destructor.
destDoubleLit :: I.Expr -> Maybe Double
destDoubleLit ex = do
  I.LitDouble i <- destLit ex
  return i

-- Constant-folded Values ------------------------------------------------------

-- | Constant-folded values.
data CfVal
  = CfBool Bool
  | CfInteger Integer
  | CfFloat Float
  | CfDouble Double
  | CfExpr I.Expr
    deriving (Show)

-- | Convert to a constant-folded value.  Picks the one successful lit, if any.
toCfVal :: I.Expr -> CfVal
toCfVal ex = fromMaybe (CfExpr ex) $ msum
  [ CfBool    `fmap` destBoolLit    ex
  , CfInteger `fmap` destIntegerLit ex
  , CfFloat   `fmap` destFloatLit   ex
  , CfDouble  `fmap` destDoubleLit  ex
  ]

-- | Convert back to an expression.
toExpr :: CfVal -> I.Expr
toExpr val = case val of
  CfBool b    -> I.ExpLit (I.LitBool b)
  CfInteger i -> I.ExpLit (I.LitInteger i)
  CfFloat f   -> I.ExpLit (I.LitFloat f)
  CfDouble d  -> I.ExpLit (I.LitDouble d)
  CfExpr ex   -> ex

-- | Check if we're comparing the max or min bound for >= and optimize.
gteCheck :: I.Type -> [CfVal] -> Maybe Bool
gteCheck t [l,r]
  -- forall a. max >= a
  | CfInteger x <- l
  , Just s <- toMaxSize t
  , x == s = Just True
  -- forall a. a >= min
  | CfInteger y <- r
  , Just s <- toMinSize t
  , y == s = Just True
  | otherwise                            = Nothing
gteCheck _ _ = err "wrong number of args to gtCheck."

-- | Check if we're comparing the max or min bound for > and optimize.
gtCheck :: I.Type -> [CfVal] -> Maybe Bool
gtCheck t [l,r]
  -- forall a. not (min > a)
  | CfInteger x <- l
  , Just s <- toMinSize t
  , x == s = Just False
  -- forall a. not (a > max)
  | CfInteger y <- r
  , Just s <- toMaxSize t
  , y == s = Just False
  | otherwise                            = Nothing
gtCheck _ _ = err "wrong number of args to gtCheck."

fromOrdChecks :: I.Expr -> Maybe Bool -> I.Expr
fromOrdChecks expr = maybe expr (toExpr . CfBool)

-- | Apply a binary operation that requires an ord instance.
cfOrd2 :: I.ExpOp
       -> [CfVal]
       -> CfVal
cfOrd2 op [l,r] = case (l,r) of
  (CfBool x,   CfBool y)    -> CfBool (op' x y)
  (CfInteger x,CfInteger y) -> CfBool (op' x y)
  (CfFloat x,  CfFloat y)   -> CfBool (op' x y)
  (CfDouble x, CfDouble y)  -> CfBool (op' x y)
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

--------------------------------------------------------------------------------

class Integral a => IntegralOp a where
  appI1 :: (a -> a) -> a -> CfVal
  appI1 op x = CfInteger $ toInteger $ op x

  appI2 :: (a -> a -> a) -> a -> a -> CfVal
  appI2 op x y = CfInteger $ toInteger $ op x y

instance IntegralOp Int8
instance IntegralOp Int16
instance IntegralOp Int32
instance IntegralOp Int64
instance IntegralOp Word8
instance IntegralOp Word16
instance IntegralOp Word32
instance IntegralOp Word64

--------------------------------------------------------------------------------

cfNum :: I.Type
      -> I.ExpOp
      -> [CfVal]
      -> CfVal
cfNum ty op args = case args of
  [x]   -> case x of
    CfInteger l -> case ty of
      I.TyInt isz -> case isz of
        I.Int8        -> appI1 op1 (fromInteger l :: Int8)
        I.Int16       -> appI1 op1 (fromInteger l :: Int16)
        I.Int32       -> appI1 op1 (fromInteger l :: Int32)
        I.Int64       -> appI1 op1 (fromInteger l :: Int64)
      I.TyWord isz -> case isz of
        I.Word8       -> appI1 op1 (fromInteger l :: Word8)
        I.Word16      -> appI1 op1 (fromInteger l :: Word16)
        I.Word32      -> appI1 op1 (fromInteger l :: Word32)
        I.Word64      -> appI1 op1 (fromInteger l :: Word64)
      _ -> err $ "bad type to cfNum loc 1 "
    CfFloat   l -> CfFloat   (op1 l)
    CfDouble  l -> CfDouble  (op1 l)
    _           -> CfExpr    (I.ExpOp op [toExpr x])

  [x,y] -> case (x,y) of
    (CfInteger l, CfInteger r) -> case ty of
      I.TyInt isz -> case isz of
        I.Int8        -> appI2 op2 (fromInteger l :: Int8)
                                   (fromInteger r :: Int8)
        I.Int16       -> appI2 op2 (fromInteger l :: Int16)
                                   (fromInteger r :: Int16)
        I.Int32       -> appI2 op2 (fromInteger l :: Int32)
                                   (fromInteger r :: Int32)
        I.Int64       -> appI2 op2 (fromInteger l :: Int64)
                                   (fromInteger r :: Int64)
      I.TyWord isz -> case isz of
        I.Word8       -> appI2 op2 (fromInteger l :: Word8)
                                   (fromInteger r :: Word8)
        I.Word16      -> appI2 op2 (fromInteger l :: Word16)
                                   (fromInteger r :: Word16)
        I.Word32      -> appI2 op2 (fromInteger l :: Word32)
                                   (fromInteger r :: Word32)
        I.Word64      -> appI2 op2 (fromInteger l :: Word64)
                                   (fromInteger r :: Word64)
      _ -> err "bad type to cfNum loc 2"
    (CfFloat   l, CfFloat r)   -> CfFloat   (op2 l r)
    (CfDouble l,  CfDouble r)  -> CfDouble  (op2 l r)
    _                          -> CfExpr    (I.ExpOp op [toExpr x, toExpr y])

  _ -> err "wrong num args to cfNum"
  where
  op2 :: Num a => a -> a -> a
  op2 = case op of
    I.ExpMul    -> (*)
    I.ExpAdd    -> (+)
    I.ExpSub    -> (-)
    _ -> err "bad op to cfNum loc 3"
  op1 :: Num a => a -> a
  op1 = case op of
    I.ExpNegate -> negate
    I.ExpAbs    -> abs
    I.ExpSignum -> signum
    _ -> err "bad op to cfNum loc 4"

cfIntOp2 :: I.Type -> I.ExpOp -> [CfVal] -> CfVal
cfIntOp2 ty iOp [CfInteger l, CfInteger r] = case ty of
  I.TyInt isz -> case isz of
    I.Int8        -> appI2 op2 (fromInteger l :: Int8)
                               (fromInteger r :: Int8)
    I.Int16       -> appI2 op2 (fromInteger l :: Int16)
                               (fromInteger r :: Int16)
    I.Int32       -> appI2 op2 (fromInteger l :: Int32)
                               (fromInteger r :: Int32)
    I.Int64       -> appI2 op2 (fromInteger l :: Int64)
                               (fromInteger r :: Int64)
  I.TyWord isz -> case isz of
    I.Word8       -> appI2 op2 (fromInteger l :: Word8)
                               (fromInteger r :: Word8)
    I.Word16      -> appI2 op2 (fromInteger l :: Word16)
                               (fromInteger r :: Word16)
    I.Word32      -> appI2 op2 (fromInteger l :: Word32)
                               (fromInteger r :: Word32)
    I.Word64      -> appI2 op2 (fromInteger l :: Word64)
                               (fromInteger r :: Word64)
  _ -> err "bad type to cfIntOp2 loc 1"

  where
  op2 :: Integral a => a -> a -> a
  op2 = case iOp of
    I.ExpDiv -> quot
    -- Haskell's `rem` matches C ISO 1999 semantics of the remainder having the
    -- same sign as the dividend.
    I.ExpMod -> rem
    _ -> err "bad op to cfIntOp2"

cfIntOp2 _ iOp [x, y] = CfExpr (I.ExpOp iOp [toExpr x, toExpr y])
cfIntOp2 _ _ _        = err "wrong number of args to cfOp2"

--------------------------------------------------------------------------------

-- | Constant folding for unary operations that require a floating instance.
cfFloating :: I.ExpOp
           -> [CfVal]
           -> CfVal
cfFloating op args = case args of
  [x]   -> case x of
             CfFloat f  -> CfFloat  (op1 f)
             CfDouble d -> CfDouble (op1 d)
             _          -> CfExpr   (I.ExpOp op [toExpr x])
  [x,y] -> case (x,y) of
             (CfFloat l,  CfFloat r)  -> CfFloat (op2 l r)
             (CfDouble l, CfDouble r) -> CfDouble (op2 l r)
             _                        -> CfExpr   (I.ExpOp op [toExpr x
                                                              , toExpr y])
  _     -> err "wrong number of args to cfFloating"
  where
  op1 :: Floating a => a -> a
  op1 = case op of
    I.ExpRecip   -> recip
    I.ExpFExp    -> exp
    I.ExpFSqrt   -> sqrt
    I.ExpFLog    -> log
    I.ExpFSin    -> sin
    I.ExpFCos    -> cos
    I.ExpFTan    -> tan
    I.ExpFAsin   -> asin
    I.ExpFAcos   -> acos
    I.ExpFAtan   -> atan
    I.ExpFSinh   -> sinh
    I.ExpFCosh   -> cosh
    I.ExpFTanh   -> tanh
    I.ExpFAsinh  -> asinh
    I.ExpFAcosh  -> acosh
    I.ExpFAtanh  -> atanh
    _            -> err "wrong op1 to cfFloating"

  op2 :: Floating a => a -> a -> a
  op2 = case op of
    I.ExpFPow     -> (**)
    I.ExpFLogBase -> logBase
    _            -> err "wrong op2 to cfFloating"

cfFloatingB :: I.ExpOp
            -> [CfVal]
            -> CfVal
cfFloatingB op [x] = case x of
  CfFloat f  -> CfBool  (op' f)
  CfDouble d -> CfBool  (op' d)
  _          -> CfExpr  (I.ExpOp op [toExpr x])
  where
  op' :: RealFloat a => a -> Bool
  op' = case op of
    I.ExpIsNan _ -> isNaN
    I.ExpIsInf _ -> isInfinite
    _            -> err "wrong op to cfFloatingB"
cfFloatingB _ _ = err "wrong number of args to cfFloatingB"

--------------------------------------------------------------------------------

err :: String -> a
err msg = error $ "Ivory-Opts internal error: " ++ msg
