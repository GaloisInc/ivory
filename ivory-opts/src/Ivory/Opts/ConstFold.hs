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

    I.ExpOp op args       -> cfOp ty op (map (cf ty) args)

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
    I.Loop ty v e incr blk'    ->
      let e' = opt ty e in
      case e' of
        I.ExpLit (I.LitBool b) ->
          if b then error $ "Constant folding evaluated True expression "
                          ++ "in a loop bound.  The loop will never terminate!"
               else error $ "Constant folding evaluated False expression "
                          ++ "in a loop bound.  The loop will never execute!"
        _                      ->
          snoc $ I.Loop ty v (opt ty e) (loopIncrFold (opt ty) incr)
                        (newFold blk')
    I.Break              -> snoc I.Break
    I.Forever b          -> snoc $ I.Forever (newFold b)
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

-- | Reconstruct an operator, folding away operations when possible.  It's
-- assumed that the argument list has already had constant folding applied.
cfOp :: I.Type -> I.ExpOp -> [I.Expr] -> I.Expr
cfOp ty op args =
  let args' = map toCfVal args in
  let dArgs = map destBoolLit args in
  let noop = I.ExpOp op args in
  case op of
    I.ExpEq _  -> toExpr (cfOrd2 op (==) args')
    I.ExpNeq _ -> toExpr (cfOrd2 op (/=) args')

    I.ExpCond
      | Just b <- arg0 dArgs
      -> if b then arg1 args else arg2 args
      | otherwise -> noop
    I.ExpGt orEq t
      | orEq      -> fromOrdChecks (toExpr (cfOrd2 op (>=) args'))
                                   (gteCheck t args')
      | otherwise -> fromOrdChecks (toExpr (cfOrd2 op (>) args'))
                                   (gtCheck t args')
    I.ExpLt orEq t
      | orEq      -> fromOrdChecks (toExpr (cfOrd2 op (<=) args'))
                                   (gteCheck t (reverse args'))
      | otherwise -> fromOrdChecks (toExpr (cfOrd2 op (<) args'))
                                   (gtCheck t (reverse args'))
    I.ExpNot
      | Just b <- arg0 dArgs
      -> I.ExpLit (I.LitBool (not b))
      | otherwise -> noop
    I.ExpAnd
      | Just lb <- arg0 dArgs
      , Just rb <- arg1 dArgs
      -> I.ExpLit (I.LitBool (lb && rb))
      | Just lb <- arg0 dArgs
      -> if lb then arg1 args else I.ExpLit (I.LitBool False)
      | Just rb <- arg1 dArgs
      -> if rb then arg0 args else I.ExpLit (I.LitBool False)
      | otherwise -> noop
    I.ExpOr
      | Just lb <- arg0 dArgs
      , Just rb <- arg1 dArgs
      -> I.ExpLit (I.LitBool (lb || rb))
      | Just lb <- arg0 dArgs
      -> if lb then I.ExpLit (I.LitBool True) else arg1 args
      | Just rb <- arg1 dArgs
      -> if rb then I.ExpLit (I.LitBool True) else arg0 args
      | otherwise -> noop

    I.ExpMul      -> toExpr (cfNum2 op (*)    args')
    I.ExpAdd      -> toExpr (cfNum2 op (+)    args')
    I.ExpSub      -> toExpr (cfNum2 op (-)    args')
    I.ExpNegate   -> toExpr (cfNum1 op negate args')
    I.ExpAbs      -> toExpr (cfNum1 op abs    args')
    I.ExpSignum   -> toExpr (cfNum1 op signum args')

    I.ExpDiv      -> toExpr (cfDiv args')
    I.ExpMod      -> toExpr (cfMod args')
    I.ExpRecip    -> toExpr (cfFloating1 op recip args')

    I.ExpIsNan _  -> toExpr (cfFloatingB op isNaN   args')
    I.ExpIsInf _  -> toExpr (cfFloatingB op isInfinite args')

    I.ExpFExp     -> toExpr (cfFloating1 op exp     args')
    I.ExpFSqrt    -> toExpr (cfFloating1 op sqrt    args')
    I.ExpFLog     -> toExpr (cfFloating1 op log     args')
    I.ExpFPow     -> toExpr (cfFloating2 op (**)    args')
    I.ExpFLogBase -> toExpr (cfFloating2 op logBase args')
    I.ExpFSin     -> toExpr (cfFloating1 op sin     args')
    I.ExpFCos     -> toExpr (cfFloating1 op cos     args')
    I.ExpFTan     -> toExpr (cfFloating1 op tan     args')
    I.ExpFAsin    -> toExpr (cfFloating1 op asin    args')
    I.ExpFAcos    -> toExpr (cfFloating1 op acos    args')
    I.ExpFAtan    -> toExpr (cfFloating1 op atan    args')
    I.ExpFSinh    -> toExpr (cfFloating1 op sinh    args')
    I.ExpFCosh    -> toExpr (cfFloating1 op cosh    args')
    I.ExpFTanh    -> toExpr (cfFloating1 op tanh    args')
    I.ExpFAsinh   -> toExpr (cfFloating1 op asinh   args')
    I.ExpFAcosh   -> toExpr (cfFloating1 op acosh   args')
    I.ExpFAtanh   -> toExpr (cfFloating1 op atanh   args')

    -- Unimplemented right now
    I.ExpToFloat _     -> noop
    I.ExpFromFloat _   -> noop
    I.ExpRoundF        -> noop
    I.ExpCeilF         -> noop
    I.ExpFloorF        -> noop
    I.ExpBitAnd        -> toExpr (cfBitAnd ty args')
    I.ExpBitOr         -> toExpr (cfBitOr ty args')
    I.ExpBitXor        -> noop
    I.ExpBitComplement -> noop
    I.ExpBitShiftL     -> noop
    I.ExpBitShiftR     -> noop

cfBitAnd :: I.Type -> [CfVal] -> CfVal
cfBitAnd ty [l,r]
  | ones ty  l = r
  | ones ty  r = l
  | zeros ty l = CfInteger 0
  | zeros ty r = CfInteger 0
  | otherwise  = CfExpr (I.ExpOp I.ExpBitAnd [toExpr l, toExpr r])
cfBitAnd _ _ = error "Wrong number of args to cfBitAnd in constant folder."

cfBitOr :: I.Type -> [CfVal] -> CfVal
cfBitOr ty [l,r]
  | zeros ty l = r
  | zeros ty r = l
  | ones ty  l = CfInteger 1
  | ones ty  r = CfInteger 1
  | otherwise  = CfExpr (I.ExpOp I.ExpBitOr [toExpr l, toExpr r])
cfBitOr _ _ = error "Wrong number of args to cfBitOr in constant folder."

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

-- | Convert to a constant-folded value.
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

fromOrdChecks :: I.Expr -> Maybe Bool -> I.Expr
fromOrdChecks expr =
  maybe expr (\b -> toExpr (CfBool b))

-- | Check if we're comparing the max or min bound for >=.
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
gteCheck _ _ = error "wrong number of args to gtCheck."

-- | Check if we're comparing the max or min bound for >.
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
gtCheck _ _ = error "wrong number of args to gtCheck."

-- | Apply a binary operation that requires an ord instance.
cfOrd2 :: I.ExpOp
       -> (forall a. Ord a => a -> a -> Bool)
       -> [CfVal]
       -> CfVal
cfOrd2 n op [l,r] = case (l,r) of
  (CfBool x,   CfBool y)    -> CfBool (op x y)
  (CfInteger x,CfInteger y) -> CfBool (op x y)
  (CfFloat x,  CfFloat y)   -> CfBool (op x y)
  (CfDouble x, CfDouble y)  -> CfBool (op x y)
  _                         -> CfExpr (I.ExpOp n [toExpr l,toExpr r])
cfOrd2 _ _ _ = error "wrong number of args to cfOrd2"

-- | Apply a unary operation that requires a num intance.
cfNum1 :: I.ExpOp
       -> (forall a. Num a => a -> a)
       -> ([CfVal] -> CfVal)
cfNum1 n op [x] = case x of
  CfInteger l -> CfInteger (op l)
  CfFloat l   -> CfFloat   (op l)
  CfDouble l  -> CfDouble  (op l)
  _           -> CfExpr    (I.ExpOp n [toExpr x])
cfNum1 _ _ _ = error "wrong number of args to cfNum1"

-- | Apply a binary operation that requires a num intance.
cfNum2 :: I.ExpOp
       -> (forall a. Num a => a -> a -> a)
       -> ([CfVal] -> CfVal)
cfNum2 n op [x,y] = case (x,y) of
  (CfInteger l,CfInteger r) -> CfInteger (op l r)
  (CfFloat l,  CfFloat r)   -> CfFloat   (op l r)
  (CfDouble l, CfDouble r)  -> CfDouble  (op l r)
  _                         -> CfExpr    (I.ExpOp n [toExpr x,toExpr y])
cfNum2 _ _ _ = error "wrong number of args to cfNum2"

-- | Constant folding for div on integer literals.  There's probably some
-- strange behavior here, without knowing the type where the computation should
-- take place.
cfDiv :: [CfVal] -> CfVal
cfDiv [l,r] = case (l,r) of
  (CfInteger x,CfInteger y) -> CfInteger (x `div` y)
  _                         -> CfExpr    (I.ExpOp I.ExpDiv [toExpr l,toExpr r])
cfDiv _ = error "wrong number of args to cfDiv"

-- | Constant folding for mod on integer literals.  We use Haskell's `rem` which
-- matches C ISO 1999 semantics of the remainder having the same sign as the
-- dividend.
cfMod :: [CfVal] -> CfVal
cfMod [l,r] = case (l,r) of
  (CfInteger x,CfInteger y) -> CfInteger (x `rem` y)
  _                         -> CfExpr    (I.ExpOp I.ExpMod [toExpr l, toExpr r])
cfMod _ = error "wrong number of args to cfMod"

-- | Constant folding for unary operations that require a floating instance.
cfFloating1 :: I.ExpOp
            -> (forall a. Floating a => a -> a)
            -> ([CfVal] -> CfVal)
cfFloating1 n op [x] = case x of
  CfFloat f  -> CfFloat  (op f)
  CfDouble d -> CfDouble (op d)
  _          -> CfExpr   (I.ExpOp n [toExpr x])
cfFloating1 _ _ _ = error "wrong number of args to cfFloating1"

cfFloatingB :: I.ExpOp
            -> (forall a. RealFloat a => a -> Bool)
            -> ([CfVal] -> CfVal)
cfFloatingB n op [x] = case x of
  CfFloat f  -> CfBool  (op f)
  CfDouble d -> CfBool  (op d)
  _          -> CfExpr   (I.ExpOp n [toExpr x])
cfFloatingB _ _ _ = error "wrong number of args to cfFloatingB"

-- | Constant folding for binary operations that require a floating instance.
cfFloating2 :: I.ExpOp
            -> (forall a. Floating a => a -> a -> a)
            -> ([CfVal] -> CfVal)
cfFloating2 n op [x,y] = case (x,y) of
  (CfFloat l, CfFloat r)   -> CfFloat  (op l r)
  (CfDouble l, CfDouble r) -> CfDouble (op l r)
  _                        -> CfExpr   (I.ExpOp n [toExpr x, toExpr y])
cfFloating2 _ _ _ = error "wrong number of args to cfFloating2"
