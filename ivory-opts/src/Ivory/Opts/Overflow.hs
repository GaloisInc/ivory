{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------
-- | Assert folding: add asserts expression overflow/underflow.
--------------------------------------------------------------------------------

module Ivory.Opts.Overflow
  ( overflowFold
  ) where

import Ivory.Opts.AssertFold

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Language.IBool as T
import qualified Ivory.Language.Type as T
import Ivory.Language

import Control.Monad (mzero)
import Prelude hiding (max,min)
import Data.Maybe

--------------------------------------------------------------------------------

overflowFold :: I.Proc -> I.Proc
overflowFold = procFold (expFoldDefault arithAssert)

--------------------------------------------------------------------------------

type Bounds a = (a,a)

arithAssert :: Asserter
arithAssert ty e = case e of
  I.ExpOp op args -> arithAssert' ty op args
  _               -> Nothing

arithAssert' :: I.Type -> I.ExpOp -> [I.Expr] -> Maybe I.Expr
arithAssert' ty op args = fmap T.unwrapExpr $
  case op of
    I.ExpAdd -> case ty of
      I.TyWord I.Word8  -> addExprW (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> addExprW (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> addExprW (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> addExprW (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> addExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> addExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int32   -> addExprI (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> addExprI (minMax :: Bounds Sint64)
      _                 -> Nothing

    I.ExpSub -> case ty of
      I.TyWord I.Word8  -> subExprW (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> subExprW (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> subExprW (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> subExprW (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> subExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> subExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int32   -> subExprI (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> subExprI (minMax :: Bounds Sint64)
      _                 -> Nothing

    I.ExpMul -> case ty of
      I.TyWord I.Word8  -> mulExprW (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> mulExprW (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> mulExprW (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> mulExprW (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> mulExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> mulExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int32   -> mulExprI (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> mulExprI (minMax :: Bounds Sint64)
      _                 -> Nothing

    I.ExpDiv -> case ty of
      I.TyWord I.Word8  -> divExpr (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> divExpr (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> divExpr (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> divExpr (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> divExpr (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> divExpr (minMax :: Bounds Sint8)
      I.TyInt I.Int32   -> divExpr (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> divExpr (minMax :: Bounds Sint64)
      _                 -> Nothing

    I.ExpMod -> case ty of
      I.TyWord I.Word8  -> divExpr (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> divExpr (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> divExpr (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> divExpr (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> divExpr (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> divExpr (minMax :: Bounds Sint8)
      I.TyInt I.Int32   -> divExpr (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> divExpr (minMax :: Bounds Sint64)
      _                 -> Nothing

    _ -> Nothing

  where
  minMax :: forall t . (Bounded t) => Bounds t
  minMax = (minBound :: t, maxBound :: t)

  getArgs :: Maybe (I.Expr,I.Expr)
  getArgs = case args of
    [e0,e1] -> return (e0,e1)
    _       -> mzero

  ----------------------------------------------------------

  addExprI :: forall t . (Num t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> Maybe T.IBool
  addExprI m@(min,_) = do
    let w :: I.Expr -> t
        w  = T.wrapExpr
    (e0,e1) <- getArgs
    return $ ((w e0 >?  0) .&& (w e1 >?  0) .&& fromJust (addExprW m))
         .|| ((w e0 >=? 0) .&& (w e1 <=? 0))
         .|| ((w e0 <=? 0) .&& (w e1 >=? 0))
         .|| ((w e0 <?  0) .&& (w e1 <?  0) .&& (min - w e0 <=? w e1))

  addExprW :: forall t. (Num t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> Maybe T.IBool
  addExprW (_,max) = do
    let w :: I.Expr -> t
        w  = T.wrapExpr
    (e0,e1) <- getArgs
    return (max - w e0  >=? w e1)

  ----------------------------------------------------------

  subExprI :: forall t. (Num t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> Maybe T.IBool
  subExprI (_,max) = do
    let w :: I.Expr -> t
        w  = T.wrapExpr
    (e0,e1) <- getArgs
    return $ ((w e0 >?  0) .&& (w e1 <?  0) .&& ((max - w e0) + w e1 >=? 0))
         .|| ((w e1 >?  0) .&& (w e0 <?  0) .&& ((max - w e1) + w e0 >=? 0))
         .|| ((w e0 >=? 0) .&& (w e1 >=? 0))
         .|| ((w e0 <=? 0) .&& (w e1 <=? 0))

  subExprW :: forall t. (Num t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> Maybe T.IBool
  subExprW _ = do
    let w :: I.Expr -> t
        w  = T.wrapExpr
    (e0,e1) <- getArgs
    return ((w e0 :: t) >=? w e1)

  ----------------------------------------------------------

  mulExprI :: forall t. (Num t, IvoryIntegral t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> Maybe T.IBool
  mulExprI m@(min,_) = do
    let w :: I.Expr -> t
        w  = T.wrapExpr
    (e0,e1) <- getArgs
    return $ ((w e0 >=? 0) .&& (w e1 >=? 0) .&& fromJust (mulExprW m))
         .|| ((w e0 <?  0) .&& (w e1 <?  0) .&& fromJust (mulExprW m))
         .|| ((w e0 <?  0) .&& (w e1 >?  0) .&& (w e1 <=? min `iDiv` w e0))
         .|| ((w e0 >?  0) .&& (w e1 <?  0) .&& (w e0 <=? min `iDiv` w e1))

  mulExprW :: forall t. (Num t, IvoryIntegral t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> Maybe T.IBool
  mulExprW (_,max) = do
    let w :: I.Expr -> t
        w  = T.wrapExpr
    (e0,e1) <- getArgs
    return ((w e0 ==? 0) .|| (max `iDiv` w e0 >=? w e1))

  ----------------------------------------------------------

  divExpr :: forall t. (Num t, IvoryIntegral t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> Maybe T.IBool
  divExpr (_,_) = do
    let w :: I.Expr -> t
        w  = T.wrapExpr
    (_,e1) <- getArgs
    return (w e1 /=? (0 :: t))

----------------------------------------------------------
