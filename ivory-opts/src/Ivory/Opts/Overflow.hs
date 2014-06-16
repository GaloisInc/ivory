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

import Prelude hiding (max,min)
import Data.Word
import Data.Int

--------------------------------------------------------------------------------

overflowFold :: I.Proc -> I.Proc
overflowFold = procFold "ovf" (expFoldDefault arithAssert)

--------------------------------------------------------------------------------

type Bounds a = (a,a)

arithAssert :: I.Type -> I.Expr -> [I.Expr]
arithAssert ty e = case e of
  I.ExpLit i       -> litAssert ty i -- Should be impossible to fail, if all
                                     -- initializers have been accounted for.
  I.ExpOp op args  -> arithAssert' ty op args
  _                -> []

litAssert :: I.Type -> I.Literal -> [I.Expr]
litAssert ty lit = case lit of
  I.LitInteger i ->
    case ty of
      I.TyWord I.Word8  -> boundLit (minMax :: Bounds Word8)
      I.TyWord I.Word16 -> boundLit (minMax :: Bounds Word16)
      I.TyWord I.Word32 -> boundLit (minMax :: Bounds Word32)
      I.TyWord I.Word64 -> boundLit (minMax :: Bounds Word64)
      I.TyInt I.Int8    -> boundLit (minMax :: Bounds Int8)
      I.TyInt I.Int16   -> boundLit (minMax :: Bounds Int16)
      I.TyInt I.Int32   -> boundLit (minMax :: Bounds Int32)
      I.TyInt I.Int64   -> boundLit (minMax :: Bounds Int64)
      _                 -> []
      where
      boundLit (min,max) = fmap T.unwrapExpr $
        if fromIntegral min <= i && i <= fromIntegral max
         then [true]
         else [false]
  _ -> []

arithAssert' :: I.Type -> I.ExpOp -> [I.Expr] -> [I.Expr]
arithAssert' ty op args = fmap T.unwrapExpr $
  case op of
    I.ExpAdd -> case ty of
      I.TyWord I.Word8  -> sing $ addExprW (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> sing $ addExprW (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> sing $ addExprW (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> sing $ addExprW (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> sing $ addExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> sing $ addExprI (minMax :: Bounds Sint16)
      I.TyInt I.Int32   -> sing $ addExprI (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> sing $ addExprI (minMax :: Bounds Sint64)
      _                 -> []

    I.ExpSub -> case ty of
      I.TyWord I.Word8  -> sing $ subExprW (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> sing $ subExprW (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> sing $ subExprW (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> sing $ subExprW (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> sing $ subExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> sing $ subExprI (minMax :: Bounds Sint16)
      I.TyInt I.Int32   -> sing $ subExprI (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> sing $ subExprI (minMax :: Bounds Sint64)
      _                 -> []

    I.ExpMul -> case ty of
      I.TyWord I.Word8  -> sing $ mulExprW (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> sing $ mulExprW (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> sing $ mulExprW (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> sing $ mulExprW (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> sing $ mulExprI (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> sing $ mulExprI (minMax :: Bounds Sint16)
      I.TyInt I.Int32   -> sing $ mulExprI (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> sing $ mulExprI (minMax :: Bounds Sint64)
      _                 -> []

    I.ExpDiv -> case ty of
      I.TyWord I.Word8  -> sing $ divExpr (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> sing $ divExpr (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> sing $ divExpr (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> sing $ divExpr (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> sing $ divExpr (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> sing $ divExpr (minMax :: Bounds Sint16)
      I.TyInt I.Int32   -> sing $ divExpr (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> sing $ divExpr (minMax :: Bounds Sint64)
      _                 -> []

    I.ExpMod -> case ty of
      I.TyWord I.Word8  -> sing $ divExpr (minMax :: Bounds Uint8)
      I.TyWord I.Word16 -> sing $ divExpr (minMax :: Bounds Uint16)
      I.TyWord I.Word32 -> sing $ divExpr (minMax :: Bounds Uint32)
      I.TyWord I.Word64 -> sing $ divExpr (minMax :: Bounds Uint64)
      I.TyInt I.Int8    -> sing $ divExpr (minMax :: Bounds Sint8)
      I.TyInt I.Int16   -> sing $ divExpr (minMax :: Bounds Sint16)
      I.TyInt I.Int32   -> sing $ divExpr (minMax :: Bounds Sint32)
      I.TyInt I.Int64   -> sing $ divExpr (minMax :: Bounds Sint64)
      _                 -> []

    _ -> []

  where
  (e0, e1) = (args !! 0, args !! 1)
  sing = (: [])

  ----------------------------------------------------------

  addExprI :: forall t . (Num t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> T.IBool
  addExprI (min,max) =
    let w :: I.Expr -> t
        w  = T.wrapExpr in
        (w e0 >=? 0 .&& w e1 >=? 0 .&& max - w e0 >=? w e1)
    .|| (w e0 <=? 0 .&& w e1 <=? 0 .&& min - w e0 <=? w e1)
    .|| (signum (w e0) /=? signum (w e1))

  addExprW :: forall t. (Num t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> T.IBool
  addExprW (_,max) = do
    let w :: I.Expr -> t
        w  = T.wrapExpr
    max - w e0 >=? w e1

  ----------------------------------------------------------

  subExprI :: forall t. (Num t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> T.IBool
  subExprI (min,max) =
    let w :: I.Expr -> t
        w  = T.wrapExpr in
        (w e0 >=? 0 .&& w e1 <=? 0 .&& max + w e1 >=? w e0)
    .|| (w e0 <=? 0 .&& w e1 >=? 0 .&& min + w e1 <=? w e0)
    .|| (signum (w e0) ==? signum (w e1))

  subExprW :: forall t. (Num t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> T.IBool
  subExprW _ =
    let w :: I.Expr -> t
        w  = T.wrapExpr in
    (w e0 :: t) >=? w e1

  ----------------------------------------------------------

  minNegOne :: forall t. (Num t, IvoryIntegral t, IvoryOrd t, T.IvoryExpr t)
            => t -> t -> t -> T.IBool
  minNegOne min x y = x /=? min .|| y /=? (-1)

  mulExprI :: forall t. (Num t, IvoryIntegral t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> T.IBool
  mulExprI (min,max) =
    let w :: I.Expr -> t
        w  = T.wrapExpr in
        (minNegOne min (w e0) (w e1) .|| minNegOne min (w e1) (w e0))
    .&& (    w e0 ==? 0
         .|| w e1 ==? 0
         .|| (    max `iDiv` abs (w e0) >=? abs (w e1)
              .&& min `iDiv` abs (w e0) <=? abs (w e1)))

  mulExprW :: forall t. (Num t, IvoryIntegral t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> T.IBool
  mulExprW (_,max) =
    let w :: I.Expr -> t
        w  = T.wrapExpr in
    (w e0 ==? 0) .|| (max `iDiv` w e0 >=? w e1)

  ----------------------------------------------------------

  divExpr :: forall t. (Num t, IvoryIntegral t, IvoryOrd t, T.IvoryExpr t)
           => Bounds t -> T.IBool
  divExpr (min,_) =
    let w :: I.Expr -> t
        w  = T.wrapExpr in
    w e1 /=? (0 :: t) .&& minNegOne min (w e0) (w e1)

----------------------------------------------------------

minMax :: forall t . (Bounded t) => Bounds t
minMax = (minBound :: t, maxBound :: t)
