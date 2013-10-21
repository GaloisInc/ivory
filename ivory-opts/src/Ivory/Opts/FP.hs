{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

--------------------------------------------------------------------------------
-- | Assert folding: add asserts expression NaN and Infinite floats.
--------------------------------------------------------------------------------

module Ivory.Opts.FP
  ( fpFold
  ) where

import Ivory.Opts.AssertFold
import Ivory.Opts.Utils

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------

fpFold :: I.Proc -> I.Proc
fpFold = procFold expFold

--------------------------------------------------------------------------------

expFold :: I.Type -> I.Expr -> Assert ()
expFold ty e = case e of
  I.ExpSym{} -> return ()
  I.ExpVar{} -> return ()
  I.ExpLit{} -> return ()
  eo@(I.ExpOp op args) -> do
    putExpr (fpAssert ty eo)
    mapM_ (expFold $ expOpType ty op) args
  I.ExpLabel ty' e0 _  -> expFold  ty' e0
  I.ExpIndex tIdx eIdx tArr eArr -> do
    expFold tIdx eIdx
    expFold tArr eArr
  I.ExpSafeCast ty' e0  -> expFold ty' e0
  I.ExpToIx e0 _        -> expFold (I.TyInt I.Int32) e0
  I.ExpAddrOfGlobal{}   -> return ()

fpAssert :: I.Type -> I.Expr -> Maybe I.Expr
fpAssert ty e = case ty of
  I.TyFloat   -> Just $ mkAssert ty e
  I.TyDouble  -> Just $ mkAssert ty e
  _           -> Nothing

mkAssert :: I.Type -> I.Expr -> I.Expr
mkAssert ty e = I.ExpOp I.ExpAnd
  [ I.ExpOp I.ExpNot [I.ExpOp (I.ExpIsNan ty) [e]]
  , I.ExpOp I.ExpNot [I.ExpOp (I.ExpIsInf ty) [e]] ]
