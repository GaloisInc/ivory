--------------------------------------------------------------------------------
-- | Division by zero checks.
--------------------------------------------------------------------------------

module Ivory.Opts.DivZero
  ( divZeroFold
  ) where

import Ivory.Opts.AssertFold
import Ivory.Opts.Utils

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------

divZeroFold :: I.Proc -> I.Proc
divZeroFold = procFold expFold

--------------------------------------------------------------------------------

expFold :: I.Type -> I.Expr -> Assert ()
expFold ty e = case e of
  I.ExpSym{} -> return ()
  I.ExpVar{} -> return ()
  I.ExpLit{} -> return ()
  I.ExpOp op args -> do
    putExpr (divAssert ty op args)
    mapM_ (expFold $ expOpType ty op) args
  I.ExpLabel ty' e0 _  -> expFold  ty' e0
  I.ExpIndex tIdx eIdx tArr eArr -> do
    expFold tIdx eIdx
    expFold tArr eArr
  I.ExpSafeCast ty' e0 -> expFold ty' e0

--------------------------------------------------------------------------------

divAssert :: I.Type -> I.ExpOp -> [I.Expr] -> Maybe I.Expr
divAssert ty op args =
  case (op,args) of
    (I.ExpDiv,[_,r])      -> ma r
    (I.ExpMod,[_,r])      -> ma r
    (I.ExpRecip,[e])      -> ma e
    (I.ExpFLog,[e])       -> ma e
    (I.ExpFLogBase,[_,r]) -> ma r
    _                     -> Nothing
  where
  ma x = return $ I.ExpOp (I.ExpNeq ty) [x,zeroExp]
  zeroExp = case ty of
              I.TyInt  _ -> I.ExpLit (I.LitInteger 0)
              I.TyWord _ -> I.ExpLit (I.LitInteger 0)
              I.TyFloat  -> I.ExpLit (I.LitFloat 0)
              I.TyDouble -> I.ExpLit (I.LitDouble 0)
              _          -> error $
                "unrecognized expression in Div by zero checking: " ++ show ty
                ++ ", op: " ++ show op ++ ", args: " ++ show args

--------------------------------------------------------------------------------
