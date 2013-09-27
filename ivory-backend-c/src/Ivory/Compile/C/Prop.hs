{-# LANGUAGE QuasiQuotes #-}

-- | Rewrite ensures variable with return expression.

module Ivory.Compile.C.Prop where

import qualified Ivory.Language.Syntax as I

--------------------------------------------------------------------------------

-- | Replace ensures variable with the return expression.
ensTrans :: I.Expr -> I.Expr -> I.Expr
ensTrans retE = loop
  where
  loop e = case e of
    I.ExpSym{}             -> e
    I.ExpVar v             -> if v == I.retval then retE else e
    I.ExpLit{}             -> e
    I.ExpOp op args        -> I.ExpOp op (map loop args)
    I.ExpLabel t e0 s      -> I.ExpLabel t (loop e0) s
    I.ExpIndex t e0 t1 e1  -> I.ExpIndex t (loop e0) t1 (loop e1)
    I.ExpSafeCast t e0     -> I.ExpSafeCast t (loop e0)
    I.ExpToIx e0 maxSz     -> I.ExpToIx (loop e0) maxSz
    I.ExpAddrOfGlobal{}    -> e
    I.ExpDynArrayLength e0 -> I.ExpDynArrayLength (loop e0)
    I.ExpDynArrayData t e0 -> I.ExpDynArrayData t (loop e0)

--------------------------------------------------------------------------------
