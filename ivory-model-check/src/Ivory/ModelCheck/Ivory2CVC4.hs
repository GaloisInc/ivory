{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE NamedFieldPuns #-}

module Ivory.ModelCheck.Ivory2CVC4
  ( modelCheck ) where

import           Data.Word
import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Proc   as P

import Ivory.ModelCheck.CVC4
import Ivory.ModelCheck.Monad

-- XXX testing
import Ivory.Language hiding (assert, true, false)
import qualified Ivory.Language as L
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------
-- | Environment and variable store.

--------------------------------------------------------------------------------

modelCheck :: I.Module -> ModelCheck ()
modelCheck m = mapM_ modelCheckProc (getProcs $ I.modProcs m)
  where
  getProcs ps = I.public ps ++ I.private ps

--------------------------------------------------------------------------------

modelCheckProc :: I.Proc -> ModelCheck ()
modelCheckProc I.Proc { I.procSym      = sym
                      , I.procRetTy    = ret
                      , I.procArgs     = args
                      , I.procBody     = body
                      , I.procRequires = requires
                      , I.procEnsures  = ensures
                      }
    -- XXX ignore requires/ensures for now
  = --let ens  = map I.getEnsure ensures
    --let reqs = map (toRequire . I.getRequire) requires
    do
  mapM_ toParam args
  mapM_ (toBody undefined) body

--------------------------------------------------------------------------------

-- XXX implicit requirements here on sizes based on types.
toParam :: I.Typed I.Var -> ModelCheck ()
toParam (I.Typed t val) = do
  v' <- updateEnv (toVar val)
  addDecl (varDecl v' (toType t))

--------------------------------------------------------------------------------

-- | Symbolically execute statements, carrying the return requirements forward
-- to each location that there is a return statement.
toBody :: [I.Cond] -> I.Stmt -> ModelCheck ()
toBody ens stmt =
  let toBody' = toBody ens in
  case stmt of
    I.Assign t v exp       -> toAssign t v exp
    I.ReturnVoid           -> return ()
    I.Assert exp           -> addQuery (toExpr I.TyBool exp)
    I.IfTE exp blk0 blk1   -> do
      let b = toExpr I.TyBool exp
      let branch bexp blk = do
            resetSt
            addEqn bexp
            mapM_ toBody' blk
      -- Run each branch in a fresh context (keeping the environment).
      let tbnch = runMC (branch b blk0)
      let fbnch = runMC (branch (not' b) blk1)
      mergeSt (snd tbnch) (snd fbnch)

--------------------------------------------------------------------------------


toVar :: I.Var -> Var
toVar v =
  case v of
    I.VarName n     -> n
    I.VarInternal n -> n
    I.VarLitName n  -> n

--------------------------------------------------------------------------------

toAssign :: I.Type -> I.Var -> I.Expr -> ModelCheck ()
toAssign t v exp = do
  v' <- updateEnv (toVar v)
  addDecl $ varAssign v' (toType t) (toExpr t exp)

--------------------------------------------------------------------------------

toExpr :: I.Type -> I.Expr -> Expr
toExpr t exp =
  case exp of
    I.ExpLit lit    ->
      case lit of
        I.LitInteger i -> intLit i
        I.LitBool b    -> if b then T else F
    I.ExpVar v      -> var (toVar v)
    I.ExpOp op args -> toExprOp t op args

--------------------------------------------------------------------------------

toExprOp :: I.Type -> I.ExpOp -> [I.Expr] -> Expr
toExprOp t op args =
  case op of
    I.ExpEq t'      -> go t' (.==)
    I.ExpLt orEq t' ->
      case orEq of
        True  -> go t' (.<=)
        False -> go t' (.<)
    I.ExpGt orEq t' ->
      case orEq of
        True  -> go t' (.>=)
        False -> go t' (.>)

  where
  arg0 = args !! 0
  arg1 = args !! 1
  go t' op = (toExpr t' arg0) `op` (toExpr t' arg1)

--------------------------------------------------------------------------------

toType :: I.Type -> Type
toType t =
  case t of
    I.TyBool     -> Bool
    (I.TyWord _) -> Integer
    (I.TyInt  _) -> Integer
    I.TyFloat    -> Real
    I.TyDouble   -> Real

-- toRequire = undefined
-- toRequire :: I.Cond -> C.BlockItem
-- toRequire = toAssertion id "REQUIRES"

-- -- | Takes the return expression, the condition, and returns a 'BlockItem'.
-- toEnsure :: I.Expr -> I.Cond -> C.BlockItem
-- toEnsure retE = toAssertion (loop retE) "ENSURES"
--   where
--   -- Replace ensures variable with the return expression.
--   loop :: I.Expr -> I.Expr -> I.Expr
--   loop e = case e of
--     I.ExpSym{}             -> e
--     I.ExpVar v             -> if v == I.retval then retE else e
--     I.ExpLit{}             -> e
--     I.ExpOp op args        -> I.ExpOp op (map loop args)
--     I.ExpLabel t e0 s      -> I.ExpLabel t (loop e0) s
--     I.ExpIndex t e0 t1 e1  -> I.ExpIndex t (loop e0) t1 (loop e1)
--     I.ExpSafeCast t e0     -> I.ExpSafeCast t (loop e0)
--     I.ExpToIx e0 maxSz     -> I.ExpToIx (loop e0) maxSz
--     I.ExpAddrOfGlobal{}    -> e

-- toAssertion :: (I.Expr -> I.Expr) -> String -> I.Cond -> C.BlockItem
-- toAssertion trans call cond = C.BlockStm $
--   case cond of
--     I.CondBool e          ->
--       [cstm| $id:call($exp:(toExpr I.TyBool (trans e))); |]
--     I.CondDeref t e var c ->
--       let res = (toBody []) (I.Deref t var (trans e)) in
--       let c1  = toAssertion trans call c in
--       [cstm| { $items:res $item:c1 } |]

--------------------------------------------------------------------------------

fooProc :: Def ('[Uint8, Uint8] :-> ())
fooProc = proc "foo" $ \y x -> body $ do
  -- z <- assign x
  -- L.assert (z <? 3)
  ifte_ (y <? 3)
    (do ifte_ (y ==? 3)
              (L.assert L.false)
              retVoid)
    (do z <- assign x
        L.assert (z >=? 3))
  retVoid

m :: Module
m = package "foo" (incl fooProc)

run = modelCheck m
