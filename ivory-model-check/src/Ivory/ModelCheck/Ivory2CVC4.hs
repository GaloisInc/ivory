module Ivory.ModelCheck.Ivory2CVC4
--  ( modelCheckMod )
 where

import           Data.Word
import           Control.Monad
import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Proc   as P

import           Ivory.ModelCheck.CVC4
import           Ivory.ModelCheck.Monad

--------------------------------------------------------------------------------
-- | Environment and variable store.

--------------------------------------------------------------------------------

modelCheckMod :: I.Module -> ModelCheck ()
modelCheckMod m = mapM_ modelCheckProc (getProcs $ I.modProcs m)
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
toParam (I.Typed t val) =
  void $ updateEnv (toType t) (toVar val)
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
    I.IfTE exp blk0 blk1   -> toIfTE ens exp blk0 blk1
    I.Local t v inits      -> toLocal t v inits
    I.Store t ptr exp      -> toStore t ptr exp
    I.AllocRef t ref name  -> toAlloc t ref name
    I.Deref t v ref         -> toDeref t v ref

toDeref :: I.Type -> I.Var -> I.Expr -> ModelCheck ()
toDeref t v ref = do
  v' <- updateEnv (toType t) (toVar v)
  ref' <- lookupVar (fromRef ref)
  addEqn (var v' .== var ref')

toAlloc :: I.Type -> I.Var -> I.Name -> ModelCheck ()
toAlloc t ref name = do
  v' <- updateEnv (toType t) (toVar ref)
  addEqn (var v' .== var (toName name))

toStore :: I.Type -> I.Expr -> I.Expr -> ModelCheck ()
toStore t ptr exp = do
  v' <- updateEnv (toType t) (fromRef ptr)
  addEqn (var v' .== toExpr t exp)

toLocal :: I.Type -> I.Var -> I.Init -> ModelCheck ()
toLocal t v inits = do
  v' <- updateEnv (toType t) (toVar v)
  addEqn (var v' .== toInit inits)

toInit :: I.Init -> Expr
toInit init =
  case init of
--    I.InitZero       -> lit 0
    I.InitExpr t exp -> toExpr t exp

toAssign :: I.Type -> I.Var -> I.Expr -> ModelCheck ()
toAssign t v exp = do
  v' <- updateEnv (toType t) (toVar v)
  addEqn $ (var v' .== toExpr t exp)

toIfTE :: [I.Cond] -> I.Expr -> [I.Stmt] -> [I.Stmt] -> ModelCheck ()
toIfTE ens exp blk0 blk1 = do
  let b = toExpr I.TyBool exp
  let branch bexp blk = do
        resetSt
        addEqn bexp
        mapM_ (toBody ens) blk
  -- Run each branch in a fresh context (keeping the environment).
  let tbnch = runMC (branch b blk0)
  let fbnch = runMC (branch (not' b) blk1)
  mergeSt (snd tbnch) (snd fbnch)

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
-- Helpers

toName :: I.Name -> Var
toName name =
  case name of
    I.NameSym s -> s
    I.NameVar v -> toVar v

toVar :: I.Var -> Var
toVar v =
  case v of
    I.VarName n     -> n
    I.VarInternal n -> n
    I.VarLitName n  -> n

fromRef :: I.Expr -> Var
fromRef ref =
  case ref of
      I.ExpVar v -> toVar v
      exp        -> error $ "Unexpected ref in fromRef: " ++ show exp

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

