module Ivory.ModelCheck.Ivory2CVC4
--  ( modelCheckMod )
 where

import           Data.Word
import           Control.Monad
import           MonadLib (set, get)
import           Data.Monoid
import           Data.List
import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Proc   as P
import           Ivory.Opts.ConstFold (constFold)

import           Ivory.ModelCheck.CVC4
import           Ivory.ModelCheck.Monad

-- XXX testing
--import Debug.Trace

=--------------------------------------------------------------------------------

modelCheckMod :: I.Module -> ModelCheck ()
modelCheckMod m =
  -- We rely on constant folding having happend in the model-checker.  E.g., in
  -- computing the number of loop iterations.
  mapM_ (modelCheckProc . constFold) (getProcs $ I.modProcs m)
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
toParam (I.Typed t val) = void $ addEnvVar (toType t) (toVar val)
--------------------------------------------------------------------------------

-- | Symbolically execute statements, carrying the return requirements forward
-- to each location that there is a return statement.
toBody :: [I.Cond] -> I.Stmt -> ModelCheck ()
toBody ens stmt =
  let toBody' = toBody ens in
  case stmt of
    I.Assign t v exp       -> toAssign t v exp
    I.ReturnVoid           -> return ()
    I.Assert exp           -> addQuery =<< toExpr I.TyBool exp
    I.IfTE exp blk0 blk1   -> toIfTE ens exp blk0 blk1
    I.Local t v inits      -> toLocal t v inits
    I.Store t ptr exp      -> toStore t ptr exp
    I.AllocRef t ref name  -> toAlloc t ref name
    I.Deref t v ref        -> toDeref t v ref
    I.Loop v exp inc blk   -> toLoop v exp inc blk

toDeref :: I.Type -> I.Var -> I.Expr -> ModelCheck ()
toDeref t v ref = do
  v' <- addEnvVar (toType t) (toVar v)
  r  <- toRef t ref
  addInvariant (var v' .== var r)

toAlloc :: I.Type -> I.Var -> I.Name -> ModelCheck ()
toAlloc t ref name = do
  v' <- addEnvVar (toType t) (toVar ref)
  addInvariant (var v' .== var (toName name))

toStore :: I.Type -> I.Expr -> I.Expr -> ModelCheck ()
toStore t ptr exp = do
  p  <- toRef t ptr
  v' <- addEnvVar (toType t) p
  e  <- toExpr t exp
  addInvariant (var v' .== e)

toLocal :: I.Type -> I.Var -> I.Init -> ModelCheck ()
toLocal t v inits = do
  v' <- addEnvVar (toType t) (toVar v)
  is <- toInit inits
  addInvariant (var v' .== is)

toInit :: I.Init -> ModelCheck Expr
toInit init =
  case init of
--    I.InitZero       -> lit 0
    I.InitExpr t exp -> toExpr t exp

toAssign :: I.Type -> I.Var -> I.Expr -> ModelCheck ()
toAssign t v exp = do
  v' <- addEnvVar (toType t) (toVar v)
  e  <- toExpr t exp
  addInvariant $ (var v' .== e)

-- XXX Abstraction (to implement): If there is load/stores in the block, the we
-- don't care how many times it iterates.  It's pure.
toLoop :: I.Var -> I.Expr -> I.LoopIncr -> [I.Stmt] -> ModelCheck ()
toLoop v start end blk =
  mapM_ go ixs
  where
  go :: Integer -> ModelCheck ()
  go ix = do
    v'  <- addEnvVar (toType t) (toVar v)
    addInvariant (var v' .== intLit ix)
    mapM_ (toBody undefined) blk

  t = I.TyInt I.Int32

  loopData = loopIterations start end

  ixs | loopOp loopData == Incr
      = takeWhile (<= endVal loopData) $
          iterate (+ 1) (startVal loopData)
      | loopOp loopData == Decr
      = takeWhile (>= endVal loopData) $
          iterate (flip (-) 1) (startVal loopData)

toIfTE :: [I.Cond] -> I.Expr -> [I.Stmt] -> [I.Stmt] -> ModelCheck ()
toIfTE ens cond blk0 blk1 = do
  st  <- getState
  b   <- toExpr I.TyBool cond
  runBranch st b blk0
  st' <- joinState st
  runBranch st (not' b) blk1
  void (joinState st')
  where
  runBranch :: SymExecSt -> Expr -> [I.Stmt] -> ModelCheck ()
  runBranch st b blk = do
    resetSt st                   -- Empty state except environment
    mapM_ (toBody undefined) blk -- Body under the invariant
    branchSt b                   -- Make conditions under hypothesis b

--------------------------------------------------------------------------------

toExpr :: I.Type -> I.Expr -> ModelCheck Expr
toExpr t exp = case exp of
  I.ExpLit lit    -> return $
    case lit of
      I.LitInteger i -> toBoundedInt t i
      I.LitBool b    -> if b then T else F
  I.ExpVar v      -> lookupVar (toVar v) >>= return . var
  I.ExpOp op args -> toExprOp t op args

--------------------------------------------------------------------------------

assertBoundedVar :: I.Type -> Var -> Expr
assertBoundedVar t v = getBounds t

  where


  getBounds t = case t of
    I.TyWord w -> case w of
      Word8  -> fi (minBound :: Word8 , maxBound :: Word8)
      Word16 -> fi (minBound :: Word16, maxBound :: Word16)
      Word32 -> fi (minBound :: Word32, maxBound :: Word32)
      Word64 -> fi (minBound :: Word64, maxBound :: Word164)

    I.TyInt i -> case i of
      Int8  -> fi (minBound :: Int8 , maxBound :: Int8)
      Int16 -> fi (minBound :: Int16, maxBound :: Int16)
      Int32 -> fi (minBound :: Int32, maxBound :: Int32)
      Int64 -> fi (minBound :: Int64, maxBound :: Int164)

    _         -> 

  fi (a,b) = fromBounds (fromIntegral a, fromIntegral b)
  fromBounds (a,b) = (intLit a .<= v) .&& (intLit b .=> v)

toExprOp :: I.Type -> I.ExpOp -> [I.Expr] -> ModelCheck Expr
toExprOp t op args = case op of
  I.ExpEq t'      -> go t' (.==)
  I.ExpNot        -> toExpr I.TyBool arg0 >>= return . not'
  I.ExpAnd        -> go t (.&&)
  I.ExpOr         -> go t (.||)
  I.ExpLt orEq t' ->
    case orEq of
      True  -> go t' (.<=)
      False -> go t' (.<)
  I.ExpGt orEq t' ->
    case orEq of
      True  -> go t' (.>=)
      False -> go t' (.>)
  I.ExpMod        -> toMod t arg0 arg1
  I.ExpAdd        -> go t (.+)
  I.ExpSub        -> go t (.-)
  I.ExpNegate     ->
    let neg = I.ExpOp I.ExpSub [litOp t 0, arg0] in
    toExpr t neg
  _               -> error $ "no op " ++ show op

  where
  arg0 = args !! 0
  arg1 = args !! 1

  go t' op = do
    e0 <- toExpr t' arg0
    e1 <- toExpr t' arg1
    return (e0 `op` e1)

-- Abstraction: a % b (C semantics) implies
--
-- (   ((a => 0) && (a % b => 0) && (a % b < b) && (a % b <= a))
--  || ((a < 0)  && (a % b <= 0) && (a % b > b) && (a % b => a)))
--
-- make a fresh variable v == a % b
-- and assert the above for v then returning it.
toMod :: I.Type -> I.Expr -> I.Expr -> ModelCheck Expr
toMod t e0 e1 = do
  -- XXX
  v <- return . varOp =<< incReservedVar (toType t)
  let z = litOp t 0
  let disj0 =   (        (geqOp t e0 z)
                 `andOp` (geqOp t v  z)
                 `andOp` (leOp  t v  e1)
                 `andOp` (leqOp t v  e0)
                )
  let disj1 =   (        (leOp  t e0 z)
                 `andOp` (leqOp t v  z)
                 `andOp` (geOp  t v  e1)
                 `andOp` (geqOp  t v e0)
                )
  e <- toExpr t (disj0 `orOp` disj1)
  addInvariant e
  toExpr t v

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

toType :: I.Type -> Type
toType t =
  case t of
    I.TyBool     -> Bool
    (I.TyWord _) -> Integer
    (I.TyInt  _) -> Integer
    I.TyFloat    -> Real
    I.TyDouble   -> Real
    I.TyRef t'   -> toType t'
    -- _            -> error $ show t

toRef :: I.Type -> I.Expr -> ModelCheck Var
toRef t ref = do
  e <- toExpr t ref
  case e of
    Var v -> return v
    _     -> error $ "Unexpected expression " ++ show e
          ++ " to toRef."


data LoopOp = Incr | Decr deriving (Show, Read, Eq)
data Loop = Loop
  { startVal :: Integer
  , endVal   :: Integer
  , loopOp   :: LoopOp
  } deriving (Show, Read, Eq)

-- Compute the number of iterations in a loop.  Assume the constant folder has
-- run.
loopIterations :: I.Expr -> I.LoopIncr -> Loop
loopIterations start end = Loop (getLit start) (snd fromIncr) (fst fromIncr)
  where
  getLit e = case e of
    I.ExpLit l   -> case l of
      I.LitInteger i -> i
      _              -> err
    _            -> err

  fromIncr = case end of
               I.IncrTo e -> (Incr, getLit e)
               I.DecrTo e -> (Decr, getLit e)

  err = error "Error in loopIterations"

--------------------------------------------------------------------------------
-- Language construction helpers
binOp :: I.ExpOp -> I.Expr -> I.Expr -> I.Expr
binOp op e0 e1 = I.ExpOp op [e0, e1]

orOp, andOp :: I.Expr -> I.Expr -> I.Expr
orOp  = binOp I.ExpOr
andOp = binOp I.ExpAnd

leOp, leqOp, geOp, geqOp :: I.Type -> I.Expr -> I.Expr -> I.Expr
leOp  t = binOp (I.ExpLt False t)
leqOp t = binOp (I.ExpLt True  t)
geOp  t = binOp (I.ExpGt False t)
geqOp t = binOp (I.ExpGt True  t)

-- negOp :: I.Expr -> I.Expr
-- negOp e = I.ExpOp I.ExpNot [e]

-- addOp :: I.Expr -> I.Expr -> I.Expr
-- addOp e0 e1 = binOp I.ExpAdd e0 e1

-- subOp :: I.Expr -> I.Expr -> I.Expr
-- subOp e0 e1 = binOp I.ExpSub e0 e1

-- incrOp :: I.Type -> I.Expr -> I.Expr
-- incrOp t e = addOp e (litOp t 1)

-- decrOp :: I.Type -> I.Expr -> I.Expr
-- decrOp t e = subOp e (litOp t 1)

litOp :: I.Type -> Integer -> I.Expr
litOp t n = I.ExpLit e
  where
  e = case t of
        I.TyWord _ -> I.LitInteger n
        I.TyInt  _ -> I.LitInteger n
        I.TyFloat  -> I.LitFloat   (fromIntegral n)
        I.TyDouble -> I.LitDouble  (fromIntegral n)
        _          -> error $ "impossible lit in litOp: " ++ show t

varOp :: Var -> I.Expr
varOp = I.ExpVar . I.VarName

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

addEnvVar :: I.Type -> Var -> ModelCheck ()
addEnvVar t v = do
  updateEnv (toType t) v
  assertBoundedVar t v


--------------------------------------------------------------------------------

