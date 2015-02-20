{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ivory.Opts.CSE (cseFold) where

import Control.Applicative
import qualified Data.DList as D
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Reify
import Data.Traversable
import Ivory.Language.Array (ixRep)
import qualified Ivory.Language.Syntax as AST
import MonadLib (WriterT, StateT, Id, get, set, sets, sets_, put, collect, lift, runM)
import Prelude hiding (foldr, mapM, mapM_)
import System.IO.Unsafe (unsafePerformIO)

-- | Find each common sub-expression and extract it to a new variable,
-- making any sharing explicit. However, this function should never move
-- evaluation of an expression earlier than it would have occurred in
-- the source program, which means that sometimes an expression must be
-- re-computed on each of several execution paths.
cseFold :: AST.Proc -> AST.Proc
cseFold def = def
  { AST.procBody = reconstruct $ unsafePerformIO $ reifyGraph $ AST.procBody def }

-- | Variable assignments emitted so far.
data Bindings = Bindings
  { availableBindings :: (Map (Unique, AST.Type) Int)
  , unusedBindings :: IntSet
  , totalBindings :: Int
  }

-- | A monad for emitting both source-level statements as well as
-- assignments that capture common subexpressions.
--
-- Note that the StateT is outside the WriterT so that we can first run
-- the StateT, getting a set of expressions which shouldn't be assigned
-- to fresh names, and only then decide whether to write out Assign
-- statements. See the comment in `updateFacts`.
type BlockM a = StateT Bindings (WriterT (D.DList AST.Stmt) Id) a

-- | We perform CSE on expressions but also across all the blocks in a
-- procedure.
data CSE t
  = CSEExpr (ExprF t)
  | CSEBlock (BlockF t)
  deriving (Show, Eq, Ord, Functor)

-- | During CSE, we replace recursive references to an expression with a
-- unique ID for that expression.
data ExprF t
  = ExpSimpleF AST.Expr
    -- ^ For expressions that cannot contain any expressions recursively.
  | ExpLabelF AST.Type t String
  | ExpIndexF AST.Type t AST.Type t
  | ExpToIxF t Integer
  | ExpSafeCastF AST.Type t
  | ExpOpF AST.ExpOp [t]
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable)

instance MuRef AST.Expr where
  type DeRef AST.Expr = CSE
  mapDeRef child e = CSEExpr <$> case e of
    AST.ExpSym{} -> pure $ ExpSimpleF e
    AST.ExpVar{} -> pure $ ExpSimpleF e
    AST.ExpLit{} -> pure $ ExpSimpleF e
    AST.ExpLabel ty ex nm -> ExpLabelF <$> pure ty <*> child ex <*> pure nm
    AST.ExpIndex ty1 ex1 ty2 ex2 -> ExpIndexF <$> pure ty1 <*> child ex1 <*> pure ty2 <*> child ex2
    AST.ExpToIx ex bound -> ExpToIxF <$> child ex <*> pure bound
    AST.ExpSafeCast ty ex -> ExpSafeCastF ty <$> child ex
    AST.ExpOp op args -> ExpOpF op <$> traverse child args
    AST.ExpAddrOfGlobal{} -> pure $ ExpSimpleF e
    AST.ExpMaxMin{} -> pure $ ExpSimpleF e
    AST.ExpSizeOf{} -> pure $ ExpSimpleF e

-- | Convert a flattened expression back to a real expression.
toExpr :: ExprF AST.Expr -> AST.Expr
toExpr (ExpSimpleF ex) = ex
toExpr (ExpLabelF ty ex nm) = AST.ExpLabel ty ex nm
toExpr (ExpIndexF ty1 ex1 ty2 ex2) = AST.ExpIndex ty1 ex1 ty2 ex2
toExpr (ExpToIxF ex bound) = AST.ExpToIx ex bound
toExpr (ExpSafeCastF ty ex) = AST.ExpSafeCast ty ex
toExpr (ExpOpF op args) = AST.ExpOp op args

-- | Wrap the second type in either TyRef or TyConstRef, according to
-- whether the first argument was a constant ref.
copyConst :: AST.Type -> AST.Type -> AST.Type
copyConst (AST.TyRef _) ty = AST.TyRef ty
copyConst (AST.TyConstRef _) ty = AST.TyConstRef ty
copyConst ty _ = error $ "Ivory.Opts.CSE.copyConst: expected a Ref type but got " ++ show ty

-- | Label all sub-expressions with the type at which they're used,
-- assuming that this expression is used at the given type.
labelTypes :: AST.Type -> ExprF k -> ExprF (k, AST.Type)
labelTypes _ (ExpSimpleF e) = ExpSimpleF e
labelTypes resty (ExpLabelF ty ex nm) = ExpLabelF ty (ex, copyConst resty ty) nm
labelTypes resty (ExpIndexF ty1 ex1 ty2 ex2) = ExpIndexF ty1 (ex1, copyConst resty ty1) ty2 (ex2, ty2)
labelTypes _ (ExpToIxF ex bd) = ExpToIxF (ex, ixRep) bd
labelTypes _ (ExpSafeCastF ty ex) = ExpSafeCastF ty (ex, ty)
labelTypes ty (ExpOpF op args) = ExpOpF op $ case op of
  AST.ExpEq t -> map (`atType` t) args
  AST.ExpNeq t -> map (`atType` t) args
  AST.ExpCond -> let (cond, rest) = splitAt 1 args in map (`atType` AST.TyBool) cond ++ map (`atType` ty) rest
  AST.ExpGt _ t -> map (`atType` t) args
  AST.ExpLt _ t -> map (`atType` t) args
  AST.ExpIsNan t  -> map (`atType` t) args
  AST.ExpIsInf t  -> map (`atType` t) args
  _ -> map (`atType` ty) args
  where
  atType = (,)

-- | Like ExprF, we replace recursive references to
-- blocks/statements/expressions with unique IDs.
--
-- Note that we treat statements as a kind of block, because extracting
-- assignments for the common subexpressions in a statement can result
-- in multiple statements, which looks much like a block.
--
-- We're not performing CSE on all recursive references yet. Unsupported
-- statement types should generate correct, but unoptimized, code. This
-- list can be extended as needed, though.
data BlockF t
  = StmtSimple AST.Stmt
    -- ^ For statements that cannot contain any expressions, or that we don't want to CSE.
  | StmtIfTE t t t
  | StmtDeref AST.Type AST.Var t
  | StmtStore AST.Type t t
  | StmtAssign AST.Type AST.Var t
  | StmtCall AST.Type (Maybe AST.Var) AST.Name [AST.Typed t]
  | StmtLocal AST.Type AST.Var (InitF t)
  | StmtRefCopy AST.Type t t
  | StmtLoop AST.Var t (LoopIncrF t) t
  | StmtForever t
  | Block [t]
  deriving (Show, Eq, Ord, Functor)

data LoopIncrF t
  = IncrTo t
  | DecrTo t
  deriving (Show, Eq, Ord, Functor)

data InitF t
  = InitZero
  | InitExpr AST.Type t
  | InitStruct [(String, InitF t)]
  | InitArray [InitF t]
  deriving (Show, Eq, Ord, Functor)

instance MuRef AST.Stmt where
  type DeRef AST.Stmt = CSE
  mapDeRef child stmt = CSEBlock <$> case stmt of
    AST.IfTE cond tb fb -> StmtIfTE <$> child cond <*> child tb <*> child fb
    AST.Deref ty var ex -> StmtDeref ty var <$> child ex
    AST.Store ty lhs rhs -> StmtStore ty <$> child lhs <*> child rhs
    AST.Assign ty var ex -> StmtAssign ty var <$> child ex
    AST.Call ty mv nm args -> StmtCall ty mv nm <$> traverse (\ (AST.Typed argTy argEx) -> AST.Typed argTy <$> child argEx) args
    AST.Local ty var initex -> StmtLocal ty var <$> mapInit initex
    AST.RefCopy ty dst src -> StmtRefCopy ty <$> child dst <*> child src
    AST.Loop var ex incr lb -> StmtLoop var <$> child ex <*> mapIncr incr <*> child lb
    AST.Forever lb -> StmtForever <$> child lb
    s -> pure $ StmtSimple s
    where
    mapInit AST.InitZero = pure InitZero
    mapInit (AST.InitExpr ty ex) = InitExpr ty <$> child ex
    mapInit (AST.InitStruct fields) = InitStruct <$> traverse (\ (nm, i) -> (,) nm <$> mapInit i) fields
    mapInit (AST.InitArray elements) = InitArray <$> traverse mapInit elements
    mapIncr (AST.IncrTo ex) = IncrTo <$> child ex
    mapIncr (AST.DecrTo ex) = DecrTo <$> child ex

instance (MuRef a, DeRef [a] ~ DeRef a) => MuRef [a] where
  type DeRef [a] = CSE
  mapDeRef child xs = CSEBlock <$> Block <$> traverse child xs

-- | Convert a flattened statement or block back to a real block.
toBlock :: (k -> AST.Type -> BlockM AST.Expr) -> (k -> BlockM ()) -> BlockF k -> BlockM ()
toBlock expr block b = case b of
  StmtSimple s -> stmt $ return s
  StmtIfTE ex tb fb -> stmt $ AST.IfTE <$> expr ex AST.TyBool <*> genBlock (block tb) <*> genBlock (block fb)
  -- XXX: The AST does not preserve whether the RHS of a deref was for a
  -- const ref, but it's safe to assume it's const.
  StmtDeref ty var ex -> stmt $ AST.Deref ty var <$> expr ex (AST.TyConstRef ty)
  -- XXX: The LHS of a store must not have been const.
  StmtStore ty lhs rhs -> stmt $ AST.Store ty <$> expr lhs (AST.TyRef ty) <*> expr rhs ty
  StmtAssign ty var ex -> stmt $ AST.Assign ty var <$> expr ex ty
  StmtCall ty mv nm args -> stmt $ AST.Call ty mv nm <$> mapM (\ (AST.Typed argTy argEx) -> AST.Typed argTy <$> expr argEx argTy) args
  StmtLocal ty var initex -> stmt $ AST.Local ty var <$> toInit initex
  -- XXX: See deref and store comments above.
  StmtRefCopy ty dst src -> stmt $ AST.RefCopy ty <$> expr dst (AST.TyRef ty) <*> expr src (AST.TyConstRef ty)
  StmtLoop var ex incr lb -> stmt $ AST.Loop var <$> expr ex ixRep <*> toIncr incr <*> genBlock (block lb)
  StmtForever lb -> stmt $ AST.Forever <$> genBlock (block lb)
  Block stmts -> mapM_ block stmts
  where
  stmt stmtM = fmap D.singleton stmtM >>= put
  toInit InitZero = pure AST.InitZero
  toInit (InitExpr ty ex) = AST.InitExpr ty <$> expr ex ty
  toInit (InitStruct fields) = AST.InitStruct <$> traverse (\ (nm, i) -> (,) nm <$> toInit i) fields
  toInit (InitArray elements) = AST.InitArray <$> traverse toInit elements
  toIncr (IncrTo ex) = AST.IncrTo <$> expr ex ixRep
  toIncr (DecrTo ex) = AST.DecrTo <$> expr ex ixRep

-- | When a statement contains a block, we need to propagate the
-- available expressions into that block. However, on exit from that
-- block, the expressions it made newly-available go out of scope, so we
-- remove them from the available set for subsequent statements.
genBlock :: BlockM () -> BlockM AST.Block
genBlock gen = do
  oldBindings <- get
  ((), stmts) <- collect gen
  sets_ $ \ newBindings -> newBindings { availableBindings = availableBindings oldBindings }
  return $ D.toList stmts

-- | Data to accumulate as we analyze each expression and each
-- block/statement.
type Facts = (IntMap (AST.Type -> BlockM AST.Expr), IntMap (BlockM ()))

-- | We can only generate code from a DAG, so this function calls
-- `error` if the reified graph has cycles. Because we walk the AST in
-- topo-sorted order, if we haven't already computed the desired fact,
-- then we're trying to follow a back-edge in the graph, and that means
-- the graph has cycles.
getFact :: IntMap v -> Unique -> v
getFact m k = case IntMap.lookup k m of
  Nothing -> error "IvoryCSE: cycle detected in expression graph"
  Just v -> v

-- | Walk a reified AST in topo-sorted order, accumulating analysis
-- results.
--
-- `usedOnce` must be the final value of `unusedBindings` after analysis
-- is complete.
updateFacts :: IntSet -> (Unique, CSE Unique) -> Facts -> Facts
updateFacts _ (ident, CSEBlock block) (exprFacts, blockFacts) = (exprFacts, IntMap.insert ident (toBlock (getFact exprFacts) (getFact blockFacts) block) blockFacts)
updateFacts usedOnce (ident, CSEExpr expr) (exprFacts, blockFacts) = (IntMap.insert ident fact exprFacts, blockFacts)
  where
  nameOf var = AST.VarName $ "cse" ++ show var
  fact = case expr of
    ExpSimpleF e -> const $ return e
    ex -> \ ty -> do
      bindings <- get
      case Map.lookup (ident, ty) $ availableBindings bindings of
        Just var -> do
          set $ bindings { unusedBindings = IntSet.delete var $ unusedBindings bindings }
          return $ AST.ExpVar $ nameOf var
        Nothing -> do
          ex' <- fmap toExpr $ mapM (uncurry $ getFact exprFacts) $ labelTypes ty ex
          var <- sets $ \ (Bindings { availableBindings = avail, unusedBindings = unused, totalBindings = maxId}) ->
            (maxId, Bindings
              { availableBindings = Map.insert (ident, ty) maxId avail
              , unusedBindings = IntSet.insert maxId unused
              , totalBindings = maxId + 1
              })
          -- Defer a final decision on whether to inline this expression
          -- or allocate a variable for it until we've finished running
          -- the State monad and can extract the unusedBindings set from
          -- there. After that the Writer monad can make decisions based
          -- on usedOnce without throwing a <<loop>> exception.
          lift $ if var `IntSet.member` usedOnce
            then return ex'
            else do
              put $ D.singleton $ AST.Assign ty (nameOf var) ex'
              return $ AST.ExpVar $ nameOf var

-- | Values that we may generate by simplification rules on the reified
-- representation of the graph.
data Constant
  = ConstFalse
  | ConstTrue
  | ConstZero
  | ConstTwo
  deriving (Bounded, Enum)

-- | AST implementation for each constant value.
constExpr :: Constant -> CSE Unique
constExpr ConstFalse = CSEExpr $ ExpSimpleF $ AST.ExpLit $ AST.LitBool False
constExpr ConstTrue = CSEExpr $ ExpSimpleF $ AST.ExpLit $ AST.LitBool True
constExpr ConstZero = CSEExpr $ ExpSimpleF $ AST.ExpLit $ AST.LitInteger 0
constExpr ConstTwo = CSEExpr $ ExpSimpleF $ AST.ExpLit $ AST.LitInteger 2

-- | Generate a unique integer for each constant which doesn't collide
-- with any IDs that reifyGraph may generate.
constUnique :: Constant -> Unique
constUnique c = negate $ 1 + fromEnum c

-- | Wrapper around Facts to track unshared duplicates.
type Dupes = (Map (CSE Unique) Unique, IntMap Unique, Facts)

-- | Wrapper around updateFacts to remove unshared duplicates. Also,
-- checking for equality of statements or expressions is constant-time
-- in this representation, so apply any simplifications that rely on
-- equality of subtrees here.
dedup :: IntSet -> (Unique, CSE Unique) -> Dupes -> Dupes
dedup usedOnce (ident, expr) (seen, remap, facts) = case expr' of
  -- If this operator yields a constant on equal operands, we can
  -- rewrite it to that constant.
  CSEExpr (ExpOpF (AST.ExpEq ty) [a, b]) | not (isFloat ty) && a == b -> remapTo $ constUnique ConstTrue
  CSEExpr (ExpOpF (AST.ExpNeq ty) [a, b]) | not (isFloat ty) && a == b -> remapTo $ constUnique ConstFalse
  CSEExpr (ExpOpF (AST.ExpGt isEq ty) [a, b]) | not (isFloat ty) && a == b -> remapTo $ if isEq then constUnique ConstTrue else constUnique ConstFalse
  CSEExpr (ExpOpF (AST.ExpLt isEq ty) [a, b]) | not (isFloat ty) && a == b -> remapTo $ if isEq then constUnique ConstTrue else constUnique ConstFalse
  CSEExpr (ExpOpF AST.ExpBitXor [a, b]) | a == b -> remapTo $ constUnique ConstZero
  -- NOTE: This transformation is not safe for ExpSub on floating-point
  -- values, which could be NaN.

  -- If this operator is idempotent and its operands are equal, we can
  -- replace it with either operand without changing its meaning.
  CSEExpr (ExpOpF AST.ExpAnd [a, b]) | a == b -> remapTo a
  CSEExpr (ExpOpF AST.ExpOr [a, b]) | a == b -> remapTo a
  CSEExpr (ExpOpF AST.ExpBitAnd [a, b]) | a == b -> remapTo a
  CSEExpr (ExpOpF AST.ExpBitOr [a, b]) | a == b -> remapTo a

  -- If both branches of a conditional expression or statement have the
  -- same effect, then we don't need to evaluate the condition; we can
  -- just replace it with either branch. This is not safe in C because
  -- the condition might have side effects, but Ivory expressions never
  -- have side effects.
  CSEExpr (ExpOpF AST.ExpCond [_, t, f]) | t == f -> remapTo t
  -- NOTE: This results in inserting a Block directly into another
  -- Block, which can't happen any other way.
  CSEBlock (StmtIfTE _ t f) | t == f -> remapTo t

  -- Single-statement blocks generate the same code as the statement.
  CSEBlock (Block [s]) -> remapTo s

  -- No equal subtrees, so run with it.
  _ -> case Map.lookup expr' seen of
    Just ident' -> remapTo ident'
    Nothing -> (Map.insert expr' ident seen, remap, updateFacts usedOnce (ident, expr') facts)
  where
  remapTo ident' = (seen, IntMap.insert ident ident' remap, facts)
  expr' = case fmap (\ k -> IntMap.findWithDefault k k remap) expr of
    -- Perhaps this operator can be replaced by a simpler one when its
    -- operands are equal.
    CSEExpr (ExpOpF AST.ExpAdd [a, b]) | a == b -> CSEExpr $ ExpOpF AST.ExpMul $ sort [constUnique ConstTwo, a]

    -- If this operator is commutative, we can put its arguments in any
    -- order we want. If we choose the same order every time, more
    -- semantically equivalent subexpressions will be factored out.
    CSEExpr (ExpOpF op args) | isCommutative op -> CSEExpr $ ExpOpF op $ sort args
    asis -> asis

  isFloat AST.TyFloat = True
  isFloat AST.TyDouble = True
  isFloat _ = False

  isCommutative (AST.ExpEq _) = True
  isCommutative (AST.ExpNeq _) = True
  isCommutative AST.ExpMul = True
  isCommutative AST.ExpAdd = True
  isCommutative AST.ExpBitAnd = True
  isCommutative AST.ExpBitOr = True
  isCommutative AST.ExpBitXor = True
  isCommutative _ = False

-- | Given a reified AST, reconstruct an Ivory AST with all sharing made
-- explicit.
reconstruct :: Graph CSE -> AST.Block
reconstruct (Graph subexprs root) = D.toList rootBlock
  where
  -- NOTE: `dedup` needs to merge the constants in first, which means
  -- that as long as this is a `foldr`, they need to be appended after
  -- `subexprs`. Don't try to optimize this by re-ordering the list.
  (_, remap, (_, blockFacts)) = foldr (dedup usedOnce) mempty $ subexprs ++ [ (constUnique c, constExpr c) | c <- [minBound..maxBound] ]
  Just rootGen = IntMap.lookup (IntMap.findWithDefault root root remap) blockFacts
  (((), Bindings { unusedBindings = usedOnce }), rootBlock) = runM rootGen $ Bindings Map.empty IntSet.empty 0
