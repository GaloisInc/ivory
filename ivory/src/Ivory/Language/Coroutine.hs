{- |
Module: Coroutine.hs
Description: Resumable coroutines implementation
Copyright: (c) 2014 Galois, Inc.

This is an implementation of coroutines in Ivory.  These coroutines:

  * may be suspended and resumed,
  * are parametrized over any memory-area type ('Area'),
  * cannot return values, but can receive values when they are resumed (though
this is an incidental detail of the current implementation),
  * for now, can have only one instance executing at once, given a particular
coroutine name and Ivory module.

-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.Coroutine (
  -- * Usage Notes
  -- $usageNotes
  
  -- * Implementation Notes
  -- $implNotes
  
  Coroutine(..), CoroutineBody(..), coroutine,
  ) where

import Prelude ()
import Prelude.Compat

import Control.Monad (unless,when)
import Control.Monad.Fix (mfix)
import qualified Data.DList as D
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Ivory.Language.Area
import Ivory.Language.Array
import Ivory.Language.Effects
import Ivory.Language.IBool
import Ivory.Language.Module
import Ivory.Language.Monad
import Ivory.Language.Proc
import Ivory.Language.Proxy
import Ivory.Language.Ref
import qualified Ivory.Language.Syntax as AST
import Ivory.Language.Type
import qualified MonadLib

-- Optimizations TODO:
-- TODO: if every block yields, don't generate a loop
-- TODO: re-use continuation variables that have gone out of scope
-- TODO: full liveness analysis to maximize re-use
-- TODO: only extract a variable to the continuation if it is live across a suspend

{- $usageNotes

The coroutine itself is presented as a function which has one argument,
@yield@.  @yield@ suspends the coroutine's execution at the point of usage.

'coroutineRun' (given an argument of 'false') then resumes the suspended
coroutine at that point, passing in a value in the process.

-}

{- $implNotes

This implementation handles a coroutine in the form of a single contiguous
block of Ivory code.  It turns this block into a single large C function,
breaking it up into an initialization portion and a series of branches inside
an infinite loop.  Each branch represents a resume and suspend point within
that block.

It was mentioned that @yield@ /suspends/ a coroutine for later resumption.
The state of the computation to be resumed later - that is, its
/continuation/ - is stored in a C struct, not visible to the outer Ivory code,
but denoted internally with a suffix of @_continuation@.  This C struct
contains a state variable and every local variable that is created.

At each @yield@, after collecting the current computation state into the
continuation, the C function breaks out of its loop and returns.

The @yield@ action is implemented as a sort of pseudo-function call which is
given a purposely-invalid name (see 'yieldName'). This Ivory call does not
become an actual function call in generated code, but rather, the code is
suspended right there.  (See the 'extractLocals' and 'addYield' functions.)

The pseudo-function call returns a 'ConstRef' to the coroutine's type.  Of
course, no actual function call exists, but the action itself still returns
something - hence, Ivory code is able to refer to the return value of a
@yield@.  Dereferences to it via 'deref' are turned to references to the
continuation struct.

As this @yield@ action is an Ivory effect, it can be passed at the Haskell
level, but it cannot be passed as a C value. (If one could hypothetically run
'procPtr' on it, then one could pass it around as a value, but any attempt to
call it outside of the same coroutine would result in either an invalid
function call, or a suspend & return of a different coroutine.)

-}

-- | Concrete representation of a coroutine; use 'coroutine' to produce one.
data Coroutine a = Coroutine
  { -- | Name of the coroutine (must be a valid C identifier)
    coroutineName :: String
    -- | Ivory effect for initializing a coroutine (if first argument is
    -- 'true'), or resuming it (if first argument is 'false').  The second
    -- argument is passed to the coroutine when resuming, and ignored when
    -- initializing.
  , coroutineRun :: forall eff s s'. GetAlloc eff ~ 'Scope s' =>
                    IBool -> ConstRef s a -> Ivory eff ()
    -- | The components a 'Module' will need for this coroutine
  , coroutineDef :: ModuleDef
  }

-- | The definition of a coroutine body, in the form of a function taking one
-- argument: an Ivory effect that is the @yield@ action which suspends the
-- coroutine, and sets the point at which 'coroutineRun' resumes it, passing a
-- value.
newtype CoroutineBody a =
  CoroutineBody (forall s1 s2 .
                 (forall b .
                  Ivory ('Effects ('Returns ()) b ('Scope s2)) (Ref s1 a)) ->
                         Ivory (ProcEffects s2 ()) ())

-- | Smart constructor for a 'Coroutine'
coroutine :: forall a. IvoryArea a =>
             String -- ^ Coroutine name (must be a valid C identifier)
             -> CoroutineBody a -- ^ Coroutine definition
             -> Coroutine a
coroutine name (CoroutineBody fromYield) = Coroutine { .. }
  where
  ((), CodeBlock { blockStmts = rawCode }) =
    runIvory $ fromYield $ call (proc yieldName $ body $ return ())
  -- The above call is a fake function call which code elsewhere extracts from
  -- the AST and replaces.

  params = CoroutineParams
           { getCont = AST.ExpLabel strTy $ AST.ExpAddrOfGlobal $
                       AST.areaSym contArea
           , getBreakLabel =
             error "Ivory.Language.Coroutine: no break label set, but breakOut called"
           }

  initialState = CoroutineState
                 { rewrites = Map.empty
                 , labels = []
                 , derefs = 0
                 }

  -- Even the initial block needs a label, in case there's a yield or
  -- return before any control flow statements. Otherwise, the resulting
  -- resumeAt call will emit a 'break;' statement outside of the
  -- forever-loop that the state machine runs in, which is invalid C.
  initCode = makeLabel' =<< getBlock rawCode (resumeAt 0)
  (((initLabel, _), (localVars, resumes)), finalState) =
     MonadLib.runM initCode params initialState
  initBB = BasicBlock [] $ BranchTo False initLabel

  -- | Name of the continuation struct type:
  strName = name ++ "_continuation"

  -- | Definitions of the continuation struct type:
  strDef = AST.Struct strName $
           -- State variable:
           AST.Typed stateType stateName :
           D.toList localVars
  strTy = AST.TyStruct strName

  -- | Area for the continuation struct instance:
  contArea = AST.Area (name ++ "_cont") False strTy AST.InitZero

  coroutineName = name

  litLabel = AST.ExpLit . AST.LitInteger . fromIntegral

  genBB (BasicBlock pre term) = pre ++ case term of
    BranchTo suspend label -> (AST.Store stateType (getCont params stateName) $ litLabel label) : if suspend then [AST.Break] else []
    CondBranchTo cond tb fb -> [AST.IfTE cond (genBB tb) (genBB fb)]

  coroutineRun :: IBool -> ConstRef s a -> Ivory eff ()
  coroutineRun doInit arg = do
    ifte_ doInit (emits mempty { blockStmts = genBB initBB }) (return ())
    emit $ AST.Forever $ (AST.Deref stateType (AST.VarName stateName) $ getCont params stateName) : do
      (label, block) <- keepUsedBlocks initLabel $ zip [0..] $ map joinTerminators $ (BasicBlock [] $ BranchTo True 0) : reverse (labels finalState)
      let cond = AST.ExpOp (AST.ExpEq stateType)
                 [AST.ExpVar (AST.VarName stateName), litLabel label]
          fn :: AST.Expr -> [AST.Stmt]
          fn = Map.findWithDefault (const []) label resumes
          b' = fn (unwrapExpr arg) ++ genBB block
      return $ AST.IfTE cond b' []

  coroutineDef = do
    visibility <- MonadLib.ask
    MonadLib.put $ mempty
      { AST.modStructs = visAcc visibility strDef
      , AST.modAreas = visAcc visibility contArea
      }

-- | This is used as the name of a pseudo-function call which marks the
-- 'yield' in a coroutine.  It is purposely not a valid C identifier so that
-- it can't collide with a real procedure.
yieldName :: String
yieldName = "+yield"

-- | Name of the variable used for the coroutine's current state.
stateName :: String
stateName = "state"

-- | The type used for the continuation's state.
stateType :: AST.Type
stateType = AST.TyWord AST.Word32

data BasicBlock = BasicBlock AST.Block Terminator
type Goto = Int
data Terminator
  = BranchTo Bool Goto
  | CondBranchTo AST.Expr BasicBlock BasicBlock

joinTerminators :: BasicBlock -> BasicBlock
joinTerminators (BasicBlock b (CondBranchTo cond t f)) =
  case (joinTerminators t, joinTerminators f) of
  (BasicBlock bt (BranchTo yt lt), BasicBlock bf (BranchTo yf lf))
    | yt == yf && lt == lf -> BasicBlock (b ++ [AST.IfTE cond bt bf]) (BranchTo yt lt)
  (t', f') -> BasicBlock b (CondBranchTo cond t' f')
joinTerminators bb = bb

doInline :: Goto -> [(Goto, BasicBlock)] -> [(Goto, BasicBlock)]
doInline inlineLabel blocks = do
  let Just (BasicBlock newStmts tgt) = lookup inlineLabel blocks
  let inlineBlock (BasicBlock b (BranchTo False dst))
        | dst == inlineLabel = BasicBlock (b ++ newStmts) tgt
      inlineBlock (BasicBlock b (CondBranchTo cond tb fb))
        = joinTerminators $ BasicBlock b $ CondBranchTo cond (inlineBlock tb) (inlineBlock fb)
      inlineBlock bb = bb
  (label, bb) <- blocks
  return $ if label == inlineLabel then (label, bb) else (label, inlineBlock bb)

keepUsedBlocks :: Goto -> [(Goto, BasicBlock)] -> [(Goto, BasicBlock)]
keepUsedBlocks root blocks = sweep $ snd $ MonadLib.runM (mark root >> ref root) IntMap.empty
  where
  mark :: Goto -> MonadLib.StateT (IntMap.IntMap Int) MonadLib.Id ()
  mark label = do
    seen <- MonadLib.get
    ref label
    unless (label `IntMap.member` seen) $ do
      let Just b = lookup label blocks
      markBlock b
  ref label = MonadLib.sets_ $ IntMap.insertWith (+) label 1
  markBlock (BasicBlock _ (BranchTo suspend label)) = do
    mark label
    -- add an extra reference for yield targets
    when suspend $ ref label
  markBlock (BasicBlock _ (CondBranchTo _ tb fb)) = markBlock tb >> markBlock fb
  sweep used = filter (\ (label, _) -> IntMap.findWithDefault 0 label used > 1) $
    foldr doInline blocks [ label | (label, 1) <- IntMap.assocs used ]

data CoroutineParams = CoroutineParams
  { getCont :: String -> AST.Expr
  , getBreakLabel :: Terminator
  }

data CoroutineState = CoroutineState
  { rewrites :: Map.Map AST.Var (CoroutineMonad AST.Expr)
  , labels :: [BasicBlock]
  , derefs :: !Integer
  }

type CoroutineResume = Map.Map Goto (AST.Expr -> AST.Block)

type CoroutineVars = (D.DList (AST.Typed String), CoroutineResume)

type CoroutineMonad = MonadLib.WriterT
                      (D.DList AST.Stmt)
                      (MonadLib.ReaderT CoroutineParams
                       (MonadLib.WriterT CoroutineVars
                        (MonadLib.StateT CoroutineState MonadLib.Id)))

-- | Walk an 'AST.Stmt', update its variables as necessary to refer to the
-- continuation struct, and any time a new local is introduced, add it to the
-- continuation struct.
extractLocals :: AST.Stmt
                 -> CoroutineMonad Terminator
                 -> CoroutineMonad Terminator
extractLocals (AST.IfTE cond tb fb) rest = do
  after <- makeLabel rest
  CondBranchTo <$> runUpdateExpr (updateExpr cond)
    <*> getBlock tb (return after)
    <*> getBlock fb (return after)
extractLocals (AST.Assert cond) rest =
  (AST.Assert <$> runUpdateExpr (updateExpr cond)) >>= stmt >> rest
extractLocals (AST.CompilerAssert cond) rest =
  (AST.CompilerAssert <$> runUpdateExpr (updateExpr cond)) >>= stmt >> rest
extractLocals (AST.Assume cond) rest =
  (AST.Assume <$> runUpdateExpr (updateExpr cond)) >>= stmt >> rest
extractLocals (AST.Return {}) _ =
  error "Ivory.Language.Coroutine: can't return a value from the coroutine body"
-- XXX: this discards any code after a return. is that OK?
extractLocals (AST.ReturnVoid) _ = resumeAt 0
extractLocals (AST.Deref ty var ex) rest =
  (AST.RefCopy ty <$> addLocal ty var <*> runUpdateExpr (updateExpr ex)) >>=
  stmt >> rest
  -- Note here that an 'AST.Deref' also emits a 'RefCopy' and another local.
extractLocals (AST.Store ty lhs rhs) rest =
  (runUpdateExpr $ AST.Store ty <$> updateExpr lhs <*> updateExpr rhs) >>=
  stmt >> rest
extractLocals (AST.Assign ty var ex) rest =
  (AST.Store ty <$> addLocal ty var <*> runUpdateExpr (updateExpr ex)) >>=
  stmt >> rest
extractLocals (AST.Call ty mvar name args) rest
  -- 'yieldName' is the pseudo-function call which we handle specially at this
  -- point using 'addYield':
  | name == AST.NameSym yieldName = do
      -- XXX: yield takes no arguments and always returns something
      let (Just var, []) = (mvar, args)
      addYield ty var rest
  | otherwise = do
      -- All other function calls pass through normally, but have their
      -- arguments run through 'updateTypedExpr' and have their results saved
      -- into the continuation:
      stmt =<< AST.Call ty mvar name <$>
        runUpdateExpr (mapM updateTypedExpr args)
      case mvar of
       Nothing -> return ()
       Just var -> do
         cont <- addLocal ty var
         stmt $ AST.Store ty cont $ AST.ExpVar var
      rest
extractLocals (AST.Local ty var initex) rest = do
  cont <- addLocal ty var
  let AST.VarName varStr = var
  let ref = AST.VarName $ varStr ++ "_ref"
  initex' <- runUpdateExpr $ updateInit initex
  stmts
    [ AST.Local ty var initex'
    , AST.AllocRef ty ref $ AST.NameVar var
    , AST.RefCopy ty cont $ AST.ExpVar ref
    ]
  rest
extractLocals (AST.RefCopy ty lhs rhs) rest =
  (runUpdateExpr $ AST.RefCopy ty <$> updateExpr lhs <*> updateExpr rhs) >>=
  stmt >> rest
extractLocals (AST.AllocRef _ty refvar name) rest = do
  let AST.NameVar var = name -- XXX: AFAICT, AllocRef can't have a NameSym argument.
  refvar `rewriteTo` contRef var
  rest
extractLocals (AST.Loop var initEx incr b) rest = do
  let ty = ivoryType (Proxy :: Proxy IxRep)
  cont <- addLocal ty var
  stmt =<< AST.Store ty cont <$> runUpdateExpr (updateExpr initEx)
  after <- makeLabel rest
  mfix $ \ loop -> makeLabel $ do
    let (condOp, incOp, limitEx) = case incr of
          AST.IncrTo ex -> (AST.ExpGt, AST.ExpAdd, ex)
          AST.DecrTo ex -> (AST.ExpLt, AST.ExpSub, ex)
    cond <- runUpdateExpr $ updateExpr $
            AST.ExpOp (condOp False ty) [AST.ExpVar var, limitEx]
    CondBranchTo cond (BasicBlock [] after) <$> do
      setBreakLabel after $ getBlock b $ do
        stmt =<< AST.Store ty cont <$>
          runUpdateExpr (updateExpr $ AST.ExpOp incOp
                         [AST.ExpVar var, AST.ExpLit (AST.LitInteger 1)])
        return loop
extractLocals (AST.Forever b) rest = do
  after <- makeLabel rest
  mfix $ \ loop -> makeLabel $ setBreakLabel after $
                   foldr extractLocals (return loop) b
-- XXX: this discards any code after a break. is that OK?
extractLocals (AST.Break) _ = MonadLib.asks getBreakLabel
extractLocals s@(AST.Comment{}) rest = stmt s >> rest

getBlock :: AST.Block -> CoroutineMonad Terminator -> CoroutineMonad BasicBlock
getBlock b next = do
  (term, b') <- MonadLib.collect $ foldr extractLocals next b
  return $ BasicBlock (D.toList b') term

makeLabel :: CoroutineMonad Terminator -> CoroutineMonad Terminator
makeLabel m = do
  block <- getBlock [] m
  case block of
    BasicBlock b term | all isComment b -> do
      stmts b
      return term
    _ -> goto =<< makeLabel' block
  where
  isComment (AST.Comment{}) = True
  isComment _ = False

makeLabel' :: BasicBlock -> CoroutineMonad Goto
makeLabel' block = MonadLib.sets $ \ state ->
  let state' = state { labels = block : labels state }
  in (length (labels state'), state')

goto :: Goto -> CoroutineMonad Terminator
goto = return . BranchTo False

resumeAt :: Goto -> CoroutineMonad Terminator
resumeAt = return . BranchTo True

contRef :: AST.Var -> CoroutineMonad AST.Expr
contRef var = do
  let AST.VarName varStr = var
  MonadLib.asks getCont <*> pure varStr

addLocal :: AST.Type -> AST.Var -> CoroutineMonad AST.Expr
addLocal ty var = do
  let AST.VarName varStr = var
  MonadLib.lift $ MonadLib.put (D.singleton $ AST.Typed ty varStr, mempty)
  cont <- contRef var
  var `rewriteTo` do
    idx <- MonadLib.sets $ \state ->
                            (derefs state, state { derefs = derefs state + 1 })
    let var' = AST.VarName $ "cont" ++ show idx
    stmt $ AST.Deref ty var' cont
    return $ AST.ExpVar var'
  return cont

-- | Generate the code to turn a @yield@ pseudo-call to a suspend and resume.
addYield :: AST.Type -> AST.Var -> CoroutineMonad Terminator ->
            CoroutineMonad Terminator
addYield ty var rest = do
  let AST.TyRef derefTy = ty
      AST.VarName varStr = var
  MonadLib.lift $ MonadLib.put
    (D.singleton $ AST.Typed derefTy varStr, mempty)
  cont <- contRef var
  var `rewriteTo` return cont
  after <- makeLabel' =<< getBlock [] rest
  let resume arg = [AST.RefCopy derefTy cont arg]
  MonadLib.lift $ MonadLib.put (mempty, Map.singleton after resume)
  resumeAt after

setBreakLabel :: Terminator -> CoroutineMonad a -> CoroutineMonad a
setBreakLabel label m = do
  params <- MonadLib.ask
  MonadLib.local (params { getBreakLabel = label }) m

-- | Insert a statement into a 'CoroutineMonad'.
stmt :: AST.Stmt -> CoroutineMonad ()
stmt s = MonadLib.put $ D.singleton s

-- | Insert a block of statements into a 'CoroutineMonad'.
stmts :: AST.Block -> CoroutineMonad ()
stmts = MonadLib.put . D.fromList

-- | Add a rewrite to some 'AST.Var', such that references to it are replaced
-- with execution of the given monadic code, and the value with the supplied
-- expression. ('addLocal' uses this to ensure that local variables that are
-- promoted to the global continuation are 'deref'ed before each use. This
-- 'deref' occurs each time because any intervening @yield@ puts a previous
-- 'deref' out of scope.)
rewriteTo :: AST.Var -- ^ Variable to replace
             -> CoroutineMonad AST.Expr -- ^ New value & statements
             -> CoroutineMonad ()
rewriteTo var repl =
  MonadLib.sets_ $ \state -> state
                             { rewrites = Map.insert var repl $ rewrites state }

-- | State monad transformer containing 'CoroutineMonad', plus a memoization
-- table recording which rewrites have already been evaluated for the current
-- statement.  /(Since an Ivory statement cannot have side effects - such as
-- storing to the continuation or suspending with 'yield' - we may safely
-- dereference the continuation variable just once per statement, regardless of
-- how many subexpressions contain that variable.)/
type UpdateExpr a = MonadLib.StateT (Map.Map AST.Var AST.Expr) CoroutineMonad a

-- | Starting from an empty memoization table, run the memoized monadic code
-- from a sequence of 'updateExpr'.  /(As each statement requires an empty
-- memoization table, this should be run only on single statements or on
-- smaller divisions of one, though it may generate redundant 'deref's in the
-- latter case.)/
runUpdateExpr :: UpdateExpr a -> CoroutineMonad a
runUpdateExpr = fmap fst . MonadLib.runStateT Map.empty

-- | Update an 'AST.Expr' with variable rewrites, applying and updating the
-- memoization table of rewrites in the process.
updateExpr :: AST.Expr -> UpdateExpr AST.Expr
updateExpr ex@(AST.ExpVar var) = do
  updated <- MonadLib.get
  -- Here's the actual memoizing. If the memo table doesn't already have the
  -- given variable in it, we try to find it in the rewrites table, and save
  -- the resulting Expr in the memo table in case we encounter another use of
  -- the same variable. If it isn't in the rewrites table, it wasn't declared
  -- in this local scope, so we'll just hope it was global or something and use
  -- it as-is. And if it was already in the memo table, we don't evaluate the
  -- monadic action in the rewrites table a second time.
  case Map.lookup var updated of
    Just ex' -> return ex'
    Nothing -> do
      ex' <- MonadLib.lift $ do
        Map.findWithDefault (return ex) var =<< fmap rewrites MonadLib.get
      MonadLib.sets_ $ Map.insert var ex'
      return ex'
updateExpr (AST.ExpLabel ty ex label) =
  AST.ExpLabel ty <$> updateExpr ex <*> pure label
updateExpr (AST.ExpIndex ty1 ex1 ty2 ex2) =
  AST.ExpIndex <$> pure ty1 <*> updateExpr ex1 <*> pure ty2 <*> updateExpr ex2
updateExpr (AST.ExpToIx ex bound) =
  AST.ExpToIx <$> updateExpr ex <*> pure bound
updateExpr (AST.ExpSafeCast ty ex) =
  AST.ExpSafeCast ty <$> updateExpr ex
updateExpr (AST.ExpOp op args) =
  AST.ExpOp op <$> mapM updateExpr args
updateExpr ex = return ex

-- | Basically 'updateExpr', but on an initializer.
updateInit :: AST.Init -> UpdateExpr AST.Init
updateInit AST.InitZero = return AST.InitZero
updateInit (AST.InitExpr ty ex) =
  -- 'AST.InitExpr' contains an expression that 'updateExpr' needs to handle:
  AST.InitExpr ty <$> updateExpr ex
updateInit (AST.InitStruct fields) =
  -- Every field of a @struct@ in an 'AST.InitStruct' contains an expression
  -- that must go through 'updateExpr':
  AST.InitStruct <$> mapM (\ (name, ex) -> (,) name <$> updateInit ex) fields
updateInit (AST.InitArray elems) =
  -- An 'AST.InitArray' is a list of 'AST.Init' which we must recurse over:
  AST.InitArray <$> mapM updateInit elems

-- | Basically 'updateExpr', but on a typed expression.
updateTypedExpr :: AST.Typed AST.Expr -> UpdateExpr (AST.Typed AST.Expr)
updateTypedExpr (AST.Typed ty ex) = AST.Typed ty <$> updateExpr ex
