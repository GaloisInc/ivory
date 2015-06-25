{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.Coroutine (
  Coroutine(..), CoroutineBody(..), coroutine
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import qualified Data.DList as D
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Monoid
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

data Coroutine a = Coroutine
  { coroutineName :: String
  , coroutineRun :: forall eff s s'. GetAlloc eff ~ Scope s' => IBool -> ConstRef s a -> Ivory eff ()
  , coroutineDef :: ModuleDef
  }

newtype CoroutineBody a =
  CoroutineBody (forall s1 s2.
                 (forall b.
                  Ivory ('Effects (Returns ()) b (Scope s2)) (Ref s1 a)) ->
                         Ivory (ProcEffects s2 ()) ())

coroutine :: forall a. IvoryArea a => String -> CoroutineBody a -> Coroutine a
coroutine name (CoroutineBody fromYield) = Coroutine { .. }
  where
  ((), CodeBlock { blockStmts = rawCode }) = runIvory $ fromYield $ call (proc yieldName $ body $ return ())

  params = CoroutineParams
    { getCont = AST.ExpLabel strTy $ AST.ExpAddrOfGlobal $ AST.areaSym cont
    , getBreakLabel = error "Ivory.Language.Coroutine: no break label set, but breakOut called"
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
  (((initLabel, _), (localVars, resumes)), finalState) = MonadLib.runM initCode params initialState
  initBB = BasicBlock [] $ BranchTo False initLabel

  strName = name ++ "_continuation"
  strDef = AST.Struct strName $ AST.Typed stateType stateName : D.toList localVars
  strTy = AST.TyStruct strName
  cont = AST.Area (name ++ "_cont") False strTy AST.InitZero

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
      let cond = AST.ExpOp (AST.ExpEq stateType) [AST.ExpVar (AST.VarName stateName), litLabel label]
      let b' = Map.findWithDefault (const []) label resumes (unwrapExpr arg) ++ genBB block
      return $ AST.IfTE cond b' []

  coroutineDef = do
    visibility <- MonadLib.ask
    MonadLib.put $ mempty
      { AST.modStructs = visAcc visibility strDef
      , AST.modAreas = visAcc visibility cont
      }

yieldName :: String
yieldName = "+yield" -- not a valid C identifier, so can't collide with a real proc

stateName :: String
stateName = "state"

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

type CoroutineMonad = MonadLib.WriterT (D.DList AST.Stmt) (MonadLib.ReaderT CoroutineParams (MonadLib.WriterT CoroutineVars (MonadLib.StateT CoroutineState MonadLib.Id)))

extractLocals :: AST.Stmt -> CoroutineMonad Terminator -> CoroutineMonad Terminator
extractLocals (AST.IfTE cond tb fb) rest = do
  after <- makeLabel rest
  CondBranchTo <$> runUpdateExpr (updateExpr cond) <*> getBlock tb (return after) <*> getBlock fb (return after)
extractLocals (AST.Assert cond) rest = (AST.Assert <$> runUpdateExpr (updateExpr cond)) >>= stmt >> rest
extractLocals (AST.CompilerAssert cond) rest = (AST.CompilerAssert <$> runUpdateExpr (updateExpr cond)) >>= stmt >> rest
extractLocals (AST.Assume cond) rest = (AST.Assume <$> runUpdateExpr (updateExpr cond)) >>= stmt >> rest
extractLocals (AST.Return {}) _ = error "Ivory.Language.Coroutine: can't return a value from the coroutine body"
-- XXX: this discards any code after a return. is that OK?
extractLocals (AST.ReturnVoid) _ = resumeAt 0
extractLocals (AST.Deref ty var ex) rest = (AST.RefCopy ty <$> addLocal ty var <*> runUpdateExpr (updateExpr ex)) >>= stmt >> rest
extractLocals (AST.Store ty lhs rhs) rest = (runUpdateExpr $ AST.Store ty <$> updateExpr lhs <*> updateExpr rhs) >>= stmt >> rest
extractLocals (AST.Assign ty var ex) rest = (AST.Store ty <$> addLocal ty var <*> runUpdateExpr (updateExpr ex)) >>= stmt >> rest
extractLocals (AST.Call ty mvar name args) rest
  | name == AST.NameSym yieldName = do
    let (Just var, []) = (mvar, args) -- XXX: yield takes no arguments and always returns something
    addYield ty var rest
  | otherwise = do
    stmt =<< AST.Call ty mvar name <$> runUpdateExpr (mapM updateTypedExpr args)
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
extractLocals (AST.RefCopy ty lhs rhs) rest = (runUpdateExpr $ AST.RefCopy ty <$> updateExpr lhs <*> updateExpr rhs) >>= stmt >> rest
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
    cond <- runUpdateExpr $ updateExpr $ AST.ExpOp (condOp False ty) [AST.ExpVar var, limitEx]
    CondBranchTo cond (BasicBlock [] after) <$> do
      setBreakLabel after $ getBlock b $ do
        stmt =<< AST.Store ty cont <$> runUpdateExpr (updateExpr $ AST.ExpOp incOp [AST.ExpVar var, AST.ExpLit (AST.LitInteger 1)])
        return loop
extractLocals (AST.Forever b) rest = do
  after <- makeLabel rest
  mfix $ \ loop -> makeLabel $ setBreakLabel after $ foldr extractLocals (return loop) b
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
    idx <- MonadLib.sets $ \ state -> (derefs state, state { derefs = derefs state + 1 })
    let var' = AST.VarName $ "cont" ++ show idx
    stmt $ AST.Deref ty var' cont
    return $ AST.ExpVar var'
  return cont

addYield :: AST.Type -> AST.Var -> CoroutineMonad Terminator -> CoroutineMonad Terminator
addYield ty var rest = do
  let AST.TyRef derefTy = ty
  let AST.VarName varStr = var
  MonadLib.lift $ MonadLib.put (D.singleton $ AST.Typed derefTy varStr, mempty)
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

stmt :: AST.Stmt -> CoroutineMonad ()
stmt = MonadLib.put . D.singleton

stmts :: AST.Block -> CoroutineMonad ()
stmts = MonadLib.put . D.fromList

rewriteTo :: AST.Var -> CoroutineMonad AST.Expr -> CoroutineMonad ()
rewriteTo var repl = MonadLib.sets_ $ \ state -> state { rewrites = Map.insert var repl $ rewrites state }

type UpdateExpr a = MonadLib.StateT (Map.Map AST.Var AST.Expr) CoroutineMonad a

runUpdateExpr :: UpdateExpr a -> CoroutineMonad a
runUpdateExpr = fmap fst . MonadLib.runStateT Map.empty

updateExpr :: AST.Expr -> UpdateExpr AST.Expr
updateExpr ex@(AST.ExpVar var) = do
  updated <- MonadLib.get
  case Map.lookup var updated of
    Just ex' -> return ex'
    Nothing -> do
      ex' <- MonadLib.lift $ do
        Map.findWithDefault (return ex) var =<< fmap rewrites MonadLib.get
      MonadLib.sets_ $ Map.insert var ex'
      return ex'
updateExpr (AST.ExpLabel ty ex label) = AST.ExpLabel ty <$> updateExpr ex <*> pure label
updateExpr (AST.ExpIndex ty1 ex1 ty2 ex2) = AST.ExpIndex <$> pure ty1 <*> updateExpr ex1 <*> pure ty2 <*> updateExpr ex2
updateExpr (AST.ExpToIx ex bound) = AST.ExpToIx <$> updateExpr ex <*> pure bound
updateExpr (AST.ExpSafeCast ty ex) = AST.ExpSafeCast ty <$> updateExpr ex
updateExpr (AST.ExpOp op args) = AST.ExpOp op <$> mapM updateExpr args
updateExpr ex = return ex

updateInit :: AST.Init -> UpdateExpr AST.Init
updateInit AST.InitZero = return AST.InitZero
updateInit (AST.InitExpr ty ex) = AST.InitExpr ty <$> updateExpr ex
updateInit (AST.InitStruct fields) = AST.InitStruct <$> mapM (\ (name, ex) -> (,) name <$> updateInit ex) fields
updateInit (AST.InitArray elems) = AST.InitArray <$> mapM updateInit elems

updateTypedExpr :: AST.Typed AST.Expr -> UpdateExpr (AST.Typed AST.Expr)
updateTypedExpr (AST.Typed ty ex) = AST.Typed ty <$> updateExpr ex
