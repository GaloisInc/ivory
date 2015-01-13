{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Language.Coroutine (
  Coroutine(..), coroutine
) where

import Control.Applicative
import Control.Monad.Fix
import qualified Data.DList as D
import qualified Data.Map as Map
import Data.Monoid
import Ivory.Language.Area
import Ivory.Language.Array
import Ivory.Language.Module
import Ivory.Language.Monad
import Ivory.Language.Proc
import Ivory.Language.Proxy
import Ivory.Language.Ref
import qualified Ivory.Language.Syntax as AST
import Ivory.Language.Type
import qualified MonadLib

data Coroutine a = Coroutine
  { coroutineName :: String
  , coroutineInit :: Def ('[] :-> ())
  , coroutineRun :: forall s. Def ('[ConstRef s a] :-> ())
  , coroutineDef :: ModuleDef
  }

-- TODO: allow fromYield to have parameters and assumptions; forward those into coroutineInit.
coroutine :: forall a. IvoryArea a => ((forall s eff. Ivory eff (Ref s a)) -> Def ('[] :-> ())) -> Coroutine a
coroutine fromYield = Coroutine { .. }
  where
  DefProc (AST.Proc { AST.procSym = name, AST.procBody = rawCode }) = fromYield $ call (proc yieldName $ body $ return ())

  params = CoroutineParams
    { getCont = AST.ExpLabel strTy $ AST.ExpSym $ AST.areaSym cont
    , getBreakLabel = error "Ivory.Language.Coroutine: no break label set, but breakOut called"
    , getLabelProc = \ label -> name ++ "_bb_" ++ show label
    }

  initialState = CoroutineState
    { rewrites = Map.empty
    , labels = []
    , derefs = 0
    }

  ((((), initCode), (localVars, resumes)), finalState) = MonadLib.runM (extractLocals (resumeAt 0) rawCode) params initialState

  strName = name ++ "_continuation"
  strDef = AST.Struct strName $ AST.Typed stateType stateName : D.toList localVars
  strTy = AST.TyStruct strName
  cont = AST.Area strName False strTy AST.InitZero

  coroutineName = name

  coroutineInit = DefProc $ simpleProc (name ++ "_init") $ D.toList initCode

  runCode = (AST.Deref stateType (AST.VarName stateName) $ getCont params $ stateName) : D.toList resumes
  coroutineRun = DefProc $ (simpleProc (name ++ "_run") runCode)
    { AST.procArgs = [AST.Typed (AST.TyConstRef $ ivoryArea (Proxy :: Proxy a)) $ AST.VarName argName] }

  coroutineDef = do
    incl coroutineInit
    incl coroutineRun
    MonadLib.put $ mempty
      { AST.modStructs = mempty { AST.private = [strDef] }
      , AST.modAreas = mempty { AST.private = [cont] }
      }
    private $ mapM_ (\ (label, block) -> incl $ DefProc $ simpleProc (getLabelProc params label) block) $ zip [1..] $ reverse $ labels finalState

simpleProc :: AST.Sym -> AST.Block -> AST.Proc
simpleProc name block = AST.Proc
  { AST.procSym = name
  , AST.procRetTy = AST.TyVoid
  , AST.procArgs = []
  , AST.procBody = block
  , AST.procRequires = []
  , AST.procEnsures = []
  }

yieldName :: String
yieldName = "+yield" -- not a valid C identifier, so can't collide with a real proc

stateName :: String
stateName = "state"

stateType :: AST.Type
stateType = AST.TyWord AST.Word32

argName :: String
argName = "next_value"

type Goto = Int

data CoroutineParams = CoroutineParams
  { getCont :: String -> AST.Expr
  , getBreakLabel :: Goto
  , getLabelProc :: Goto -> AST.Sym
  }

data CoroutineState = CoroutineState
  { rewrites :: Map.Map AST.Var (CoroutineMonad AST.Expr)
  , labels :: [AST.Block]
  , derefs :: !Integer
  }

type CoroutineVars = (D.DList (AST.Typed String), D.DList AST.Stmt)

type CoroutineMonad = MonadLib.WriterT (D.DList AST.Stmt) (MonadLib.ReaderT CoroutineParams (MonadLib.WriterT CoroutineVars (MonadLib.StateT CoroutineState MonadLib.Id)))

extractLocals :: CoroutineMonad () -> AST.Block -> CoroutineMonad ()
extractLocals next [] = next
extractLocals next (AST.IfTE cond tb fb : rest) = do
  after <- makeLabel $ extractLocals next rest
  stmt =<< AST.IfTE <$> updateExpr cond <*> getBlock tb (goto after) <*> getBlock fb (goto after)
extractLocals _next (AST.Return {} : _) = error "Ivory.Language.Coroutine: can't return a value from the coroutine body"
-- XXX: this discards any code after a return. is that OK?
extractLocals _next (AST.ReturnVoid : _) = resumeAt 0
extractLocals next (AST.Call ty mvar name args : rest)
  | name == AST.NameSym yieldName = do
    let (Just var, []) = (mvar, args) -- XXX: yield takes no arguments and always returns something
    addYield ty var rest next
  | otherwise = do
    stmt =<< AST.Call ty mvar name <$> mapM updateTypedExpr args
    case mvar of
      Nothing -> return ()
      Just var -> do
        cont <- addLocal ty var
        stmt $ AST.Store ty cont $ AST.ExpVar var
    extractLocals next rest
extractLocals next (AST.Local ty var initex : rest) = do
  cont <- addLocal ty var
  let AST.VarName varStr = var
  let ref = AST.VarName $ varStr ++ "_ref"
  initex' <- updateInit initex
  stmts
    [ AST.Local ty var initex'
    , AST.AllocRef ty ref $ AST.NameVar var
    , AST.RefCopy ty cont $ AST.ExpVar ref
    ]
  extractLocals next rest
extractLocals next (AST.AllocRef _ty refvar name : rest) = do
  let AST.NameVar var = name -- XXX: AFAICT, AllocRef can't have a NameSym argument.
  refvar `rewriteTo` contRef var
  extractLocals next rest
extractLocals next (AST.Loop var initEx incr b : rest) = do
  let ty = ivoryType (Proxy :: Proxy IxRep)
  cont <- addLocal ty var
  stmt =<< AST.Store ty cont <$> updateExpr initEx
  after <- makeLabel $ extractLocals next rest
  loop <- mfix $ \ loop -> makeLabel $ do
    let (condOp, incOp, limitEx) = case incr of
          AST.IncrTo ex -> (AST.ExpGt, AST.ExpAdd, ex)
          AST.DecrTo ex -> (AST.ExpLt, AST.ExpSub, ex)
    cond <- updateExpr $ AST.ExpOp (condOp False ty) [AST.ExpVar var, limitEx]
    stmt =<< AST.IfTE cond <$> getBlock [] (goto after) <*> pure []
    b' <- setBreakLabel after $ getBlock b $ do
      stmt $ AST.Store ty cont $ AST.ExpOp incOp [AST.ExpVar var, AST.ExpLit (AST.LitInteger 1)]
      goto loop
    stmts b'
  goto loop
extractLocals next (AST.Forever b : rest) = do
  after <- makeLabel $ extractLocals next rest
  loop <- mfix $ \ loop -> makeLabel $ do
    b' <- setBreakLabel after $ getBlock b (goto loop)
    stmts b'
  goto loop
-- XXX: this discards any code after a break. is that OK?
extractLocals _next (AST.Break : _) = goto =<< MonadLib.asks getBreakLabel
extractLocals next (s : rest) = do
  stmt =<< case s of
    AST.Assert cond -> AST.Assert <$> updateExpr cond
    AST.CompilerAssert cond -> AST.CompilerAssert <$> updateExpr cond
    AST.Deref ty var ex -> AST.RefCopy ty <$> addLocal ty var <*> updateExpr ex
    AST.Store ty lhs rhs -> AST.Store ty <$> updateExpr lhs <*> updateExpr rhs
    AST.Assign ty var ex -> AST.Store ty <$> addLocal ty var <*> updateExpr ex
    _ -> return s
  extractLocals next rest

getBlock :: AST.Block -> CoroutineMonad () -> CoroutineMonad AST.Block
getBlock b next = do
  ((), b') <- MonadLib.collect $ extractLocals next b
  return $ D.toList b'

makeLabel :: CoroutineMonad () -> CoroutineMonad Goto
makeLabel m = do
  block <- getBlock [] m
  MonadLib.sets $ \ state ->
    let state' = state { labels = block : labels state }
    in (length (labels state'), state')

goto :: Goto -> CoroutineMonad ()
goto label = do
  labelProc <- MonadLib.asks getLabelProc
  stmts
    [ AST.Call AST.TyVoid Nothing (AST.NameSym $ labelProc label) []
    , AST.ReturnVoid
    ]

resumeAt :: Goto -> CoroutineMonad ()
resumeAt label = do
  cont <- contRef $ AST.VarName stateName
  stmts
    [ AST.Store stateType cont $ AST.ExpLit $ AST.LitInteger $ fromIntegral label
    , AST.ReturnVoid
    ]

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

addYield :: AST.Type -> AST.Var -> AST.Block -> CoroutineMonad () -> CoroutineMonad ()
addYield ty var rest next = do
  cont <- addLocal ty var
  after <- makeLabel $ extractLocals next rest
  let cond = AST.ExpOp (AST.ExpEq stateType) [AST.ExpVar (AST.VarName stateName), AST.ExpLit $ AST.LitInteger $ fromIntegral after]
  resume <- getBlock [] $ do
    let AST.TyRef derefTy = ty
    stmt $ AST.RefCopy derefTy cont $ AST.ExpVar (AST.VarName argName)
    goto after
  MonadLib.lift $ MonadLib.put (mempty, D.singleton $ AST.IfTE cond resume [])
  resumeAt after

setBreakLabel :: Goto -> CoroutineMonad a -> CoroutineMonad a
setBreakLabel label m = do
  params <- MonadLib.ask
  MonadLib.local (params { getBreakLabel = label }) m

stmt :: AST.Stmt -> CoroutineMonad ()
stmt = MonadLib.put . D.singleton

stmts :: AST.Block -> CoroutineMonad ()
stmts = MonadLib.put . D.fromList

rewriteTo :: AST.Var -> CoroutineMonad AST.Expr -> CoroutineMonad ()
rewriteTo var repl = MonadLib.sets_ $ \ state -> state { rewrites = Map.insert var repl $ rewrites state }

updateExpr :: AST.Expr -> CoroutineMonad AST.Expr
updateExpr ex@(AST.ExpVar var) = Map.findWithDefault (return ex) var =<< fmap rewrites MonadLib.get
updateExpr (AST.ExpLabel ty ex label) = AST.ExpLabel ty <$> updateExpr ex <*> pure label
updateExpr (AST.ExpIndex ty1 ex1 ty2 ex2) = AST.ExpIndex <$> pure ty1 <*> updateExpr ex1 <*> pure ty2 <*> updateExpr ex2
updateExpr (AST.ExpToIx ex bound) = AST.ExpToIx <$> updateExpr ex <*> pure bound
updateExpr (AST.ExpSafeCast ty ex) = AST.ExpSafeCast ty <$> updateExpr ex
updateExpr (AST.ExpOp op args) = AST.ExpOp op <$> mapM updateExpr args
updateExpr ex = return ex

updateInit :: AST.Init -> CoroutineMonad AST.Init
updateInit AST.InitZero = return AST.InitZero
updateInit (AST.InitExpr ty ex) = AST.InitExpr ty <$> updateExpr ex
updateInit (AST.InitStruct fields) = AST.InitStruct <$> mapM (\ (name, ex) -> (,) name <$> updateInit ex) fields
updateInit (AST.InitArray elems) = AST.InitArray <$> mapM updateInit elems

updateTypedExpr :: AST.Typed AST.Expr -> CoroutineMonad (AST.Typed AST.Expr)
updateTypedExpr (AST.Typed ty ex) = AST.Typed ty <$> updateExpr ex
