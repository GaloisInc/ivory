{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Language.Plugin (plugin) where

import           Control.Exception
import           Control.Monad
import           CostCentre
import qualified Data.Array        as Array
import           Data.IntMap       (IntMap)
import qualified Data.IntMap       as IntMap
import           DynamicLoading
import           GhcPlugins
import           Trace.Hpc.Mix
import           Trace.Hpc.Util

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals
  return $ locs : todos
  where 
  killForeignStubs = "kill-foreign-stubs" `elem` opts
  locs = CoreDoPluginPass "Add Locations" $ pass killForeignStubs

pass :: Bool -> ModGuts -> CoreM ModGuts
pass killForeignStubs guts@(ModGuts {..}) = do
  hsc_env <- getHscEnv
  df  <- getDynFlags
  EPS {..} <- liftIO $ hscEPS hsc_env

  Just withLocName <- liftIO $ lookupRdrName hsc_env iVORY_MONAD wITH_LOC
  withLocVar <- lookupId withLocName

  Just mkLocName <- liftIO $ lookupRdrName hsc_env iVORY_MONAD mK_LOC
  mkLocVar <- lookupId mkLocName

  Just ivoryName <- liftIO $ lookupRdrName hsc_env iVORY_MONAD iVORY
  ivoryCon <- lookupTyCon ivoryName

  mkLoc <- liftIO $ getLocs df guts

  binds <- mapM (addLocationsBind mkLoc
                                  (mkWithLocExpr df mkLocVar withLocVar)
                                  (isIvoryStmt ivoryCon))
                mg_binds

  let stubs = if killForeignStubs
                 then NoStubs
                 else mg_foreign

  return (guts { mg_binds = binds, mg_foreign = stubs })

getLocs :: DynFlags -> ModGuts -> IO (Tickish Var -> SrcSpan)
getLocs df ModGuts {..} = do
  let modName = moduleName mg_module
  mixEntries <- getMixEntries modName (hpcDir df) 
                  `catch` \(_ :: SomeException) -> return []
  let locs = IntMap.fromList $ zip [0..] mixEntries
  let bks  = IntMap.fromList $ Array.assocs $ modBreaks_locs mg_modBreaks
  return (tickLoc locs bks)

getMixEntries :: ModuleName -> FilePath -> IO [SrcSpan]
getMixEntries nm dir = do
  Mix file _ _ _ entries <- readMix [dir] (Left $ moduleNameString nm)
  let f = fsLit file
  return [ mkSrcSpan (mkSrcLoc f l1 c1) (mkSrcLoc f l2 c2) 
         | (hpc, _) <- entries, let (l1,c1,l2,c2) = fromHpcPos hpc 
         ]

tickLoc :: IntMap SrcSpan -> IntMap SrcSpan -> Tickish Var -> SrcSpan
tickLoc _       _        (ProfNote cc _ _) = cc_loc cc
tickLoc hpcLocs _        (HpcTick _ i)     = IntMap.findWithDefault noSrcSpan i hpcLocs
tickLoc _       bkPoints (Breakpoint i _)  = IntMap.findWithDefault noSrcSpan i bkPoints


addLocationsBind :: (Tickish Var -> SrcSpan)
                 -> (SrcSpan -> CoreExpr -> CoreM CoreExpr) 
                 -> (CoreExpr -> Bool) 
                 -> CoreBind -> CoreM CoreBind
addLocationsBind getSpan withLoc isIvory bndr = case bndr of
  NonRec b expr -> NonRec b `liftM` addLocationsExpr getSpan withLoc isIvory expr
  Rec binds     -> do binds' <- forM binds $ \(b, expr) -> 
                                  (b,) `liftM` addLocationsExpr getSpan withLoc isIvory expr
                      return $ Rec binds'

addLocationsExpr :: (Tickish Var -> SrcSpan)
                 -> (SrcSpan -> CoreExpr -> CoreM CoreExpr) 
                 -> (CoreExpr -> Bool) 
                 -> CoreExpr -> CoreM CoreExpr
addLocationsExpr getSpan withLoc isIvory = go noSrcSpan
  where
  go ss (Tick t expr) 
    | isGoodSrcSpan (getSpan t)
    = go (getSpan t) expr
    | otherwise
    = go ss expr
  go ss e@(App expr arg) 
    | isIvory e
    = do e' <- liftM2 App (go ss expr) (go ss arg)
         withLoc ss e'
    | otherwise
    = liftM2 App (go ss expr) (go ss arg)
  go ss (Lam x expr)
    = liftM (Lam x) (go ss expr)
  go ss (Let bndr expr)
    = liftM2 Let (addLocationsBind getSpan withLoc isIvory bndr) (go ss expr)
  go ss (Case expr x t alts)
    = liftM2 (\e as -> Case e x t as) (go ss expr) (mapM (addLocationsAlt ss) alts)
  go ss (Cast expr c)
    = (`Cast` c) `liftM` go ss expr
  go _  expr
    = return expr

  addLocationsAlt ss (c, xs, expr)
    = (c, xs,) `liftM` go ss expr


isIvoryStmt :: TyCon -> CoreExpr -> Bool
isIvoryStmt ivory expr
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == ivory
  | otherwise
  = False


mkWithLocExpr :: DynFlags -> Var -> Var -> SrcSpan -> CoreExpr -> CoreM CoreExpr
mkWithLocExpr df mkLocVar withLocVar (RealSrcSpan ss) expr = do
  loc <- mkLocExpr mkLocVar df ss
  return $ mkCoreApps (Var withLocVar) [ Type effTy, Type exprResTy
                                       , loc, expr
                                       ]
  where
  (_, [effTy, exprResTy]) = splitAppTys $ exprType expr

mkWithLocExpr _ _ _ _ expr = return expr


mkLocExpr :: Var -> DynFlags -> RealSrcSpan -> CoreM CoreExpr
mkLocExpr mkLocVar df ss = do 
  file <- mkStringExprFS $ srcSpanFile ss
  return $ mkCoreApps (Var mkLocVar) [ file
                                     , mkInt df (srcSpanStartLine ss), mkInt df (srcSpanStartCol ss)
                                     , mkInt df (srcSpanEndLine ss), mkInt df (srcSpanEndCol ss)
                                     ]

iVORY_MONAD :: ModuleName
iVORY_MONAD = mkModuleName "Ivory.Language.Monad"

wITH_LOC, mK_LOC, iVORY :: RdrName
wITH_LOC    = mkVarUnqual $ fsLit "withLocation"
mK_LOC      = mkVarUnqual $ fsLit "mkLocation"
iVORY       = mkRdrQual iVORY_MONAD $ mkTcOcc "Ivory"


--------------------------------------------------------------------------------
-- Let's maintain a bit of backwards compatibility..
--------------------------------------------------------------------------------

lookupRdrName :: HscEnv -> ModuleName -> RdrName -> IO (Maybe Name)
mkInt :: DynFlags -> Int -> CoreExpr

#if __GLASGOW_HASKELL__ >= 708
lookupRdrName = lookupRdrNameInModuleForPlugins
mkInt = mkIntExprInt
#else
lookupRdrName = lookupRdrNameInModule
mkInt _ = mkIntExprInt
#endif
