{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Ivory.Language.Plugin (plugin) where

import qualified Data.IntMap           as IntMap
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

  let modName = moduleNameString $ moduleName mg_module
  Mix file _ _ _ entries <- liftIO $ readMix [hpcDir df] $ Left modName
  fileExpr <- mkStringExpr file
  let locs = IntMap.fromList $ zip [0..] $ map fst entries

  let binds = map (addLocationsBind (mkWithLocExpr withLocVar)
                                    (mkLocExpr mkLocVar df fileExpr . (locs IntMap.!))
                                    (isIvoryStmt ivoryCon))
                   mg_binds

  let stubs = if killForeignStubs
                 then NoStubs
                 else mg_foreign

  return (guts { mg_binds = binds, mg_foreign = stubs })

addLocationsBind :: (CoreExpr -> CoreExpr -> CoreExpr) 
                 -> (Int -> CoreExpr) 
                 -> (CoreExpr -> Bool) 
                 -> CoreBind -> CoreBind
addLocationsBind withLoc mkLoc isIvory bndr = case bndr of
  NonRec b expr -> NonRec b (addLocationsExpr withLoc mkLoc isIvory expr)
  Rec binds     -> Rec [ (b, addLocationsExpr withLoc mkLoc isIvory expr) 
                       | (b, expr) <- binds 
                       ]

addLocationsExpr :: (CoreExpr -> CoreExpr -> CoreExpr) 
                 -> (Int -> CoreExpr) 
                 -> (CoreExpr -> Bool) 
                 -> CoreExpr -> CoreExpr
addLocationsExpr withLoc mkLoc isIvory = go 0
  where
  go _  (Tick (HpcTick _ i) expr) 
    = go i expr
  go i (Tick _ expr) 
    = go i expr
  go i e@(App expr arg) 
    | isIvory e
    = withLoc (mkLoc i) (App (go i expr) (go i arg))
    | otherwise
    = App (go i expr) (go i arg)
  go i (Lam x expr)
    = Lam x (go i expr)
  go i (Let bndr expr)
    = Let (addLocationsBind withLoc mkLoc isIvory bndr) (go i expr)
  go i (Case expr x t alts)
    = Case (go i expr) x t (map (addLocationsAlt i) alts)
  go i (Cast expr c)
    = Cast (go i expr) c
  go _  expr
    = expr

  addLocationsAlt i (c, xs, expr)
    = (c, xs, go i expr)


isIvoryStmt :: TyCon -> CoreExpr -> Bool
isIvoryStmt ivory expr
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == ivory
  | otherwise
  = False


mkWithLocExpr :: Var -> CoreExpr -> CoreExpr -> CoreExpr
mkWithLocExpr withLocVar loc expr 
  = mkCoreApps (Var withLocVar) [ Type effTy, Type exprResTy
                                , loc, expr
                                ]
  where
  (_, [effTy, exprResTy]) = splitAppTys $ exprType expr


mkLocExpr :: Var -> DynFlags -> CoreExpr -> HpcPos -> CoreExpr
mkLocExpr mkLocVar df file (fromHpcPos -> (l1,c1,l2,c2))
  = mkCoreApps (Var mkLocVar) [ file
                              , mkInt df l1, mkInt df c1
                              , mkInt df l2, mkInt df c2
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
