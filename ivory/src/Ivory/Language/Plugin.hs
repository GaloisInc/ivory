{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Ivory.Language.Plugin (plugin) where

import           DynamicLoading
import           GhcPlugins

import GHC.Plugins.SrcSpan

#if __GLASGOW_HASKELL__ < 708
# error Ivory.Language.Plugin requires at least ghc-7.8
#endif

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals

  hsc_env <- getHscEnv

  Just withLocName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iVORY_MONAD wITH_LOC
  withLocVar <- lookupId withLocName

  Just mkLocName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iVORY_MONAD mK_LOC
  mkLocVar <- lookupId mkLocName

  Just ivoryName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iVORY_MONAD iVORY
  ivoryCon <- lookupTyCon ivoryName

  let isInteresting expr = return (isIvoryStmt ivoryCon expr)
  let annotate loc expr = mkWithLocExpr mkLocVar withLocVar loc expr
  let locpass = mkPass isInteresting annotate killForeignStubs

  return $ (CoreDoPluginPass "Add Locations" locpass) : todos
  where
  killForeignStubs = "kill-foreign-stubs" `elem` opts


isIvoryStmt :: TyCon -> CoreExpr -> Bool
isIvoryStmt ivory expr
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == ivory
  | otherwise
  = False


mkWithLocExpr :: Var -> Var -> SrcSpan -> CoreExpr -> CoreM CoreExpr
mkWithLocExpr mkLocVar withLocVar (RealSrcSpan ss) expr = do
  loc <- mkLocExpr mkLocVar ss
  return $ mkCoreApps (Var withLocVar) [ Type effTy, Type exprResTy
                                       , loc, expr
                                       ]
  where
  (_, [effTy, exprResTy]) = splitAppTys $ exprType expr

mkWithLocExpr _ _ _ expr = return expr


mkLocExpr :: Var -> RealSrcSpan -> CoreM CoreExpr
mkLocExpr mkLocVar ss = do
  df   <- getDynFlags
  file <- mkStringExprFS $ srcSpanFile ss
  return $ mkCoreApps (Var mkLocVar) [ file
                                     , mkIntExprInt df (srcSpanStartLine ss)
                                     , mkIntExprInt df (srcSpanStartCol ss)
                                     , mkIntExprInt df (srcSpanEndLine ss)
                                     , mkIntExprInt df (srcSpanEndCol ss)
                                     ]

iVORY_MONAD :: ModuleName
iVORY_MONAD = mkModuleName "Ivory.Language.Monad"

wITH_LOC, mK_LOC, iVORY :: RdrName
wITH_LOC    = mkVarUnqual $ fsLit "withLocation"
mK_LOC      = mkVarUnqual $ fsLit "mkLocation"
iVORY       = mkRdrQual iVORY_MONAD $ mkTcOcc "Ivory"
