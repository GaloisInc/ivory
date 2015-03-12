
module Ivory.Compile.C.CmdlineFrontend
  ( compile
  , compileWith
  , runCompiler
  , runCompilerWith
  , Opts(..), parseOpts, printUsage
  , initialOpts
  ) where

import qualified Paths_ivory_backend_c      as P
import qualified Ivory.Compile.C            as C

import           Ivory.Compile.C.CmdlineFrontend.Options

import           Ivory.Language
import           Ivory.Artifact
import           Ivory.Language.Syntax.AST  as I
import qualified Ivory.Opts.BitShift        as O
import qualified Ivory.Opts.ConstFold       as O
import qualified Ivory.Opts.CSE             as O
import qualified Ivory.Opts.Overflow        as O
import qualified Ivory.Opts.DivZero         as O
import qualified Ivory.Opts.Index           as O
import qualified Ivory.Opts.FP              as O
import qualified Ivory.Opts.CFG             as G
import qualified Ivory.Opts.SanityCheck     as S
import qualified Ivory.Opts.TypeCheck       as T


import Data.Maybe (mapMaybe, catMaybes)
import Control.Monad (when)
import Data.List (foldl')
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (addExtension,(</>))

-- Code Generation Front End ---------------------------------------------------

compile :: [Module] -> [Artifact] -> IO ()
compile = compileWith Nothing

compileWith :: Maybe G.SizeMap -> [Module] -> [Artifact] -> IO ()
compileWith sm ms as = do
  args <- getArgs
  opts <- parseOpts args
  runCompilerWith sm ms as opts

runCompiler :: [Module] -> [Artifact] -> Opts -> IO ()
runCompiler ms as os = runCompilerWith Nothing ms as os

-- | Main compile function plus domain-specific type-checking that can't be
-- embedded in the Haskell type-system.  Type-checker also emits warnings.
runCompilerWith :: Maybe G.SizeMap -> [Module] -> [Artifact] -> Opts -> IO ()
runCompilerWith sm modules artifacts opts = do
  let (bs, msgs) = concatRes (map tcMod modules)
  putStrLn msgs
  when (scErrors opts) $ do
    let (bs', msgs') = concatRes (map scMod modules)
    when bs' $ do
      putStrLn msgs'
      error "Sanity-check failed!"
  when (tcErrors opts && bs) (error "There were type-checking errors.")
  rc (maybe G.defaultSizeMap id sm) modules artifacts opts
  where
  concatRes :: [(Bool, String)] -> (Bool, String)
  concatRes r = let (bs, strs) = unzip r in
            (or bs, concat strs)

  scMod :: I.Module -> (Bool, String)
  scMod m = (S.existErrors res, S.render msg)
    where
    res = S.sanityCheck modules m
    msg = S.showErrors (I.modName m) res

  tcMod :: I.Module -> (Bool, String)
  tcMod m = concatRes reses

    where
    showWarnings = tcWarnings opts

    allProcs vs = I.public vs ++ I.private vs

    reses :: [(Bool, String)]
    reses = map tcProc (allProcs (I.modProcs m))

    tcProc :: I.Proc -> (Bool, String)
    tcProc p = ( T.existErrors tc
               , errs ++ if showWarnings then warnings else []
               )
      where
      tc       = T.typeCheck p
      res f    = unlines $ f (I.procSym p) tc
      warnings = res T.showWarnings
      errs     = res T.showErrors

rc :: G.SizeMap -> [Module] -> [Artifact] -> Opts -> IO ()
rc sm modules artifacts opts
  | outProcSyms opts       = C.outputProcSyms modules
  | Nothing <- outDir opts = stdoutmodules
  | otherwise              = outputmodules

  where

  stdoutmodules = do
    mapM_ (putStrLn . C.showModule) cmodules
    cfgoutput

  outputmodules = do
    -- Irrrefutable pattern checked above
    let Just dir = outDir opts
    createDirectoryIfMissing True dir
    mapM_ (output (hdrDir dir) ".h" renderHeader) cmodules
    mapM_ (output dir          ".c" renderSource) cmodules
    runArtifactCompiler artifacts (artifactsDir dir)
    cfgoutput

  hdrDir dir =
    case outHdrDir opts of
      Nothing -> dir
      Just d  -> d

  artifactsDir dir =
    case outArtDir opts of
      Nothing -> dir
      Just d  -> d

  cfgoutput = when (cfg opts) $ do
    cfs <- mapM (\p -> G.callGraphDot p (cfgDotDir opts) optModules) cfgps
    let maxstacks = map ms (zip cfgps cfs)
    mapM_ maxStackMsg (zip cfgps maxstacks)


  optModules = map (C.runOpt passes) modules

  cfgps = cfgProc opts

  ms (p, cf) = G.maxStack p cf sm
  maxStackMsg :: (String, G.WithTop Integer) -> IO ()
  maxStackMsg (p,res) =
    putStrLn $ "Maximum stack usage from function " ++ p ++ ": " ++ show res

  cmodules   = map C.compileModule optModules

  cfPass = mkPass constFold O.constFold

  -- Put new assertion passes here and add them to passes below.
  ofPass = mkPass overflow O.overflowFold
  dzPass = mkPass divZero O.divZeroFold
  fpPass = mkPass fpCheck O.fpFold
  ixPass = mkPass ixCheck O.ixFold
  bsPass = mkPass bitShiftCheck O.bitShiftFold

  locPass = mkPass (not . srcLocs) dropSrcLocs

  mkPass passOpt pass = if passOpt opts then pass else id

  -- CSE first, because it uses observable sharing for efficiency, which will
  -- be lost if any other re-writes happen before it.
  --
  -- Next, prune any source locations we don't need.
  --
  -- Finally, constant folding before and after all assertion passes.
  --
  -- XXX This should be made more efficient at some point, since each pass
  --traverses the AST.  It's not obvious how to do that cleanly, though.
  passes e = foldl' (flip ($)) e
    [ O.cseFold
    , locPass
    , cfPass
    , ofPass, dzPass, fpPass, ixPass, bsPass
    , cfPass
    ]

  output :: FilePath -> FilePath -> (C.CompileUnits -> String)
         -> C.CompileUnits
         -> IO ()
  output dir ext render m = outputHelper fout (render m)
    where fout = addExtension (dir </> (C.unitName m)) ext

  renderHeader cu = C.renderHdr (C.headers cu) (C.unitName cu)
  renderSource cu = C.renderSrc (C.sources cu)

  outputHelper :: FilePath -> String -> IO ()
  outputHelper fname contents = case verbose opts of
    False -> out
    True -> do
      putStr ("Writing to file " ++ fname ++ "...")
      out
      putStrLn " Done"
    where
    out = writeFile fname contents

--------------------------------------------------------------------------------

runArtifactCompiler :: [Artifact] -> FilePath -> IO ()
runArtifactCompiler as dir = do
  mes <- mapM (putArtifact dir) artifacts
  case catMaybes mes of
    [] -> return ()
    errs -> error (unlines errs)
  where
  artifacts = as ++ runtimeArtifacts

runtimeArtifacts :: [Artifact]
runtimeArtifacts = map a [ "ivory.h", "ivory_asserts.h", "ivory_templates.h" ]
  where a f = artifactCabalFile P.getDataDir ("runtime/" ++ f)

--------------------------------------------------------------------------------

dropSrcLocs :: I.Proc -> I.Proc
dropSrcLocs p = p { I.procBody = dropSrcLocsBlock (I.procBody p) }
  where
  dropSrcLocsBlock = mapMaybe go
  go stmt = case stmt of
    I.IfTE b t f              -> Just $ I.IfTE b (dropSrcLocsBlock t)
                                                 (dropSrcLocsBlock f)
    I.Loop v e i b            -> Just $ I.Loop v e i (dropSrcLocsBlock b)
    I.Forever b               -> Just $ I.Forever (dropSrcLocsBlock b)
    I.Comment (I.SourcePos _) -> Nothing
    _                         -> Just stmt
