
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
import qualified Ivory.Opts.Overflow        as O
import qualified Ivory.Opts.DivZero         as O
import qualified Ivory.Opts.Index           as O
import qualified Ivory.Opts.FP              as O
import qualified Ivory.Opts.CFG             as G
import qualified Ivory.Opts.TypeCheck       as T


import Data.Maybe (mapMaybe)
import Data.Monoid
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


runArtifactCompiler :: [Artifact] -> Opts -> IO ()
runArtifactCompiler as opts = do
  errs <- putArtifacts (srcDir opts) (as ++ runtimeArtifacts)
  case errs of
    Nothing -> return ()
    Just e -> error e

runtimeArtifacts :: [Artifact]
runtimeArtifacts = map a [ "ivory.h", "ivory_asserts.h", "ivory_templates.h" ]
  where a f = artifactCabalFile P.getDataDir ("runtime/" ++ f)

runCompiler :: [Module] -> [Artifact] -> Opts -> IO ()
runCompiler ms as os = runCompilerWith Nothing ms as os

-- | Main compile function plus domain-specific type-checking that can't be
-- embedded in the Haskell type-system.  Type-checker also emits warnings.
runCompilerWith :: Maybe G.SizeMap -> [Module] -> [Artifact] -> Opts -> IO ()
runCompilerWith sm modules artifacts opts = do
  let (bs, msgs) = concatRes (map tcMod modules)
  putStrLn msgs
  when (tcErrors opts && bs) (error "There were type-checking errors.")
  rc (maybe G.defaultSizeMap id sm) modules artifacts opts
  where
  concatRes :: [(Bool, String)] -> (Bool, String)
  concatRes r = let (bs, strs) = unzip r in
            (or bs, concat strs)

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
  | stdOut opts            = stdoutmodules
  | otherwise              = outputmodules

  where

  stdoutmodules = do
    mapM_ showM_ cmodules
    cfgoutput

  outputmodules = do
    createDirectoryIfMissing True (includeDir opts)
    createDirectoryIfMissing True (srcDir opts)
    outputHeaders (includeDir opts) cmodules
    outputSources (srcDir opts) cmodules
    runArtifactCompiler artifacts opts
    cfgoutput

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

  showM_ mods = do
    mapM_ (mapM_ putStrLn) (C.showModule mods)

  cfPass = mkPass constFold O.constFold

  -- Put new assertion passes here and add them to passes below.
  ofPass = mkPass overflow O.overflowFold
  dzPass = mkPass divZero O.divZeroFold
  fpPass = mkPass fpCheck O.fpFold
  ixPass = mkPass ixCheck O.ixFold
  bsPass = mkPass bitShiftCheck O.bitShiftFold

  locPass = mkPass (not . srcLocs) dropSrcLocs

  mkPass passOpt pass = if passOpt opts then pass else id

  -- Constant folding before and after all other passes.
  --
  -- XXX This should be made more efficient at some point, since each pass
  --traverses the AST.  It's not obvious how to do that cleanly, though.
  passes e = foldl' (flip ($)) e
    [ locPass
    , cfPass
    , ofPass, dzPass, fpPass, ixPass, bsPass
    , cfPass
    ]

  -- Output headers in a directory
  outputHeaders :: FilePath -> [C.CompileUnits] -> IO ()
  outputHeaders fp cus = mapM_ (process outputHeader fp) cus
  -- Output sources in a directory
  outputSources :: FilePath -> [C.CompileUnits] -> IO ()
  outputSources fp cus = mapM_ (process outputSrc fp) cus

  process outputter dir m = outputter (dir </> (C.unitName m)) m

  -- Transform a compiled unit into a header, and write to a .h file
  outputHeader :: FilePath -> C.CompileUnits -> IO ()
  outputHeader basename cm = do
    let headerfname = addExtension basename ".h"
        header = C.renderHdr (C.headers cm) (C.unitName cm)
    outputHelper headerfname header

  -- Transform a compiled unit into a c src, and write to a .c file
  outputSrc :: FilePath -> C.CompileUnits -> IO ()
  outputSrc basename cm = do
    let srcfname = addExtension basename ".c"
        src = C.renderSrc (C.sources cm)
    outputHelper srcfname src

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

-- XXX eliminate rtIncludeDir

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
