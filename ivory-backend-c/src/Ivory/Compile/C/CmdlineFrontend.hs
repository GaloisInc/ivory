module Ivory.Compile.C.CmdlineFrontend
  ( compile
  , compileWith
  , runCompiler
  , runCompilerWith
  , Opts(..), parseOpts, printUsage
  , initialOpts
  , compileUnits
  , outputCompiler
  ) where

import           Data.List                               (intercalate, nub,
                                                          (\\))
import qualified Ivory.Compile.C                         as C
import qualified Paths_ivory_backend_c                   as P

import           Ivory.Compile.C.CmdlineFrontend.Options

import           Ivory.Artifact
import           Ivory.Language
import           Ivory.Language.Syntax.AST               as I
import qualified Ivory.Opts.BitShift                     as O
import qualified Ivory.Opts.ConstFold                    as O
import qualified Ivory.Opts.CSE                          as O
import qualified Ivory.Opts.DivZero                      as O
import qualified Ivory.Opts.FP                           as O
import qualified Ivory.Opts.Index                        as O
import qualified Ivory.Opts.Overflow                     as O
import qualified Ivory.Opts.SanityCheck                  as S
import qualified Ivory.Opts.TypeCheck                    as T


import           Control.Monad                           (when)
import           Data.List                               (foldl')
import           Data.Maybe                              (catMaybes, mapMaybe)
import           System.Directory                        (createDirectoryIfMissing)
import           System.Environment                      (getArgs)
import           System.FilePath                         (addExtension, (</>))

-- Code Generation Front End ---------------------------------------------------

compile :: [Module] -> [Located Artifact] -> IO ()
compile = compileWith

compileWith :: [Module] -> [Located Artifact] -> IO ()
compileWith ms as = do
  args <- getArgs
  opts <- parseOpts args
  runCompilerWith ms as opts

runCompiler :: [Module] -> [Located Artifact] -> Opts -> IO ()
runCompiler ms as os = runCompilerWith ms as os

-- | Main compile function.
runCompilerWith :: [Module] -> [Located Artifact] -> Opts -> IO ()
runCompilerWith modules artifacts opts = do
  cmodules <- compileUnits modules opts
  if outProcSyms opts
    then C.outputProcSyms modules
    else outputCompiler cmodules artifacts opts

outputCompiler :: [C.CompileUnits] -> [Located Artifact] -> Opts -> IO ()
outputCompiler cmodules artifacts opts
  | Nothing <- outDir opts
  = stdoutmodules cmodules
  | otherwise
  = outputmodules opts cmodules artifacts

stdoutmodules :: [C.CompileUnits] -> IO ()
stdoutmodules cmodules =
  mapM_ (putStrLn . C.showModule) cmodules

outputmodules :: Opts -> [C.CompileUnits] -> [Located Artifact] -> IO ()
outputmodules opts cmodules user_artifacts = do
  -- Irrrefutable pattern checked above
  let Just srcdir = outDir opts
  let incldir = hdrDir srcdir
  let rootdir = rootDir srcdir
  createDirectoryIfMissing True rootdir
  createDirectoryIfMissing True srcdir
  createDirectoryIfMissing True incldir
  let oh h = Incl (artifactFile h (return h)) : user_artifacts
  let user_artifacts' = maybe user_artifacts oh (otherHdr opts)
  let artifacts = runtimeHeaders ++ user_artifacts'
  warnCollisions cmodules artifacts rootdir srcdir incldir
  mapM_ (output srcdir ".c" renderSource) cmodules
  mapM_ (output incldir  ".h" renderHeader) cmodules
  runArtifactCompiler artifacts rootdir srcdir incldir

  where
  hdrDir dir =
    case outHdrDir opts of
      Nothing -> dir
      Just d  -> d

  rootDir dir =
    case outArtDir opts of -- XXX TODO: fix outArtDir naming, should be outRootDir or somethign
      Nothing -> dir
      Just d  -> d

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

-- | Compile, type-check, and optimize modules, but don't generate C files.
compileUnits ::[Module] -> Opts -> IO [C.CompileUnits]
compileUnits modules opts = do

  when (tcErrors opts) $ do
    let ts = map T.typeCheck modules
    let anyTs = or (map T.existErrors ts)
    let b = tcWarnings opts
    mapM_ (T.showTyChkModule b) ts
    when anyTs (error "Type-checking failed!")

  when (scErrors opts) $ do
    let ds = S.dupDefs modules
    let anyDs = not (null ds)
    S.showDupDefs ds
    let ss = S.sanityCheck modules
    let anySs = or (map S.existErrors ss)
    mapM_ S.showSanityChkModule ss
    when (anyDs || anySs) (error "Sanity-check failed!")

  return (mkCUnits modules opts)

mkCUnits :: [Module] -> Opts -> [C.CompileUnits]
mkCUnits modules opts = cmodules
  where
  cmodules            = map (C.compileModule (otherHdr opts)) optModules
  optModules          = map (C.runOpt passes) modules

  cfPass              = mkPass constFold O.constFold
  -- Put new assertion passes here and add them to passes below.
  ofPass              = mkPass overflow O.overflowFold
  dzPass              = mkPass divZero O.divZeroFold
  fpPass              = mkPass fpCheck O.fpFold
  ixPass              = mkPass ixCheck O.ixFold
  bsPass              = mkPass bitShiftCheck O.bitShiftFold
  locPass             = mkPass (not . srcLocs) dropSrcLocs

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

--------------------------------------------------------------------------------

runArtifactCompiler :: [Located Artifact] -> FilePath -> FilePath -> FilePath
                    -> IO ()
runArtifactCompiler las root_dir src_dir incl_dir = do
  mes <- sequence
    [ case la of
       Root a -> putArtifact root_dir a
       Src a  -> putArtifact src_dir a
       Incl a -> putArtifact incl_dir a
    | la <- las ]
  case catMaybes mes of
    [] -> return ()
    errs -> error (unlines errs)

--------------------------------------------------------------------------------

runtimeHeaders :: [Located Artifact]
runtimeHeaders = map a [ "ivory.h", "ivory_asserts.h", "ivory_templates.h" ]
  where a f = Incl $ artifactCabalFile P.getDataDir ("runtime/" ++ f)

--------------------------------------------------------------------------------
warnCollisions :: [C.CompileUnits] -- All Ivory Modules
               -> [Located Artifact] -- All artifacts
               -> FilePath  -- Root path
               -> FilePath  -- Source path
               -> FilePath  -- Incl path
               -> IO ()
warnCollisions ms as rootpath spath ipath = case dupes of
  [] -> return ()
  _ -> putStrLn $ intercalate "\n\t" $
    ["**** Warning: the following files will be written multiple times during codegen! ****"]
    ++ dupes
  where
  cnames = [ spath </> (C.unitName m ++ ".c") | m <- ms ]
  hnames = [ ipath </> (C.unitName m ++ ".h") | m <- ms ]
  anames = [ case la of
              Root a -> rootpath </> artifactFileName a
              Src a  -> spath </>  artifactFileName a
              Incl a -> ipath </>  artifactFileName a
           | la <- as ]
  allnames = cnames ++ hnames ++ anames
  dupes = allnames \\ (nub allnames)


--------------------------------------------------------------------------------

dropSrcLocs :: I.Proc -> I.Proc
dropSrcLocs p = p { I.procBody = dropSrcLocsBlock (I.procBody p) }
  where
  dropSrcLocsBlock = mapMaybe go
  go stmt = case stmt of
    I.IfTE b t f              -> Just $ I.IfTE b (dropSrcLocsBlock t)
                                                 (dropSrcLocsBlock f)
    I.Loop m v e i b          -> Just $ I.Loop m v e i (dropSrcLocsBlock b)
    I.Forever b               -> Just $ I.Forever (dropSrcLocsBlock b)
    I.Comment (I.SourcePos _) -> Nothing
    _                         -> Just stmt
