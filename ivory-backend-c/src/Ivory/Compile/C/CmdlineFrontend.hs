
module Ivory.Compile.C.CmdlineFrontend
  ( compile
  , compileWith
  , runCompiler
  , runCompilerWith
  , Opts(..), parseOpts, printUsage
  , initialOpts
  , ModuleFiles(..)
  , standaloneDepFile
  , compileDepFile
  ) where

import qualified Paths_ivory_backend_c
import qualified Ivory.Compile.C            as C
import qualified Ivory.Compile.C.SourceDeps as C

import           Ivory.Compile.C.CmdlineFrontend.Options

import           Ivory.Language
import           Ivory.Language.Syntax.AST  as I
import qualified Ivory.Opts.ConstFold       as O
import qualified Ivory.Opts.Overflow        as O
import qualified Ivory.Opts.DivZero         as O
import qualified Ivory.Opts.Index           as O
import qualified Ivory.Opts.FP              as O
import qualified Ivory.Opts.CFG             as G
import qualified Ivory.Opts.TypeCheck       as T

import qualified Data.ByteString.Char8      as B

import Data.Monoid
import Control.Monad (when)
import Data.List (foldl')
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeDirectory,takeExtension,addExtension,(</>))
import Text.PrettyPrint.Mainland
    ((<+>),line,text,stack,punctuate,render,empty,indent,displayS)
import qualified System.FilePath.Posix as PFP


-- Code Generation Front End ---------------------------------------------------

data ModuleFiles =
  ModuleFiles
    { mf_sources :: [FilePath]
    , mf_headers :: [FilePath]
    }

compile :: [Module] -> IO ()
compile = compileWith Nothing Nothing

compileWith :: Maybe G.SizeMap -> Maybe [IO FilePath] -> [Module]
            -> IO ()
compileWith sm sp ms = do
  args <- getArgs
  opts <- parseOpts args
  mf   <- runCompilerWith sm sp ms opts
  compileDepFile opts (standaloneDepFile mf)

compileDepFile :: Opts -> [(String, [String])] -> IO ()
compileDepFile opts ds =
  when (not (null (deps opts))) $ outputDepFile opts ds

-- | Main compile function plus domain-specific type-checking that can't be
-- embedded in the Haskell type-system.  Type-checker also emits warnings.
runCompilerWith :: Maybe G.SizeMap -> Maybe [IO FilePath] -> [Module] -> Opts
                -> IO ModuleFiles
runCompilerWith sm sp modules opts = do
  let (bs, msgs) = concatRes (map tcMod modules)
  putStrLn msgs
  when (tcErrors opts && bs) (error "There were type-checking errors.")
  rc (maybe G.defaultSizeMap id sm)
     (maybe [] id sp) modules opts
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

runCompiler :: [Module] -> Opts -> IO ModuleFiles
runCompiler ms os = runCompilerWith Nothing Nothing ms os

rc :: G.SizeMap -> [IO FilePath] -> [Module] -> Opts -> IO ModuleFiles
rc sm userSearchPath modules opts
  | not (null (deps opts)) = return moduleFiles
  | outProcSyms opts       = C.outputProcSyms modules >> return moduleFiles
  | stdOut opts            = stdoutmodules            >> return moduleFiles
  | otherwise              = outputmodules            >> return moduleFiles

  where
  moduleFiles = ModuleFiles
      { mf_sources = genSs ++ cpySs
      , mf_headers = genHs ++ cpyHs
      }

  ivoryHeaders = ["ivory.h", "ivory_templates.h", "ivory_asserts.h"]

  stdoutmodules = do
    mapM_ showM_ cmodules
    cfgoutput

  outputmodules = do
    searchPath <- mkSearchPath opts userSearchPath
    createDirectoryIfMissing True (includeDir opts)
    createDirectoryIfMissing True (srcDir opts)
    outputHeaders (includeDir opts) cmodules
    outputSources (srcDir opts) cmodules
    C.outputSourceDeps (includeDir opts) (srcDir opts)
       (map ("runtime/" ++) ivoryHeaders ++ (C.collectSourceDeps modules)) searchPath
    cfgoutput

  cfgoutput = when (cfg opts) $ do
    cfs <- mapM (\p -> G.callGraphDot p (cfgDotDir opts) optModules) cfgps
    let maxstacks = map ms (zip cfgps cfs)
    mapM_ maxStackMsg (zip cfgps maxstacks)


  sdeps = C.collectSourceDeps modules
  genHs = map (modFilePath (includeDir opts) ".h") cmodules
  genSs = map (modFilePath (srcDir opts) ".c")     cmodules
  cpyHs = map (srcFilePath (includeDir opts)) $
            filter (\p -> takeExtension p == ".h") sdeps
  cpySs = map (srcFilePath (srcDir opts)) $
            filter (\p -> takeExtension p == ".c") sdeps

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

  mkPass passOpt pass = if passOpt opts then pass else id

  -- Constant folding before and after all other passes.
  --
  -- XXX This should be made more efficient at some point, since each pass
  --traverses the AST.  It's not obvious how to do that cleanly, though.
  passes e = foldl' (flip ($)) e
    [ cfPass
    , ofPass, dzPass, fpPass, ixPass
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

modFilePath :: FilePath -> String -> C.CompileUnits -> String
modFilePath basepath extension unit = basepath PFP.</> (C.unitName unit) PFP.<.> extension

srcFilePath :: FilePath -> FilePath -> String
srcFilePath basepath sdep = basepath PFP.</> sdep

standaloneDepFile :: ModuleFiles -> [(String, [String])]
standaloneDepFile mf =
  [ ("HEADERS", mf_headers mf)
  , ("SOURCES", mf_sources mf)
  ]

outputDepFile :: Opts -> [(String, [String])] -> IO ()
outputDepFile opts kvs = do
  createDirectoryIfMissing True (takeDirectory path)
  outputIfChanged docstring
  where
  path = deps opts
  prefix = depPrefix opts
  docstring = displayS (render w d) ""
  w = 10000000 -- don't ever wrap lines - invalid make syntax
  d = stack $
    [ text "# dep file autogenerated by ivory compiler"
    , empty
    ] ++ map (uncurry listof) kvs

  declaration n = text prefix <> text "_" <> text n <+> text ":= \\" <> line
  listof name values = declaration name <>
    (indent 4 $ stack $ punctuate (text " \\") (map text values)) <>
    line <> empty

  outputIfChanged :: String -> IO ()
  outputIfChanged string_contents = do
    let contents = B.pack string_contents
    exists <- doesFileExist path
    case exists of
      False -> B.writeFile path contents
      True -> do
        existing <- B.readFile path
        case existing == contents of
            True -> return ()
            False -> B.writeFile path contents


mkSearchPath :: Opts -> [IO FilePath] -> IO [FilePath]
mkSearchPath opts userSearchPaths = do
  rtPath <- getRtPath
  users <- sequence userSearchPaths
  return $ rtPath:users
  where
  getRtPath :: IO FilePath
  getRtPath  = case rtIncludeDir opts of
    Just path -> return path
    Nothing   -> Paths_ivory_backend_c.getDataDir

