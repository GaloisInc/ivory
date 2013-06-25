{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Ivory.Compile.C.CmdlineFrontend
  ( compile
  , compileWith
  , runCompiler
  , runCompilerWith
  , Opts(..), parseOpts, printUsage
  , initialOpts
  ) where

import qualified Paths_ivory_backend_c
import qualified Ivory.Compile.C as C
import qualified Ivory.Compile.C.SourceDeps as C

import Ivory.Language
import qualified Ivory.Opts.ConstFold as O
import qualified Ivory.Opts.Overflow as O
import qualified Ivory.Opts.DivZero as O
import qualified Ivory.Opts.Index as O
import qualified Ivory.Opts.FP as O
import qualified Ivory.Opts.CFG as G

import Control.Monad (when)
import Data.List (foldl')
import Data.Monoid (Monoid(..),mconcat)
import System.Console.GetOpt
    (ArgOrder(Permute),OptDescr(..),ArgDescr(..),getOpt,usageInfo)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs,getProgName)
import System.Exit (exitFailure,exitSuccess)
import System.FilePath (takeDirectory,takeExtension,addExtension,(</>))
import System.IO (withFile, IOMode(..), hPutStrLn)
import Text.PrettyPrint.Mainland
    ((<+>),(<>),line,text,stack,punctuate,render,empty,indent,displayS)
import qualified System.FilePath.Posix as PFP


-- Option Parsing --------------------------------------------------------------

data OptParser opt
  = Success (opt -> opt)
  | Error [String]

instance Monoid (OptParser opt) where
  mempty                        = Success id

  -- left-to-right composition makes the last option parsed take precedence
  Success f `mappend` Success g = Success (f . g)
  Error as  `mappend` Error bs  = Error (as ++ bs)
  Error as  `mappend` _         = Error as
  _         `mappend` Error bs  = Error bs

-- | Option parser succeeded, use this function to transform the default
-- options.
success :: (opt -> opt) -> OptParser opt
success  = Success

-- | Option parser failed, emit this message.
--
-- XXX currently not used.
--invalid :: String -> OptParser opt
--invalid msg = Error [msg]


-- | Yield either a list of errors, or a function to produce an options
-- structure, given a set of default options.  Discard any non-options.
parseOptions :: [OptDescr (OptParser opt)] -> [String]
             -> (Either [String] (opt -> opt))
parseOptions opts args = case getOpt Permute opts args of
  (fs,[],[]) -> case mconcat fs of
    Success f -> Right f
    Error es  -> Left es
  (_,_,es)  -> Left es



-- Command Line Options --------------------------------------------------------

data Opts = Opts
  { stdOut      :: Bool
  , includeDir  :: FilePath
  , srcDir      :: FilePath
  -- dependencies
  , deps        :: FilePath
  , depPrefix   :: String
  , rtIncludeDir:: Maybe FilePath
  -- optimization passes
  , constFold   :: Bool
  , overflow    :: Bool
  , divZero     :: Bool
  , ixCheck     :: Bool
  , fpCheck     :: Bool
  , outProcSyms :: Bool
  -- CFG stuff
  , cfg         :: Bool
  , cfgDotDir   :: FilePath
  , cfgProc     :: [String]
  -- debugging
  , verbose     :: Bool

  , help        :: Bool
  } deriving (Show)

initialOpts :: Opts
initialOpts  = Opts
  { stdOut       = False
  , includeDir   = ""
  , srcDir       = ""

  -- dependencies
  , deps         = ""
  , depPrefix    = ""
  , rtIncludeDir = Nothing

  -- optimization passes
  , constFold    = False
  , overflow     = False
  , divZero      = False
  , ixCheck      = False
  , fpCheck      = False
  , outProcSyms  = False

  -- CFG stuff
  , cfg          = False
  , cfgDotDir    = ""
  , cfgProc      = []
  -- debugging
  , verbose      = False

  , help         = False
  }

setStdOut :: OptParser Opts
setStdOut  = success (\opts -> opts { stdOut = True } )

setIncludeDir :: String -> OptParser Opts
setIncludeDir str = success (\opts -> opts { includeDir = str })

setSrcDir :: String -> OptParser Opts
setSrcDir str = success (\opts -> opts { srcDir = str })

setDeps :: String -> OptParser Opts
setDeps str = success (\opts -> opts { deps = str })

setDepPrefix :: String -> OptParser Opts
setDepPrefix str = success (\opts -> opts { depPrefix = str })

setRtIncludeDir :: String -> OptParser Opts
setRtIncludeDir str = success (\opts -> opts { rtIncludeDir = Just str })

setConstFold :: OptParser Opts
setConstFold  = success (\opts -> opts { constFold = True })

setOverflow :: OptParser Opts
setOverflow  = success (\opts -> opts { overflow = True })

setDivZero :: OptParser Opts
setDivZero  = success (\opts -> opts { divZero = True })

setIxCheck :: OptParser Opts
setIxCheck  = success (\opts -> opts { ixCheck = True })

setFpCheck :: OptParser Opts
setFpCheck  = success (\opts -> opts { fpCheck = True })

setProcSyms :: OptParser Opts
setProcSyms  = success (\opts -> opts { outProcSyms = True })

setCfg :: OptParser Opts
setCfg  = success (\opts -> opts { cfg = True })

setCfgDotDir :: String -> OptParser Opts
setCfgDotDir str = success (\opts -> opts { cfgDotDir = str })

addCfgProc :: String -> OptParser Opts
addCfgProc str = success (\opts -> opts { cfgProc = cfgProc opts ++ [str] })

setVerbose :: OptParser Opts
setVerbose  = success (\opts -> opts { verbose = True })

setHelp :: OptParser Opts
setHelp  = success (\opts -> opts { help = True })

options :: [OptDescr (OptParser Opts)]
options  =
  [ Option "" ["std-out"] (NoArg setStdOut)
    "print to stdout only"
  , Option "" ["include-dir"] (ReqArg setIncludeDir "PATH")
    "output directory for header files"
  , Option "" ["src-dir"] (ReqArg setSrcDir "PATH")
    "output directory for source files"

  , Option "" ["dep-file"] (ReqArg setDeps "FILE")
    "makefile dependency output"
  , Option "" ["dep-prefix"] (ReqArg setDepPrefix "STRING")
    "makefile dependency prefix"
  , Option "" ["rt-include-dir"] (ReqArg setRtIncludeDir "PATH")
    "path to ivory runtime includes"

  , Option "" ["const-fold"] (NoArg setConstFold)
    "enable the constant folding pass"
  , Option "" ["overflow"] (NoArg setOverflow)
    "enable overflow checking annotations"
  , Option "" ["div-zero"] (NoArg setDivZero)
    "generate assertions checking for division by zero."
  , Option "" ["ix-check"] (NoArg setIxCheck)
    "generate assertions checking for back indexes (e.g., negative)"
  , Option "" ["fp-check"] (NoArg setFpCheck)
    "generate assertions checking for NaN and Infinitiy."

  , Option "" ["out-proc-syms"] (NoArg setProcSyms)
    "dump out the modules' function symbols"

  , Option "" ["cfg"] (NoArg setCfg)
    "Output control-flow graph and max stack usage."

  , Option "" ["cfg-dot-dir"] (ReqArg setCfgDotDir "PATH")
    "output directory for CFG Graphviz file"
  , Option "" ["cfg-proc"] (ReqArg addCfgProc "NAME")
    "entry function(s) for CFG computation."

  , Option "" ["verbose"] (NoArg setVerbose)
    "verbose debugging output"

  , Option "h" ["help"] (NoArg setHelp)
    "display this message"
  ]

-- | Parse an @Opts@ structure from a list of strings.
parseOpts :: [String] -> IO Opts
parseOpts args = case parseOptions options args of
  Right f   ->
    let opts = f initialOpts
     in if help opts
           then printUsage [] >> exitSuccess
           else return opts
  Left errs -> printUsage errs >> exitFailure

printUsage :: [String] -> IO ()
printUsage errs = do
  prog <- getProgName
  let banner = unlines
        (errs ++ ["", "Usage: " ++ prog ++ " [OPTIONS]"])
  putStrLn (usageInfo banner options)


-- Code Generation Front End ---------------------------------------------------

compile :: [Module] -> IO ()
compile = compileWith Nothing Nothing

compileWith :: Maybe G.SizeMap -> Maybe [IO FilePath] -> [Module] -> IO ()
compileWith sm sp ms = runCompilerWith sm sp ms =<< parseOpts =<< getArgs

runCompilerWith :: Maybe G.SizeMap -> Maybe [IO FilePath] -> [Module] -> Opts -> IO ()
runCompilerWith sm sp =
  rc (maybe G.defaultSizeMap id sm) (maybe [] id sp)

runCompiler :: [Module] -> Opts -> IO ()
runCompiler = runCompilerWith Nothing Nothing

rc :: G.SizeMap -> [IO FilePath] -> [Module] -> Opts -> IO ()
rc sm userSearchPath modules opts
  | outProcSyms opts = C.outputProcSyms modules
  | printDeps        = runDeps
  | otherwise        = do
    if stdOut opts then mapM_ showM_ cmodules else run
    -- CFG stuff
    when (cfg opts) $ do
      cfs <- mapM (\p -> G.callGraphDot p (cfgDotDir opts) optModules) cfgps
      let maxstacks = map ms (zip cfgps cfs)
      mapM_ maxStackMsg (zip cfgps maxstacks)
  where
  run = do
    searchPath <- mkSearchPath opts userSearchPath
    createDirectoryIfMissing True (includeDir opts)
    createDirectoryIfMissing True (srcDir opts)
    outputHeaders (includeDir opts) cmodules
    outputSources (srcDir opts) cmodules
    C.outputSourceDeps (includeDir opts) (srcDir opts)
       ("runtime/ivory.h":(C.collectSourceDeps modules)) searchPath

  runDeps =
    outputDeps (deps opts) (depPrefix opts) (genHs ++ cpyHs) (genSs ++ cpySs)
    where
    sdeps = C.collectSourceDeps modules
    genHs = map (mkDep (includeDir opts) ".h") cmodules
    genSs = map (mkDep (srcDir opts) ".c")     cmodules
    cpyHs = map (mkDepSourceDep (includeDir opts)) $
              filter (\p -> takeExtension p == ".h") sdeps
    cpySs = map (mkDepSourceDep (srcDir opts)) $
              filter (\p -> takeExtension p == ".c") sdeps

  optModules = map (C.runOpt passes) modules

  cfgps = cfgProc opts

  ms (p, cf) = G.maxStack p cf sm
  maxStackMsg :: (String, G.WithTop Integer) -> IO ()
  maxStackMsg (p,res) =
    putStrLn $ "Maximum stack usage from function " ++ p ++ ": " ++ show res

  cmodules   = map C.compileModule optModules

  printDeps = case deps opts of
    []  -> False
    [_] -> True
    _   -> error "invalid option for deps"

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
    C.writeHdr (verbose opts) (addExtension basename ".h")
               (C.headers cm) (C.unitName cm)

  -- Transform a compiled unit into a c src, and write to a .c file
  outputSrc :: FilePath -> C.CompileUnits -> IO ()
  outputSrc basename cm = do
    C.writeSrc (verbose opts) (addExtension basename ".c")
               (C.sources cm)

--------------------------------------------------------------------------------

mkDep :: FilePath -> String -> C.CompileUnits -> String
mkDep basepath extension unit = basepath PFP.</> (C.unitName unit) PFP.<.> extension

mkDepSourceDep :: FilePath -> FilePath -> String
mkDepSourceDep basepath sdep = basepath PFP.</> sdep

outputDeps :: [FilePath] -> String -> [String] -> [String] -> IO ()
outputDeps [path] prefix headers sources = do
  createDirectoryIfMissing True (takeDirectory path)
  withFile path WriteMode writedoc
  where
  writedoc h = hPutStrLn h (displayS (render w d) "")
  w = 10000000 -- don't ever wrap lines - invalid make syntax
  d = stack $
    [ text "# dep file autogenerated by ivory compiler"
    , empty
    , listof (prefix ++ "_HEADERS") headers
    , empty
    , listof (prefix ++ "_SOURCES") sources
    ]
  declaration n = text n <+> text ":= \\" <> line
  listof name values = declaration name <>
    (indent 4 $ stack $ punctuate (text " \\") (map text values))
outputDeps _ _ _ _ = error "invalid dep path (should be prevented by caller)"


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

