{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Ivory.Compile.C.CmdlineFrontend
  ( compile
  , compileWithSizeMap
  , runCompiler
  , runCompilerWithSizeMap
  , Opts(..)
  , initialOpts
  ) where

import Control.Monad
import System.Console.CmdLib
import System.FilePath
import System.Directory
import System.IO (withFile, IOMode(..))
import Data.List hiding (group)

import Text.PrettyPrint.Leijen hiding ((</>), group)

import Ivory.Language
import qualified Ivory.Compile.C as C
import qualified Ivory.Opts.ConstFold as O
import qualified Ivory.Opts.Overflow as O
import qualified Ivory.Opts.DivZero as O
import qualified Ivory.Opts.FP as O
import qualified Ivory.Opts.CFG as G

data Opts
  = Opts
    { stdOut      :: Bool
    , includeDir  :: FilePath
    , srcDir      :: FilePath
    -- dependencies
    , deps        :: [FilePath]
    , depPrefix   :: String
    -- optimization passes
    , constFold   :: Bool
    , overflow    :: Bool
    , divZero     :: Bool
    , fpCheck     :: Bool
    , outProcSyms :: Bool
    -- CFG stuff
    , cfg         :: Bool
    , cfgDotDir   :: FilePath
    , cfgProc     :: [String]
    -- debugging
    , verbose     :: Bool
    } deriving (Eq, Data, Typeable)

instance Attributes Opts where
  attributes _ = group "Options"
    [ stdOut      %> [ Help "print to standard out only"
                     , Default False
                     ]
    , includeDir  %> [ Help "output directory for header files"
                     , ArgHelp "PATH"
                     , Default "."
                     ]
    , srcDir      %> [ Help "output directory for source files"
                     , ArgHelp "PATH"
                     , Default "."
                     ]
    , constFold   %> [ Help "constant folding."
                     , Default False
                     , Invertible True
                     ]
    , overflow    %> [ Help "generate assertions checking for arithmetic overflow/underflow."
                     , Default False
                     , Invertible True
                     ]
    , divZero     %> [ Help "generate assertions checking for division by zero."
                     , Default False
                     , Invertible True
                     ]
    , fpCheck     %> [ Help "generate assertions checking for NaN and Infinitiy."
                     , Default False
                     , Invertible True
                     ]
    , outProcSyms %> [ Help "write to standard out the modules' function symbols."
                     ]
    -- CFG stuff
    , cfg         %> [ Help "Output control-flow graph and max stack usage."
                     , Default False
                     , Invertible True
                     ]
    , cfgDotDir   %> [ Help "output directory for CDG Graphviz file."
                     , ArgHelp "PATH"
                     , Default "."
                     ]
    , cfgProc     %> [ Help "entry function(s) for CFG computation."
                     , ArgHelp "[proc0, proc1, ...]"
                     , Default ([] :: [String])
                     ]
    , verbose     %> [ Help "verbose debugging output"
                     , Default False
                     , Invertible True
                     ]
    ]

instance RecordCommand Opts where
  mode_summary _  = "Ivory compilation frontend"

initialOpts :: Opts
initialOpts = Opts
  { stdOut      = False
  , includeDir  = "."
  , srcDir      = "."
  -- dependencies
  , deps        = []
  , depPrefix   = "."
  -- optimization passes
  , constFold   = False
  , overflow    = False
  , divZero     = False
  , fpCheck     = False
  , outProcSyms = False
  -- CFG stuff
  , cfg         = False
  , cfgDotDir   = "."
  , cfgProc     = ["main"]
  -- debugging
  , verbose     = False
  }

compile :: [Module] -> IO ()
compile ms = runCompiler ms =<< executeR initialOpts =<< getArgs

compileWithSizeMap :: G.SizeMap -> [Module] -> IO ()
compileWithSizeMap st ms = runCompilerWithSizeMap st ms =<< executeR initialOpts =<< getArgs

runCompilerWithSizeMap :: G.SizeMap -> [Module] -> Opts -> IO ()
runCompilerWithSizeMap sm = rc sm

runCompiler :: [Module] -> Opts -> IO ()
runCompiler = rc G.defaultSizeMap

rc :: G.SizeMap -> [Module] -> Opts -> IO ()
rc sm modules opts
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
  run = do createDirectoryIfMissing True (includeDir opts)
           createDirectoryIfMissing True (srcDir opts)
           outputHeaders (includeDir opts) cmodules
           outputSources (srcDir opts) cmodules

  runDeps = do
    outputDeps (deps opts) (depPrefix opts) hs ss
    where
    hs = map (mkDep (includeDir opts) ".h") cmodules
    ss = map (mkDep (srcDir opts) ".c")     cmodules

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

  showM_ = (mapM_ . mapM_) putStrLn . C.showModule

  cfPass = mkPass constFold O.constFold
  ofPass = mkPass overflow O.overflowFold
  dzPass = mkPass divZero O.divZeroFold
  fpPass = mkPass fpCheck O.fpFold
  mkPass passOpt pass = if passOpt opts then pass else id

  -- Constant folding before and after all other passes.
  passes e = foldl' (flip ($)) e
    [ cfPass
    , ofPass, dzPass, fpPass
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
  outputHeader basename cm =
    C.writeHdr (verbose opts) (addExtension basename ".h")
               (C.headers cm) (C.unitName cm)

  -- Transform a compiled unit into a c src, and write to a .c file
  outputSrc :: FilePath -> C.CompileUnits -> IO ()
  outputSrc basename cm =
    C.writeSrc (verbose opts) (addExtension basename ".c") (C.sources cm)

--------------------------------------------------------------------------------

mkDep :: FilePath -> String -> C.CompileUnits -> FilePath
mkDep basepath extension unit = basepath </> (C.unitName unit) <.> extension

outputDeps :: [FilePath] -> String -> [FilePath] -> [FilePath] -> IO ()
outputDeps [path] prefix headers sources = do
  createDirectoryIfMissing True (takeDirectory path)
  withFile path WriteMode writedoc
  where
  writedoc h = displayIO h rendered
  rendered = renderPretty 1.0 w d
  w = 10000000 -- don't ever wrap lines - invalid make syntax
  d = vsep $
    [ text "# dep file autogenerated by ivory compiler"
    , empty
    , listof (prefix ++ "_HEADERS") headers
    , empty
    , listof (prefix ++ "_SOURCES") sources
    ]
  declaration n = text n <+> text ":= \\" <> line
  listof name values = declaration name <>
    (indent 4 $ vsep $ punctuate (text " \\") (map text values))
outputDeps _ _ _ _ = error "invalid dep path (should be prevented by caller)"

