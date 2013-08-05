{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Ivory.Compile.C.CmdlineFrontend.Options where

import Data.Monoid (Monoid(..),mconcat)
import System.Console.GetOpt
    (ArgOrder(Permute),OptDescr(..),ArgDescr(..),getOpt,usageInfo)

import System.Environment (getProgName)
import System.Exit (exitFailure,exitSuccess)

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


