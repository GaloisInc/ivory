module Ivory.Compile.C.CmdlineFrontend.Options where

import Prelude ()
import Prelude.Compat

import System.Console.GetOpt
    (ArgOrder(Permute),OptDescr(..),ArgDescr(..),getOpt,usageInfo)

import System.Environment (getProgName)
import System.Exit (exitFailure,exitSuccess)

-- Option Parsing --------------------------------------------------------------

data OptParser opt = OptParser [String] (opt -> opt)

instance Monoid (OptParser opt) where
  mempty = OptParser [] id
  -- left-to-right composition makes the last option parsed take precedence
  OptParser as f `mappend` OptParser bs g = OptParser (as ++ bs) (f . g)

-- | Option parser succeeded, use this function to transform the default
-- options.
success :: (opt -> opt) -> OptParser opt
success  = OptParser []

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
    OptParser [] f -> Right f
    OptParser es _ -> Left es
  (_,_,es)  -> Left es



-- Command Line Options --------------------------------------------------------

data Opts = Opts
  { outDir      :: Maybe FilePath
  -- ^ output directory for all files (or standard out).
  , outHdrDir   :: Maybe FilePath
  -- ^ if set, output directory for headers. Otherwise, use @outDir@.
  -- optimization passes
  , outArtDir   :: Maybe FilePath
  -- ^ if set, output directory for artifacts. Otherwise, use @outDir@.

  -- optimization passes
  , constFold   :: Bool
  , overflow    :: Bool
  , divZero     :: Bool
  , ixCheck     :: Bool
  , fpCheck     :: Bool
  , outProcSyms :: Bool
  , bitShiftCheck :: Bool
  -- CFG stuff
  , cfg         :: Bool
  , cfgDotDir   :: FilePath
  , cfgProc     :: [String]
  -- debugging
  , verbose     :: Bool
  , srcLocs     :: Bool
  -- Typechecking
  , tcWarnings  :: Bool
  , tcErrors    :: Bool
  , scErrors    :: Bool

  , help        :: Bool
  } deriving (Show)

initialOpts :: Opts
initialOpts  = Opts
  { outDir       = Just ""
  , outHdrDir    = Nothing
  , outArtDir    = Nothing

  -- optimization/safety passes
  , constFold    = False
  , overflow     = False
  , divZero      = False
  , ixCheck      = False
  , fpCheck      = False
  , outProcSyms  = False
  , bitShiftCheck = False

  -- CFG stuff
  , cfg          = False
  , cfgDotDir    = ""
  , cfgProc      = []
  -- debugging
  , verbose      = False
  , srcLocs      = False
  , tcWarnings   = False
  , tcErrors     = True
  , scErrors     = True
  , help         = False
  }

setStdOut :: OptParser Opts
setStdOut  = success (\opts -> opts { outDir = Nothing } )

setOutDir :: String -> OptParser Opts
setOutDir str = success (\opts -> opts { outDir = Just str })

setHdrDir :: String -> OptParser Opts
setHdrDir str = success (\opts -> opts { outHdrDir = Just str })

setArtDir :: String -> OptParser Opts
setArtDir str = success (\opts -> opts { outArtDir = Just str })

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

setBitShiftCheck :: OptParser Opts
setBitShiftCheck  = success (\opts -> opts { bitShiftCheck = True })

setCfg :: OptParser Opts
setCfg  = success (\opts -> opts { cfg = True })

setCfgDotDir :: String -> OptParser Opts
setCfgDotDir str = success (\opts -> opts { cfgDotDir = str })

addCfgProc :: String -> OptParser Opts
addCfgProc str = success (\opts -> opts { cfgProc = cfgProc opts ++ [str] })

setVerbose :: OptParser Opts
setVerbose  = success (\opts -> opts { verbose = True })

setSrcLocs :: OptParser Opts
setSrcLocs  = success (\opts -> opts { srcLocs = True })

setWarnings :: OptParser Opts
setWarnings = success (\opts -> opts { tcWarnings = True })

setErrors :: Bool -> OptParser Opts
setErrors b = success (\opts -> opts { tcErrors = b })

setSanityCheck :: Bool -> OptParser Opts
setSanityCheck b = success (\opts -> opts { scErrors = b })

setHelp :: OptParser Opts
setHelp  = success (\opts -> opts { help = True })

options :: [OptDescr (OptParser Opts)]
options  =
  [ Option "" ["std-out"] (NoArg setStdOut)
    "print to stdout only"

  , Option "" ["src-dir"] (ReqArg setOutDir "PATH")
    "output directory for source files"

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
  , Option "" ["bitshift-check"] (NoArg setBitShiftCheck)
    "generate assertions checking for bit-shift overflow."

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

  , Option "" ["srclocs"] (NoArg setSrcLocs)
    "output source locations from the Ivory code"

  , Option "" ["tc-warnings"] (NoArg setWarnings)
    "show type-check warnings"

  , Option "" ["tc-errors"] (NoArg $ setErrors True)
    "Abort on type-check errors (default)"

  , Option "" ["no-tc-errors"] (NoArg $ setErrors False)
    "Treat type-check errors as warnings"

  , Option "" ["sanity-check"] (NoArg $ setSanityCheck True)
    "Enable sanity-check"

  , Option "" ["no-sanity-check"] (NoArg $ setSanityCheck False)
    "Disable sanity-check"

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


