{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Ivory.Compile.C.CmdlineFrontend (runCompiler, Opts(..), initialOpts)
import Ivory.Language (Module)
import Ivory.Artifact (Artifact)
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import System.Console.GetOpt.Simple
import System.FilePath
import Data.Char
import Data.List (intersperse)
import Data.Typeable

import qualified Data.Map as M

deriving instance Typeable Module
deriving instance Typeable Artifact

icCfg :: Conf
icCfg = [ (arg, "output", Optional, "Output file name")
        , (arg, "input",  Optional, "Input file name")
        ]

pkgDb :: M.Map String String -> Maybe String
pkgDb = M.lookup "pkg-db"

main :: IO ()
main = do
    (opts0, iptMb) <- getUsingConf icCfg ["input"]
    let opts = M.insert "input" ipt opts0
        ipt  = head iptMb
        iopt = initialOpts { outDir  = M.lookup "output" opts }

    (ivoryModules, ivoryArtifacts) <- interpFile ipt (pkgDb opts)
    runCompiler ivoryModules ivoryArtifacts iopt

interpFile :: FilePath -> Maybe FilePath -> IO ([Module], [Artifact])
interpFile file db =
  do res <- interp
     case res of
         Left err -> error $ renderError err
         Right prog -> return prog
 where
    customInterp = case db of
                        Nothing -> runInterpreter
                        Just fp ->
                            unsafeRunInterpreterWithArgs [ "-no-user-package-db"
                                                         , "package-db " ++ fp
                                                         ]
    fileModule = let (s:str) = dropExtension $ takeBaseName file
                 in map (\x -> if x == '-' then '_' else x) (toUpper s : str)
    interp = customInterp $ do
        loadModules [file]
        setImportsQ [ ( fileModule, Nothing )
                    , ( "Ivory.Language", Nothing)
                    , ( "Ivory.Artifact", Nothing)
                    , ( "Ivory.Compile.C.CmdlineFrontend", Nothing) ]
        interpret "main" infer

renderError :: InterpreterError -> String
renderError x =
 case x of
  UnknownError msg -> msg
  WontCompile errs -> concat . intersperse "\n\n" . map errMsg $ errs
  NotAllowed   msg -> msg
  GhcException msg -> msg
