{-# LANGUAGE OverloadedStrings #-}
--XXX testing
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.ModelCheck where

import qualified Ivory.Language.Syntax       as I
import           Ivory.Language.Syntax.Concrete.Location
import           Text.Printf
import           Ivory.ModelCheck.Ivory2CVC4
import           Ivory.ModelCheck.Monad
import           Ivory.ModelCheck.CVC4

import           System.FilePath.Posix
import           System.Directory
import           System.Process
import           System.IO
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8       as B
import           Data.List

--------------------------------------------------------------------------------

data Args = Args
  { printQuery  :: Bool
  , printEnv    :: Bool
  , printLocs   :: Bool
  , inlineCall  :: Bool -- ^ Should we inline `call`s or just assume the `ensures`?
  , callCVC4    :: Bool
  , cvc4Path    :: FilePath
  , cvc4Args    :: [String]
  } deriving (Show, Eq)

initArgs :: Args
initArgs = Args
  { printQuery = True
  , printEnv   = True
  , printLocs  = True
  , inlineCall = False
  , callCVC4   = True
  , cvc4Path   =  ""
  , cvc4Args   = ["--incremental"]
  }

--------------------------------------------------------------------------------

data Result = Safe | Unsafe [String] | Inconsistent | Error String
            deriving (Show, Eq)

isSafe :: Result -> Bool
isSafe Safe = True
isSafe _    = False

isUnsafe :: Result -> Bool
isUnsafe (Unsafe _) = True
isUnsafe _          = False

isError :: Result -> Bool
isError (Error _) = True
isError _         = False

showResult :: Result -> String
showResult Safe         = "Safe"
showResult Inconsistent = "Inconsistent"
showResult (Unsafe qs)  = "Unsafe: " ++ intercalate ", " qs
showResult (Error e)    = "Error: " ++ e

modelCheck' :: I.Module -> IO ()
modelCheck' m = do
  (res, file) <- modelCheck initArgs m
  print file
  print res

modelCheck :: Args -> I.Module -> IO (Result, FilePath)
modelCheck args m = do
  let (_, st) = runMC (SymOpts (inlineCall args)) (modelCheckMod m)
  let bs = B.unlines (mkScript st)
  debugging args st bs
  file <- writeInput bs
  out  <- reverse <$> runCVC4 args file
  case out of
   ("valid":_) -> return (Inconsistent, file)
   ("invalid":results)
     | all (=="valid") results -> return (Safe, file)
     | otherwise -> return (Unsafe bad, file)
     where
       bad = [ B.unpack $ concrete q
             | (q, "invalid") <- zip (tail $ allQueries st) results
             ]
   _ -> return (Error (show out), file)

--------------------------------------------------------------------------------

debugging :: Args -> SymExecSt -> B.ByteString -> IO ()
debugging args st bs = do
  when (printQuery args) $ do
    putStrLn "**** QUERY ************************************"
    B.putStrLn bs
    putStrLn "***********************************************"
    putStrLn ""

  when (printEnv args) $ do
    putStrLn "**** ENV **************************************"
    print (symEnv st)
    putStrLn "***********************************************"
    putStrLn ""

--------------------------------------------------------------------------------

mkScript :: SymExecSt -> [B.ByteString]
mkScript st =
  [ "% Script auto-generated for model-checking Ivory function "
  , B.pack (funcSym st)
  , ""
  , "% CVC4 Lib -----------------------------------"
  , ""
  ] ++ map concrete cvc4Lib
  ++
  [ ""
  , "% user-defined types -------------------------"
  , ""
  ] ++ writeStmts (map (uncurry typeDecl) . types . symSt)
  ++
  [ ""
  , "% declarations -------------------------------"
  , ""
  ] ++ writeStmts (decls . symSt)
  ++
  [ ""
  , "% program encoding ---------------------------"
  , ""
  ] ++ writeStmts (map assert . invars . symSt)
  ++
  [ ""
  , "% queries ------------------------------------"
  , ""
  ] ++ writeStmts allQueries
  where
  writeStmts :: Concrete a
             => (SymExecSt -> [a])
             -> [B.ByteString]
  writeStmts f = map concrete (reverse $ f st)

-- | Are the assertions consistent?  If not, there's a bug in the
-- model-checking.
consistencyQuery :: Statement
consistencyQuery = query $ noLoc false

allQueries :: SymExecSt -> [Statement]
allQueries st =
  consistencyQuery : (map query . assertQueries . symQuery) st

-- | Write model inputs to a temp file.
writeInput :: B.ByteString -> IO FilePath
writeInput bs = do
  dir <- getTemporaryDirectory
  let tempDir = dir </> "cvc4-inputs"
  createDirectoryIfMissing False tempDir
  (file, hd) <- openTempFile tempDir "cvc4input.cvc"
  -- putStrLn $ "Created temp file " ++ file ++ "\n"
  B.hPut hd bs
  hClose hd
  return file

-- | Run cvc4 on the input file returning the results.
runCVC4 :: Args -> FilePath -> IO [String]
runCVC4 args file = do
  (_, Just hout, _, _) <- createProcess $ (proc exec execArgs)
      { std_out = CreatePipe }
  out <- hGetContents hout
  return (lines out)
  where
  exec     = cvc4Path args </> "cvc4"
  execArgs = cvc4Args args ++ [file]

printResults :: SymExecSt -> [String] -> IO ()
printResults st results = do
  let queries = map concrete
              $ reverse
              $ allQueries st
  let match = reverse (zip queries results)
  B.putStrLn "*** If \'Query FALSE\' is valid, the assertions are inconsistent. ***\n"
  mapM_ printRes match
  where
  printRes (q,res) = printf "%-30s : %s\n" (B.unpack q) res

