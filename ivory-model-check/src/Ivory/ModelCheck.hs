{-# LANGUAGE OverloadedStrings #-}
--XXX testing
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}


module Ivory.ModelCheck where

import qualified Ivory.Language.Syntax       as I
import           Ivory.ModelCheck.Ivory2CVC4
import           Ivory.ModelCheck.Monad
import           Ivory.ModelCheck.CVC4

import           System.FilePath.Posix
import           System.Directory
import           System.Process
import           System.IO
import           Control.Monad
import qualified Data.ByteString.Char8       as B

-- XXX testing
import Ivory.Language hiding (assert, true, false, proc)
import qualified Ivory.Language as L

--------------------------------------------------------------------------------

data Args = Args
  { printQuery  :: Bool
  , printEnv    :: Bool
  , callCVC4    :: Bool
  , cvc4Path    :: FilePath
  , cvc4Args    :: [String]
  } deriving (Show, Eq)

initArgs :: Args
initArgs = Args
  { printQuery = True
  , printEnv   = True
  , callCVC4   = True
  , cvc4Path   =  ""
  , cvc4Args   = ["--incremental"]
  }

--------------------------------------------------------------------------------

modelCheck' :: I.Module -> IO ()
modelCheck' = modelCheck initArgs

modelCheck :: Args -> I.Module -> IO ()
modelCheck args m = do

  let (_, st) = runMC (modelCheckMod m)
  let bs = B.unlines (mkScript st)
  debugging args st bs
  file <- writeInput bs
  printResults st =<< runCVC4 args file

--------------------------------------------------------------------------------

debugging :: Args -> SymExecSt -> B.ByteString -> IO ()
debugging args st bs = do
  when (printQuery args) $ do
    putStrLn "                  **** QUERY ****"
    B.putStrLn bs
    B.putStrLn "                    ******"
    B.putStrLn ""

  when (printEnv args) $ do
    putStrLn "                  **** ENV ****"
    print (symEnv st)
    B.putStrLn "                   ******"
    B.putStrLn ""

--------------------------------------------------------------------------------

mkScript :: SymExecSt -> [B.ByteString]
mkScript st =
  [ "% Script auto-generated for model-checking Ivory function "
  , B.pack (funcSym st)
  , ""
  , "% declarations -------------------------------"
  , ""
  ] ++ writeStmts (decls . symSt)
  ++
  [ ""
  , "% program encoding ---------------------------"
  , ""
  ] ++ writeStmts (map assert . eqns . symSt)
  ++
  [""
  , "% queries ------------------------------------"
  , ""
  ] ++ writeStmts (map query . assertQueries . symQuery)

  where
  writeStmts :: Concrete a
             => (SymExecSt -> [a])
             -> [B.ByteString]
  writeStmts f = map concrete (reverse $ f st)

-- | Write model inputs to a temp file.
writeInput :: B.ByteString -> IO FilePath
writeInput bs = do
  dir <- getTemporaryDirectory
  let tempDir = dir </> "cvc4-inputs"
  createDirectoryIfMissing False tempDir
  (file, hd) <- openTempFile tempDir "cvc4input.cvc"
  putStrLn $ "Created temp file " ++ file
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
              $ (map query . assertQueries . symQuery) st
  let match = reverse (zip queries results)
  mapM_ printRes match
  where
  printRes (q,res) = B.putStr q >> putStr "  :  " >> putStrLn res

--------------------------------------------------------------------------------
-- XXX testing

str = B.pack "QUERY TRUE;"

foo1 :: Def ('[Uint8, Uint8] :-> ())
foo1 = L.proc "foo1" $ \y x -> body $ do
  -- z <- assign x
  -- L.assert (z <? 3)
  ifte_ (y <? 3)
    (do ifte_ (y ==? 3)
              (L.assert L.false)
              retVoid)
    (do z <- assign x
        L.assert (z >=? 3))
  retVoid

m1 :: Module
m1 = package "foo1" (incl foo1)

-----------------------

foo2 :: Def ('[] :-> ())
foo2 = L.proc "foo2" $ body $ do
  x <- local (ival (0 :: Uint8))
  store x 3
  y <- assign x
  z <- deref y
  L.assert (z ==? 3)
  retVoid

m2 :: Module
m2 = package "foo2" (incl foo2)
