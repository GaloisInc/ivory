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
import qualified Data.ByteString.Char8       as B

-- XXX testing
import Ivory.Language hiding (assert, true, false, proc)
import qualified Ivory.Language as L

--------------------------------------------------------------------------------

data Args = Args
  { printQuery  :: Bool
  , callCVC4    :: Bool
  , cvc4Path    :: FilePath
  , modules     :: [I.Module]
  , cvc4Args    :: [String]
  } deriving (Show, Eq)

modelCheck :: I.Module -> IO ()
modelCheck m = do
  let (_, st) = runMC (modelCheckMod m)
  runCVC4 (B.unlines $ mkScript st)

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

runCVC4 :: B.ByteString -> IO ()
runCVC4 bs = do
  dir <- getTemporaryDirectory
  let tempDir = dir </> "cvc4-inputs"
  createDirectoryIfMissing False tempDir
  (file, hd) <- openTempFile tempDir "cvc4input.cvc"
  putStrLn $ "Created temp file " ++ file
  B.hPut hd bs
  hClose hd
  (_, Just hout, _, _) <-
    createProcess (proc "cvc4" ["--incremental", file])
      { std_out = CreatePipe }
  out <- hGetContents hout
  putStrLn out

--------------------------------------------------------------------------------
-- XXX testing

str = B.pack "QUERY TRUE;"

fooProc :: Def ('[Uint8, Uint8] :-> ())
fooProc = L.proc "foo" $ \y x -> body $ do
  -- z <- assign x
  -- L.assert (z <? 3)
  ifte_ (y <? 3)
    (do ifte_ (y ==? 3)
              (L.assert L.false)
              retVoid)
    (do z <- assign x
        L.assert (z >=? 3))
  retVoid

m :: Module
m = package "foo" (incl fooProc)
