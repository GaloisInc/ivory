module Ivory.Artifact
  ( Artifact(..)
  , AContents(..)
  , artifactFile
  , artifactCabalFile
  , artifactText
  , artifactString
  , putArtifacts
  , printArtifacts
  ) where

import Data.Either (lefts, rights)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.FilePath
import System.Directory

data Artifact = Artifact FilePath AContents
data AContents = LiteralContents T.Text
               | FileContents (IO FilePath)


artifactFile :: FilePath -> IO FilePath -> Artifact
artifactFile outputname inputpath =
  Artifact (takeFileName outputname) (FileContents inputpath)

artifactCabalFile :: IO FilePath -> FilePath -> Artifact
artifactCabalFile inputdir inputfname =
  Artifact (takeFileName inputfname)
    (FileContents (fmap (\i -> (i </> inputfname)) inputdir))

artifactText :: FilePath -> T.Text -> Artifact
artifactText outputname t =
  Artifact (takeFileName outputname) (LiteralContents t)

artifactString :: FilePath -> String -> Artifact
artifactString f s = artifactText f (T.pack s)

-- Write a set of artifacts to a given directory.
-- Return value is an error report.
putArtifacts :: FilePath -> [Artifact] -> IO (Maybe String)
putArtifacts dir as = aux putcontents as
  where
  putcontents :: Artifact -> IO (Either String (IO ()))
  putcontents (Artifact fname c) = case c of
    LiteralContents t -> return $ Right $ T.writeFile (dir </> fname) t
    FileContents getf -> do
      srcpath <- getf
      -- Check if srcpath exists. If it does not, give an error
      exists <- doesFileExist srcpath
      case exists of
        True -> return $ Right $ copyFile srcpath (dir </> fname)
        False -> return $ Left $ "Path " ++ srcpath ++ " (for Artifact named "
                                 ++ fname ++ ") could not be found."

printArtifacts :: [Artifact] -> IO (Maybe String)
printArtifacts as = aux printcontents as
  where
  printcontents :: Artifact -> IO (Either String (IO ()))
  printcontents (Artifact fname c) = case c of
    LiteralContents t -> return $ Right $ do
      putStrLn ("Artifact " ++ fname)
      putStrLn  "==================="
      T.putStrLn t
      putStrLn  "==================="
    FileContents getf -> do
      srcpath <- getf
      -- Check if srcpath exists. If it does not, give an error
      exists <- doesFileExist srcpath
      case exists of
        True -> return $ Right $ do
          putStrLn ("Artifact " ++ fname ++ " from " ++ srcpath)
          putStrLn  "==================="
          T.readFile srcpath >>= T.putStrLn
          putStrLn  "==================="
        False -> return $ Left $ "Path " ++ srcpath ++ " (for Artifact named "
                                 ++ fname ++ ") could not be found."

aux :: (Artifact -> IO (Either String (IO ()))) -> [Artifact]
    -> IO (Maybe String)
aux contents as =  do
  output <- mapM contents as
  sequence_ (rights output)
  case lefts output of
    [] -> return Nothing
    es -> return $ Just $
            "putArtifacts had the following failures: \n" ++ unlines es

