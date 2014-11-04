module Ivory.Artifact (
  -- * Ivory Artifacts
  -- | Artifacts permit the user to generate non-Ivory language files
  -- as part of an Ivory program's build process. Artifacts are typically
  -- used to encapsulate helper code (such as Ivory.Stdlib's string functions
  -- helper .c and .h files) or to generate debug or metadata output. The
  -- contents of an `Artifact` are given by some external file, or as a string.
  --
  -- Artifacts are exposed as an abstract type `Artifact` with a set of
  -- constructors, an accessor `artifactFileName`, and two functions,
  -- `putArtifacts` and `printArtifacts`, which are used to write a set of
  -- Artifacts to a file or print them to stdout, respectively.
  --
    Artifact()
  -- | Gives the file name that will be used when writing the `Artifact` to the
  -- filesystem.
  , artifactFileName

  -- * Artifact constructors
  -- | `artifactFile` creates an `Artifact` from an output filename of type
  -- `FilePath` and an input filepath of type `IO FilePath`. The output filename
  -- should be a simple filename, any directory information will be dropped. The
  -- input filepath should be an absolute path. The input filepath is in the IO
  -- monad for compatiblility with Cabal's `getDataDir` data files functionality.
  , artifactFile

  -- | `artifactCabalFile` creates an `Artifact` given an input directory of
  -- type `IO FilePath` and an input filepath, relative to the input directory
  -- in the preceeding argument, of type `FilePath`.
  --
  -- This function is designed to be used with a Cabal Paths_(packagename)
  -- `getDataDir`, which has type `IO FilePath`, and a path to a file inside the
  -- data directory.  It is implemented in terms of `artifactFileName`. The
  -- output filename is given by dropping directory information from the input
  -- filepath.
  , artifactCabalFile

  -- | `artifactText` creates an `Artifact` from an output filename of type
  -- `FilePath` and with contents of type `Text`.
  , artifactText
  -- | `artifactString` creates an `Artifact` from an output filename of type
  -- `FilePath` with contents of type `String`. Implemented in terms of
  -- `artifactText`.
  , artifactString

  -- * Artifact actions
  -- | Takes a directory of type `FilePath` and a list of `Artifact`s, and
  -- writes each Artifact to the filesystem. Gives a `Maybe String` containing
  -- any errors encountered when an `Artifact` is specified by an input filename
  -- which does not exist.
  , putArtifacts

  -- | Takes a list of `Artifact`s and prints them to stdout.
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

artifactFileName :: Artifact -> FilePath
artifactFileName (Artifact f _ ) = f

artifactFile :: FilePath -> IO FilePath -> Artifact
artifactFile outputname inputpath =
  Artifact (takeFileName outputname) (FileContents inputpath)

artifactCabalFile :: IO FilePath -> FilePath -> Artifact
artifactCabalFile inputdir inputfname =
  artifactFile (takeFileName inputfname)
               (fmap (\i -> (i </> inputfname)) inputdir)

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

