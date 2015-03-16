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
  -- `putArtifact` and `printArtifact`, which are used to write an
  -- Artifacts to a file or print it to stdout, respectively.
  --
  -- Users may specify transformations on the contents of an artifact.
  -- Optionally, these transformations can throw an error. This is useful when
  -- using artifact files as templates.
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

  -- | `artifactTransform` and `artifactTransformString` specify a
  -- transformation on the contents of an `Artifact`.
  , artifactTransform, artifactTransformString

  -- | `artifactTransformErr` and `artifactTransformErrString` specify a
  -- transformation on the contents of an `Artifact` which may give an error.
  , artifactTransformErr, artifactTransformErrString

  -- * Artifact actions
  -- | Takes a directory of type `FilePath` and an `Artifact`
  -- writes each `Artifact` to the file system or gives an error explaining why
  -- not. `Maybe String` containins errors encountered when an `Artifact` is
  -- transformed, or specified by an input filename which does not exist.
  , putArtifact
  -- | like `putArtifact` but ignores any errors.
  , putArtifact_

  -- | Takes an `Artifact` and prints it, or an appropriate error message, to
  -- stdout.
  , printArtifact
  ) where

import Control.Monad (void, when)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.FilePath
import System.Directory
import Ivory.Artifact.Transformer

data Artifact =
  Artifact
    { artifact_outputname  :: FilePath
    , artifact_contents    :: AContents
    , artifact_transformer :: Transformer T.Text
    }

data AContents = LiteralContents T.Text
               | FileContents (IO FilePath)

artifactFileName :: Artifact -> FilePath
artifactFileName = artifact_outputname

artifactFile :: FilePath -> IO FilePath -> Artifact
artifactFile outputname inputpath = Artifact
  { artifact_outputname  = takeFileName outputname
  , artifact_contents    = FileContents inputpath
  , artifact_transformer = emptyTransformer
  }

artifactCabalFile :: IO FilePath -> FilePath -> Artifact
artifactCabalFile inputdir inputfname =
  artifactFile (takeFileName inputfname)
               (fmap (\i -> (i </> inputfname)) inputdir)

artifactText :: FilePath -> T.Text -> Artifact
artifactText outputname t = Artifact
  { artifact_outputname  = takeFileName outputname
  , artifact_contents    = LiteralContents t
  , artifact_transformer = emptyTransformer
  }

artifactString :: FilePath -> String -> Artifact
artifactString f s = artifactText f (T.pack s)

artifactTransform :: (T.Text -> T.Text) -> Artifact -> Artifact
artifactTransform f a =
  a { artifact_transformer = transform f (artifact_transformer a) }

artifactTransformString :: (String -> String) -> Artifact -> Artifact
artifactTransformString f a = artifactTransform f' a
  where f' = T.pack . f . T.unpack

artifactTransformErr :: (T.Text -> Either String T.Text) -> Artifact -> Artifact
artifactTransformErr f a =
  a { artifact_transformer = transformErr f (artifact_transformer a) }

artifactTransformErrString :: (String -> Either String String) -> Artifact -> Artifact
artifactTransformErrString f a = artifactTransformErr f' a
  where f' t = fmap T.pack (f (T.unpack t))

getArtifact :: Artifact -> IO (Either String T.Text)
getArtifact a = g (artifact_contents a)
  where
  runT t = runTransformer (artifact_transformer a) t
  g (LiteralContents t) = return (runT t)
  g (FileContents getf) = do
      srcpath <- getf
      -- Check if srcpath exists. If it does not, give an error
      exists <- doesFileExist srcpath
      case exists of
        True -> do
          t <- T.readFile srcpath
          return (runT t)
        False -> return (Left (notfound srcpath))

  notfound srcpath = "Path " ++ srcpath ++ " for Artifact named "
     ++ artifact_outputname a ++ " could not be found."

withContents :: Artifact -> (T.Text -> IO ()) -> IO (Maybe String)
withContents a f = do
  contents <- getArtifact a
  case contents of
    Left err -> return (Just err)
    Right c  -> f c >> return Nothing

putArtifact :: FilePath -> Artifact -> IO (Maybe String)
putArtifact fp a = withContents a $ \c -> do
  createDirectoryIfMissing True fp
  let fname = fp </> artifact_outputname a
  b <- doesFileExist fname
  when b $ putStrLn ("*** Warning: overwriting " ++ fname)
  T.writeFile fname c

putArtifact_ :: FilePath -> Artifact -> IO ()
putArtifact_ fp a = void (putArtifact fp a)

printArtifact :: Artifact -> IO ()
printArtifact a = do
  res <- withContents a aux
  case res of
    Nothing -> return ()
    Just err -> putStrLn $
      "Encountered error when creating artifact " ++ artifact_outputname a
        ++ ":\n" ++ err
  where
  aux c = do
    putStrLn ("Artifact " ++ artifact_outputname a)
    putStrLn "================"
    T.putStrLn c
    putStrLn "================"

