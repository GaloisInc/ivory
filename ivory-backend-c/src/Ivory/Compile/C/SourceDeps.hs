
module Ivory.Compile.C.SourceDeps where

import Control.Monad
import System.FilePath
import System.Directory

import Data.Either
import qualified Data.Set as Set

import Ivory.Language
import qualified Ivory.Language.Syntax.AST as AST

-- At some point we may want to restrict this to .c and .h files only
-- but for now we'll leave it alone.
collectSourceDeps :: [Module] -> [FilePath]
collectSourceDeps ms = Set.toList $ foldl aux Set.empty ms
  where aux acc m = Set.union acc (AST.modSourceDeps m)

outputSourceDeps :: FilePath -> FilePath -- Output dirs: include dir, source dir
                 -> [FilePath]           -- SourceDeps: list if filenames to output
                 -> [FilePath]           -- Search Path: list of directories
                 -> IO ()                --  where SourceDeps may be
outputSourceDeps inclDir srcDir srcDeps searchPath = do
    validSearchPath <- filterM doesDirectoryExist searchPath
    discovered <- mapM (findSource validSearchPath) srcDeps
    mapM_ output (rights discovered)
    case lefts discovered of
      [] -> return ()
      es -> error $ "failure to output source dependencies:\n" ++ (unlines es) ++
                    "in search path:\n" ++ (unlines searchPath)
  where
  findSource :: [FilePath] -> FilePath -> IO (Either String FilePath)
  findSource searchpath f = do
    v <- filterM doesFileExist $ map (\p -> p </> f) searchpath
    case v of
      []   -> return $ Left ("not found: " ++ f)
      a:_ -> return $ Right a
  output :: FilePath -> IO ()
  output fp = case takeExtension fp of
    ".h" -> copyFile fp (inclDir </> (takeFileName fp))
    _    -> copyFile fp (srcDir  </> (takeFileName fp))



