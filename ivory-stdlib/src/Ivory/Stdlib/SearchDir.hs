
module Ivory.Stdlib.SearchDir where

import System.FilePath

import qualified Paths_ivory_stdlib

searchDir :: IO FilePath
searchDir = do
  base <- Paths_ivory_stdlib.getDataDir
  return $ base </> "support"


