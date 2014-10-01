
module Ivory.Serialize.SearchDir where

import System.FilePath

import qualified Paths_ivory_serialize

searchDir :: IO FilePath
searchDir = do
  base <- Paths_ivory_serialize.getDataDir
  return $ base </> "support"

