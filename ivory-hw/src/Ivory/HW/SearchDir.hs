
module Ivory.HW.SearchDir where

import System.FilePath

import qualified Paths_ivory_hw

searchDir :: IO FilePath
searchDir = do
  base <- Paths_ivory_hw.getDataDir
  return $ base </> "support"


