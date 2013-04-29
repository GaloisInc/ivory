module Ivory.Interp (
    Eval()
  , io
  , withEnv
  , evalContext
  , eval
  , scanlEval, FromValue()
  , quickcheck
  , Value(..)
  , loadModule
  ) where

import Ivory.Interp.Eval
import Ivory.Interp.Module
import Ivory.Interp.Monad
import Ivory.Interp.QuickCheck
import Ivory.Interp.Value

withEnv :: Eval a -> IO a
withEnv m = do
  run <- evalContext
  run m
