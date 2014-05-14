{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Array where

import Control.Monad (void)
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

cmodule :: Module
cmodule = package "Array" $ incl arrayExample

runArrayExample :: IO ()
runArrayExample = void $ runCompiler [cmodule] initialOpts { stdOut = True }

arrayExample :: Def('[Ref s (Array 4 (Stored Uint8)), Uint8] :-> ())
arrayExample = proc "arrayExample" $ \arr n -> body $ do
  arrayMap $ \ ix -> do
    v <- deref (arr ! ix)
    store (arr ! ix) (v + n)
