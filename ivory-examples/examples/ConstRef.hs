{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module ConstRef where

import Control.Monad (void)
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

import Control.Monad ((<=<))


test :: Def ('[ConstRef (Stored Uint8)] :-> Uint8)
test  = proc "test" (ret <=< deref)

runConstRef :: IO ()
runConstRef =
  void $ runCompiler
            [package "ConstRef" (incl test)]
            initialOpts { stdOut = True }
