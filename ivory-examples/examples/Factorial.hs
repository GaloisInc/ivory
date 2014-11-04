{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Factorial where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

factorial :: Def ('[Sint32] :-> Sint32)
factorial  = proc "factorial" $ \ n ->
  -- These are made up requires/ensures for testing purposes.
  ensures (\r -> n <? r) $
  body $
    ifte_ (n >? 1)
      (do n' <- call factorial (n - 1)
          ret (n' * n)
      )
      (do ret n
      )

cmodule :: Module
cmodule = package "Factorial" $ incl factorial

runFactorial :: IO ()
runFactorial = runCompiler [cmodule] [] initialOpts { stdOut = True }
