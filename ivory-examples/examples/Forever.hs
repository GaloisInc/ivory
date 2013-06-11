{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Forever where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

factorial :: Def ('[Sint32] :-> Sint32)
factorial  = proc "factorial" $ \ n ->
  -- These are made up requires/ensures for testing purposes.
  ensures (\r -> n <? r) $ body $ do
  ifte_ (n >? 1)
    (do n' <- call factorial (n - 1)
        ret (n' * n)
    )
    (do ret n
    )

printResult :: Def ('[Sint32] :-> ())
printResult = proc "print_result" $ \_ -> body retVoid

foreverFactorial :: Def ('[Sint32] :-> ())
foreverFactorial = proc "forever_factorial" $ \ n -> body $ do
  forever $ do
    res <- call factorial n
    call_ printResult res

cmodule :: Module
cmodule = package "Forever" $ do
  incl factorial
  incl printResult
  incl foreverFactorial

runFactorial :: IO ()
runFactorial = runCompiler [cmodule] initialOpts { stdOut = True }

