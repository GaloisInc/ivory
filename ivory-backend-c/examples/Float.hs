{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Float (runFloat,cmodule) where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

runFloat :: IO ()
runFloat = runCompiler [cmodule] initialOpts { stdOut = True }

cmodule :: Module
cmodule  = package "Float" $ do
  incl test1
  incl test2

test1 :: Def ('[IFloat] :-> Sint32)
test1  = proc "test1" (\ f -> body (ret (castDefault f)))

test2 :: Def ('[Sint32] :-> IFloat)
test2  = proc "test2" (\ i -> body (ret (safeCast i)))
