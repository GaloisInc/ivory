{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module FunPtr where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

f :: Def ('[Sint32] :-> Sint32)
f  = proc "f" (\ n -> body (ret (n + 1)))

invoke :: Def ('[ ProcPtr ('[Sint32] :-> Sint32), Sint32] :-> Sint32)
invoke  = proc "invoke" (\ k n -> body (ret =<< indirect k n))

test :: Def ('[] :-> Sint32)
test  = proc "test" (body (ret =<< call invoke (procPtr f) 10))

runFunPtr :: IO ()
runFunPtr = runCompiler [cmodule] [] initialOpts { outDir = Nothing }

cmodule :: Module
cmodule = package "FunPtr" $ do
  incl f
  incl test
  incl invoke
