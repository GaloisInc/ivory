{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Extern where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

x :: Uint8
x = extern "SOME_CONST"

putchar :: Def ('[Uint8] :-> ())
putchar  = externProc "putchar"

test :: Def ('[Uint8] :-> ())
test  = proc "test" $ \ c -> body $
     call_ putchar c
  >> call_ putchar x
  >> retVoid

runExtern :: IO ()
runExtern  = runCompiler [cmodule] [] initialOpts { stdOut = True }

cmodule :: Module
cmodule  = package "Extern" $ do
  incl test
  incl putchar
  inclHeader "some_header.h" -- for SOME_CONST
