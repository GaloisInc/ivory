{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Extern where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

x :: Uint8
x = extern "SOME_CONST" "some_other_header.h"

putchar :: Def ('[Uint8] :-> ())
putchar  = importProc "putchar" "some_header.h"

test :: Def ('[Uint8] :-> ())
test  = proc "test" $ \ c -> body $
     call_ putchar c
  >> call_ putchar x
  >> retVoid

runExtern :: IO ()
runExtern  = runCompiler [cmodule] [] initialOpts { outDir = Nothing, scErrors = True }

cmodule :: Module
cmodule  = package "Extern" $ do
  incl test
  incl putchar
  inclSym x
