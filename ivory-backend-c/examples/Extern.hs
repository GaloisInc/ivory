{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Extern where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

putchar :: Def ('[Uint8] :-> ())
putchar  = externProc "putchar"

test :: Def ('[Uint8] :-> ())
test  = proc "test" $ \ c -> call_ putchar c >> retVoid

runExtern :: IO ()
runExtern  = runCompiler [cmodule] initialOpts { stdOut = True }

cmodule :: Module
cmodule  = package "Extern" $ do
  incl test
  incl putchar
