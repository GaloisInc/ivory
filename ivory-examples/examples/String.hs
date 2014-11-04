{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module String where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

printf :: Def ('[IString] :-> Sint32)
printf  = importProc "printf" "stdio.h"

printf2 :: Def ('[IString,Sint32] :-> Sint32)
printf2  = importProc "printf" "stdio.h"

test :: Def ('[] :-> ())
test  = proc "test" $ body $ do
  call_ printf "Hello, world\n"
  call_ printf2 "howdy, %i \n" 3
  retVoid

runString :: IO ()
runString = runCompiler [cmodule] [] initialOpts { stdOut = True }

cmodule :: Module
cmodule = package "String" $ do
  incl printf
  incl test
