{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Cond where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Compile.C.Modules


add :: Def ('[Uint32,Uint32] :-> Uint32)
add  = proc "add"
     $ \ x y -> ensures (\r -> r ==? x + y)
              $ body
              $ ret (x + y)

cmodule :: Module
cmodule = package "cond" $ incl add

-- Testing assertions with choice expression

foo :: Def ('[IFloat,IFloat,IFloat] :-> IFloat)
foo = proc "foo" $ \x y z -> body $ do
  let cond  = 2/x ==? 5
  let tCond = 6/y ==? 7
  let exp   = tCond ? (8/z, 9)
  ret (cond ? (exp,4))

fooMod :: Module
fooMod  = package "fooM" $ incl foo

runFoo :: IO ()
runFoo = runCompiler [fooMod] initialOpts { stdOut = True
                                          , divZero = True
                                          }
