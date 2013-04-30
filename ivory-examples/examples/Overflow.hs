{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Overflow where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

ovf1 :: Def ('[Sint8] :-> Sint8)
ovf1  = proc "ovf1" $ \ n -> body $
  ifte (n <? maxBound - 20)
       (ret (n + 15))
       (ret (n .% 2))

ovf2 :: Def ('[Sint8] :-> Sint8)
ovf2  = proc "ovf2" $ \ n -> requires [check $ n <? 1]
                           $ body
                           $ ret (n + 15)

ovf3 :: Def ('[IFloat, IFloat, IFloat] :-> IBool)
ovf3  = proc "ovf3" $ \ n m o -> body $ do
  x <- assign (n / m / o / 0)
  ret $ x >? (n / m)

cmodule :: Module
cmodule = package "Overflow" $ incl ovf1 >> incl ovf2 >> incl ovf3

writeOverflow :: Opts -> IO ()
writeOverflow opts = runCompiler [cmodule]
  opts { constFold = True, overflow = True, divZero = True }
