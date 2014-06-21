{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Overflow where

import Control.Monad (void)
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

ovf1 :: Def ('[Sint8] :-> Sint8)
ovf1  = proc "ovf1" $ \ n -> body $
  ifte_ (n <? maxBound - 20)
       (ret (n + 15))
       (ret (n .% 2))

ovf2 :: Def ('[Sint8] :-> Sint8)
ovf2  = proc "ovf2" $ \ n -> requires (n <? 1)
                           $ body
                           $ ret (n + 15)

ovf3 :: Def ('[IFloat, IFloat, IBool] :-> IFloat)
ovf3  = proc "ovf3" $ \ n m o -> body $ do
--  x <- assign (n / m / o)
  ret $ (o ? (n / m, m / n))

cmodule :: Module
cmodule = package "Overflow" $ --incl ovf1 >> incl ovf2 >> 
    incl ovf3

writeOverflow :: Opts -> IO ()
writeOverflow opts = void $ runCompiler [cmodule]
  opts { constFold = False, overflow = False, divZero = True, stdOut = True }

