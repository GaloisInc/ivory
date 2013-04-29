{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module FibLoop where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

import Control.Applicative ((<$>),(<*>))


fib :: Def ('[Uint32] :-> Uint64)
fib  = proc "fib" (\n -> body (ret =<< call fib_aux 0 1 n))

fib_aux :: Def ('[Uint32,Uint32,Uint32] :-> Uint64)
fib_aux  = proc "fib_aux" $ \ a b n -> body $ do
  ifte (n ==? 0)
    (ret (safeCast a))
    (ret . safeCast =<< call fib_aux b (a + b) (n - 1))

fib_loop :: Def ('[Ix 1000] :-> Uint32)
fib_loop  = proc "fib_loop" $ \ n -> body $ do
  a <- local (ival 0)
  b <- local (ival 0)

  let update = (+) <$> deref a <*> deref b

  n `times` \ _ -> do
    store a =<< deref b
    store b =<< update

  ret =<< deref a

cmodule :: Module
cmodule = package "FibLoop" $ do
  incl fib
  incl fib_aux
  incl fib_loop

runFibLoop :: IO ()
runFibLoop  = runCompiler [cmodule] initialOpts { stdOut = True, constFold = True }
