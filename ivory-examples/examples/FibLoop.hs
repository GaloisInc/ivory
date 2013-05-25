{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module FibLoop where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

import Control.Applicative ((<$>),(<*>))

-- Recursive implementation of fib
fib_rec :: Def ('[Uint32] :-> Uint64)
fib_rec  = proc "fib_rec" (\n -> body (ret =<< call fib_rec_aux 0 1 n))

fib_rec_aux :: Def ('[Uint32,Uint32,Uint32] :-> Uint64)
fib_rec_aux  = proc "fib_rec_aux" $ \ a b n -> body $ do
  ifte (n ==? 0)
    (ret (safeCast a))
    (ret . safeCast =<< call fib_rec_aux b (a + b) (n - 1))

-- Loop implementation of fib.

fib_loop :: Def ('[Ix 1000] :-> Uint32)
fib_loop  = proc "fib_loop" $ \ n -> body $ do
  a <- local (ival 0)
  b <- local (ival 0)

  n `times` \ _ -> do
    a' <- deref a
    b' <- deref b
    store a b'
    store b (a' + b')

  result <- deref a
  ret result

-- Loop implementation of fib, using a structure instead
-- of two discrete variables.
[ivory|
struct Fibstate
  { sa :: Stored Uint32
  ; sb :: Stored Uint32
  }
|]

fib_struct_loop :: Def ('[Ix 1000] :-> Uint32)
fib_struct_loop  = proc "fib_struct_loop" $ \ n -> body $ do
  state <- local (istruct [ sa .= ival 0 , sb .= ival 0 ])

  let update = (+) <$> deref (state ~> sa) <*> deref (state ~> sb)

  n `times` \ _ -> do
    store (state ~> sa) =<< deref (state ~> sb)
    store (state ~> sb) =<< update

  ret =<< deref (state ~> sa)

cmodule :: Module
cmodule = package "FibLoop" $ do
  incl fib_rec
  incl fib_rec_aux
  incl fib_loop

  defStruct (Proxy :: Proxy "Fibstate")
  incl fib_struct_loop

runFibLoop :: IO ()
runFibLoop  = runCompiler [cmodule] initialOpts { stdOut = True, constFold = True }
