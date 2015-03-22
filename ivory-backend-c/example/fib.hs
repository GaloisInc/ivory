{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


--------------------------------------------------------------------------------
--  Example Use
--------------------------------------------------------------------------------
-- From the 'ivory' top level directory:
--
-- make
-- cabal exec ic -- -o /tmp/fib_module_files ./ivory-backend-c/examples/fib.c
--
-- Notice 'cabal exec' allows 'ic' to find the shared Haskell libraries in
-- the proper sandbox so you don't have to statically link the binary or install
-- the libraries at a user or global level.


module Fib where
import Ivory.Language
import Ivory.Artifact

fib_loop :: Def ('[Ix 1000] :-> Uint32)
fib_loop  = proc "fib_loop" $ \ n -> body $ do
  a <- local (ival 0)
  b <- local (ival 1)

  n `times` \ _ -> do
    a' <- deref a
    b' <- deref b
    store a b'
    store b (a' + b')

  result <- deref a
  ret result

fib_tutorial_module :: Module
fib_tutorial_module = package "fib_tutorial" $ do
  incl fib_loop

main :: ([Module], [Artifact])
main = ([ fib_tutorial_module ], [])
