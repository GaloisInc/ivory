{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module FibTutorial where

import Ivory.Language
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

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

main :: IO ()
main = C.compile [ fib_tutorial_module ] []
