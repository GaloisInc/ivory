{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Array where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

cmodule :: Module
cmodule = package "Array" $ do
  incl arrayExample
  incl arrayTernary

runArrayExample :: IO ()
runArrayExample = runCompiler [cmodule] [] initialOpts { outDir = Nothing }

arrayExample :: Def('[Ref s (Array 4 (Stored Uint8)), Uint8] :-> ())
arrayExample = proc "arrayExample" $ \arr n -> body $ do
  arrayMap $ \ ix -> do
    v <- deref (arr ! ix)
    store (arr ! ix) (v + n)


arrayTernary :: Def('[IBool] :-> IFloat)
arrayTernary = proc "arrayTernary" $ \b -> body $ do
  a1 <- local (vs 1)
  a2 <- local (vs 2)
  ares <- assign (b ? (a1, a2))
  deref (ares ! 3) >>= ret
  where
  vs ::  IFloat -> Init (Array 4 (Stored IFloat))
  vs v = iarray (map ival [v, v, v, v])
