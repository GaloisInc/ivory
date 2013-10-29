{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module QC where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language
import Ivory.QuickCheck
import Test.QuickCheck.Arbitrary

[ivory|
struct foo
  { foo_a :: Stored IFloat
  ; foo_b :: Stored Uint8
  }
|]

-- Function we want to generate inputs for.
func :: Def ('[Uint8
              , Ref s (Array 3 (Stored Uint8))
              , Ref s (Struct "foo")
              ] :-> ())
func = proc "func" $ \u arr str -> body $
  arrayMap $ \ix -> do
    a <- deref (arr ! ix)
    b <- deref (str ~> foo_b)
    store (arr ! ix) (a + b + u)

type DriverDef = Def ('[] :-> ())

-- Driver function.  Takes lists of the arguments we'll pass to the function
-- test.
driver :: [Uint8]
       -> [Init (Array 3 (Stored Uint8))]
       -> [Init (Struct "foo")]
       -> DriverDef
driver as0 as1 as2 = proc "main" $ body $ do
  mapM_ oneCall (zip3 as0 as1 as2)

  where
  oneCall (a0, a1, a2) = do
    a1' <- local a1
    a2' <- local a2
    call_ func a0 a1' a2'

-- Generate the random values to pass.
runTest :: IvoryGen DriverDef
runTest = do
  args0 <- samples num arbitrary
  args1 <- samples num arbitrary
  aFoos <- samples num foo_a
  bFoos <- samples num foo_b
  return $ driver args0 args1 (zipWith foo aFoos bFoos)
  where
  foo a b = istruct [ a, b ]
  num = 10

-- Compile!
runTests :: IO ()
runTests = do
  d <- runIO runTest
  runCompiler [cmodule d] initialOpts { includeDir = "test"
                                      , srcDir     = "test"
                                      , constFold  = True
                                      }
  where
  cmodule d = package "qc" $ do
    incl d
    incl func
