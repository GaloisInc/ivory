{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Generate random inputs to Ivory-generated C code.
--
-- Example usage:
-- @
-- [ivory|
-- struct foo
--   { foo_a :: Stored IFloat
--   ; foo_b :: Stored Uint8
--   }
-- |]
--
-- -- Function we want to generate inputs for.
-- func :: Def ('[Uint8
--               , Ref s (Array 3 (Stored Uint8))
--               , Ref s (Struct "foo")
--               ] :-> ())
-- func = proc "func" $ \u arr str -> body $
--   arrayMap $ \ix -> do
--     a <- deref (arr ! ix)
--     b <- deref (str ~> foo_b)
--     store (arr ! ix) (a + b + u)
--
-- type DriverDef = Def ('[] :-> ())
--
-- -- Driver function.  Takes lists of the arguments we'll pass to the function
-- -- test.
-- driver :: [Uint8]
--        -> [Init (Array 3 (Stored Uint8))]
--        -> [Init (Struct "foo")]
--        -> DriverDef
-- driver as0 as1 as2 = proc "main" $ body $ do
--   mapM_ oneCall (zip3 as0 as1 as2)
--
--   where
--   oneCall (a0, a1, a2) = do
--     a1' <- local a1
--     a2' <- local a2
--     call_ func a0 a1' a2'
--
-- -- Generate the random values to pass.
-- runTest :: IvoryGen DriverDef
-- runTest = do
--   args0 <- samples num A.arbitrary
--   args1 <- samples num A.arbitrary
--   aFoos <- samples num foo_a
--   bFoos <- samples num foo_b
--   return $ driver args0 args1 (zipWith foo aFoos bFoos)
--   where
--   foo a b = istruct [ a, b ]
--   num = 10
--
-- -- Compile!
-- runTests :: IO ()
-- runTests = do
--   d <- runIO runTest
--   runCompiler [cmodule d] initialOpts { includeDir = "test"
--                                       , srcDir     = "test"
--                                       , constFold  = True
--                                       }
--   where
--   cmodule d = package "qc" $ do
--     incl d
--     incl func
-- @

module Ivory.QuickCheck
  ( module Ivory.QuickCheck.Monad
  , module Ivory.QuickCheck.Arbitrary
  , Samples(..)
  ) where



import qualified Test.QuickCheck.Arbitrary as A
import qualified Test.QuickCheck.Gen       as G

import           Ivory.QuickCheck.Arbitrary
import           Ivory.QuickCheck.Monad

import           Ivory.Language

import           GHC.TypeLits

--------------------------------------------------------------------------------

type Size = Int

class Samples gen res where
  samples :: Size -> gen -> IvoryGen [res]

instance A.Arbitrary a => Samples (G.Gen a) a where
  samples = mkSamples

instance (A.Arbitrary a, SingI len, IvoryInit a, IvoryType a)
      => Samples (G.Gen a) (Init (Array len (Stored a))) where
  samples i gen = mkSamples i mkArr
    where
    mkArr = do
      let sz = fromSing (sing :: Sing len)
      arr   <- G.vectorOf (fromInteger sz) gen
      return $ iarray (map ival arr)

instance (A.Arbitrary a, IvoryInit a)
      => Samples (Label sym (Stored a)) (InitStruct sym)
  where
  samples i label = mkSamples i (sampleStoredLabel label)

--------------------------------------------------------------------------------

