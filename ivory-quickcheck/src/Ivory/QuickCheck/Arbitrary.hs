{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for Ivory and helper functions.

module Ivory.QuickCheck.Arbitrary where

import qualified Test.QuickCheck.Arbitrary as A
import qualified Test.QuickCheck.Gen       as G

import           Data.Int
import           Data.Word
import           Data.String (IsString(fromString))
import           GHC.TypeLits

import           Ivory.Language

import           Ivory.QuickCheck.Monad

--------------------------------------------------------------------------------

instance A.Arbitrary IBool where
  arbitrary = fmap toIvory (A.arbitrary :: G.Gen Bool)
    where toIvory True  = true
          toIvory False = false

--------------------------------------------------------------------------------

instance A.Arbitrary IString where
  arbitrary = fmap fromString (A.arbitrary :: G.Gen String)

--------------------------------------------------------------------------------

integralArb :: (Integral a, Num b) => G.Gen a -> G.Gen b
integralArb = fmap fromIntegral

instance A.Arbitrary Uint8 where
  arbitrary = integralArb (A.arbitrary :: G.Gen Word8)

instance A.Arbitrary Uint16 where
  arbitrary = integralArb (A.arbitrary :: G.Gen Word16)

instance A.Arbitrary Uint32 where
  arbitrary = integralArb (A.arbitrary :: G.Gen Word32)

instance A.Arbitrary Uint64 where
  arbitrary = integralArb (A.arbitrary :: G.Gen Word64)

instance A.Arbitrary Sint8 where
  arbitrary = integralArb (A.arbitrary :: G.Gen Int8)

instance A.Arbitrary Sint16 where
  arbitrary = integralArb (A.arbitrary :: G.Gen Int16)

instance A.Arbitrary Sint32 where
  arbitrary = integralArb (A.arbitrary :: G.Gen Int32)

instance A.Arbitrary Sint64 where
  arbitrary = integralArb (A.arbitrary :: G.Gen Int64)

instance A.Arbitrary IFloat where
  arbitrary = fmap ifloat (A.arbitrary :: G.Gen Float)

instance A.Arbitrary IDouble where
  arbitrary = fmap idouble (A.arbitrary :: G.Gen Double)

--------------------------------------------------------------------------------

-- | Random array (of 'Stored' values) initializer.
instance (SingI len, A.Arbitrary a, IvoryType a, IvoryInit a)
  => A.Arbitrary (Init (Array len (Stored a)))
  where
  arbitrary = do
    let sz  = fromSing (sing :: Sing len)
    arr    <- G.vectorOf (fromInteger sz) A.arbitrary
    return  $ iarray (map ival arr)

--------------------------------------------------------------------------------

-- | Random struct label (of 'Stored' values) initializer.
sampleStoredLabel :: (A.Arbitrary a, IvoryInit a)
  => Label sym (Stored a) -> G.Gen (InitStruct sym)
sampleStoredLabel label = do
  v <- A.arbitrary
  return (label .= ival v)

--------------------------------------------------------------------------------

-- | Take a random number generator seed, a number of items to produce, and a
-- generator and produces an increasingly bounded list of items.
mkSamples :: Int -> G.Gen a -> IvoryGen [a]
mkSamples n (G.MkGen f) = mapM go ns
  where
  ns = [0,2..n*2]
  go i = do
    rnd <- get
    return (f rnd i)

--------------------------------------------------------------------------------
