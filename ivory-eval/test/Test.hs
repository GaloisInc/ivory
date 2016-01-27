{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude ()
import Prelude.Compat

import Ivory.Language
import Ivory.Language.Monad

import Ivory.Eval

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ sumTest, assertTest ]


--------------------------------------------------------------------------------
-- test modules

sumTest :: TestTree
sumTest = testCase "sum" $ do
  let Right (_, EvalState st _ _)
        = runEvalStartingFrom (initState mempty)
        $ evalBlock $ blockStmts $ snd $ runIvory $ do
            r <- local (izero :: Init ('Stored Sint32))
            11 `times` \(i :: Ix 11) -> do -- sum [ 10 .. 0 ]
              v <- deref r
              store r (v + safeCast i)
  assertEqual "wrong state!"
    (Map.fromList [ ("local0", Sint32 55), ("deref3", Sint32 54)
                  , ("ix2", Sint32 0), ("ref1", Ref "local0")]) 
    st

assertTest :: TestTree
assertTest = testCase "assert" $ do
  let r = runEvalStartingFrom (initState mempty)
        $ evalBlock $ blockStmts $ snd $ runIvory $
            Ivory.Language.assert ((0 :: Sint32) <? 1)
  assertBool (show r) (case r of
                        Left _ -> False
                        Right _ -> True)
