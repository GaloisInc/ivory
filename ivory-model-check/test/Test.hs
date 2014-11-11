{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Ivory.Language hiding (Struct, assert, true, false, proc, (.&&))
import qualified Ivory.Language as L
import qualified Ivory.Language.Syntax as I
import Ivory.Serialize
import Ivory.ModelCheck

import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

import qualified Examples
import qualified Heartbeat
import qualified PPM

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ shouldPass, shouldFail, examples ]

shouldPass :: TestTree
shouldPass = testGroup "should be safe"
             [ mkSuccess "foo2" m2 []
             , mkSuccess "foo3" m3 []
             , mkSuccess "foo4" m4 []
             , mkSuccess "foo5" m5 []
             , mkSuccess "foo6" m6 []
             , mkSuccess "foo7" m7 []
             , mkSuccess "foo8" m8 []
             , mkSuccess "foo9" m9 []
             , mkSuccess "foo10" m10 []
             , mkSuccess "foo11" m11 []
             , mkSuccess "foo12" m12 []
             , mkSuccess "foo13" m13 []
             , mkSuccess "foo15" m15 []
             , mkSuccess "foo16" m16 []
             , mkSuccess "foo17" m17 []
             , mkSuccess "foo18" m18 []
             , mkSuccessInline "heartbeat" Heartbeat.heartbeatModule
                                           [serializeModule]
             , mkSuccess "ppm_decode" PPM.ppmModule [PPM.userInputTypeModule]
             ]

shouldFail :: TestTree
shouldFail = testGroup "should be unsafe"
             [ mkFailure "foo1" m1 []
             , mkFailure "foo14" m14 []
             ]

examples :: TestTree
examples = testGroup "examples (shouldn't crash)"
           [ mkNotError (I.modName m) m [] | m <- Examples.modules ]

testArgs :: Args
testArgs = initArgs { printQuery = False, printEnv = False }

mkSuccess :: TestName -> Module -> [Module] -> TestTree
mkSuccess nm m deps = testCase nm $ do
  r <- modelCheck testArgs deps m
  let msg = printf "Expected: Safe\nActual: %s"
            (showResult r)
  assertBool msg (isSafe r)

mkSuccessInline :: TestName -> Module -> [Module] -> TestTree
mkSuccessInline nm m deps = testCase nm $ do
  r <- modelCheck (testArgs { inlineCall = True }) deps m
  let msg = printf "Expected: Safe\nActual: %s"
            (showResult r)
  assertBool msg (isSafe r)

mkFailure :: TestName -> Module -> [Module] -> TestTree
mkFailure nm m deps = testCase nm $ do
  r <- modelCheck testArgs deps m
  let msg = printf "Expected: Unsafe\nActual: %s"
            (showResult r)
  assertBool msg (isUnsafe r)

mkNotError :: TestName -> Module -> [Module] -> TestTree
mkNotError nm m deps = testCase nm $ do
  r <- modelCheck testArgs deps m
  let msg = printf "Expected: anything but Error\nActual: %s"
            (showResult r)
  assertBool msg (not $ isError r)

--------------------------------------------------------------------------------
-- test modules

foo1 :: Def ('[Uint8, Uint8] :-> ())
foo1 = L.proc "foo1" $ \y x -> body $ do
  ifte_ (y <? 3)
    (do ifte_ (y ==? 3)
              (L.assert $ y ==? 0)
              retVoid)
    (do z <- assign x
        -- this *should* fail
        L.assert (z >=? 3))
  retVoid

m1 :: Module
m1 = package "foo1" (incl foo1)

-----------------------

foo2 :: Def ('[] :-> ())
foo2 = L.proc "foo2" $ body $ do
  x <- local (ival (0 :: Uint8))
  store x 3
  y <- assign x
  z <- deref y
  L.assert (z ==? 3)
  retVoid

m2 :: Module
m2 = package "foo2" (incl foo2)

-----------------------

foo3 :: Def ('[] :-> ())
foo3 = L.proc "foo3" $ body $ do
  x <- local (ival (1 :: Sint32))
  -- since ivory loops are bounded, we can just unroll the whole thing!
  for (toIx (2 :: Sint32) :: Ix 4) $ \ix -> do
    store x (fromIx ix)
    y <- deref x
    L.assert ((y <? 4) L..&& (y >=? 0))

m3 :: Module
m3 = package "foo3" (incl foo3)

-----------------------

foo4 :: Def ('[] :-> ())
foo4 = L.proc "foo4" $ body $ do
  x <- local (ival (1 :: Sint32))
  -- store x (7 .% 2)
  -- store x (4 .% 3)
  store x 1
  y <- deref x
  -- L.assert (y <? 2)
  L.assert (y ==? 1)

m4 :: Module
m4 = package "foo4" (incl foo4)

-----------------------

foo5 :: Def ('[] :-> ())
foo5 = L.proc "foo5" $ body $ do
  x <- local (ival (1 :: Sint32))
  -- for loops from 0 to n-1, inclusive
  for (toIx (9 :: Sint32) :: Ix 10) $ \ix -> do
    store x (fromIx ix)
    y <- deref x
    L.assert (y <=? 10)
  y <- deref x
  L.assert ((y ==? 8))

m5 :: Module
m5 = package "foo5" (incl foo5)

-----------------------

foo6 :: Def ('[Uint8] :-> ())
foo6 = L.proc "foo1" $ \x -> body $ do
  y <- local (ival (0 :: Uint8))
  ifte_ (x <? 3)
        (do a <- local (ival (9 :: Uint8))
            b <- deref a
            store y b
        )
        (do a <- local (ival (7 :: Uint8))
            b <- deref a
            store y b
        )
  z <- deref y
  L.assert (z <=? 9)
  L.assert (z >=? 7)

m6 :: Module
m6 = package "foo6" (incl foo6)

-----------------------

foo7 :: Def ('[Uint8, Uint8] :-> Uint8)
foo7 = L.proc "foo7" $ \x y ->
       requires (x + y <=? 255)
     $ body $ do
         ret (x + y)

m7 :: Module
m7 = package "foo7" (incl foo7)

-----------------------

foo8 :: Def ('[Uint8] :-> Uint8)
foo8 = L.proc "foo8" $ \x -> body $ do
  let y = x .% 3
  L.assert (y <? 4)
  ret y

m8 :: Module
m8 = package "foo8" (incl foo8)

-----------------------

[ivory|
struct foo2
{ aFoo :: Stored Uint8
; bFoo :: (Array 4 Uint8)
}
|]

foo9 :: Def ('[Ref s (L.Struct "foo2")] :-> ())
foo9 = L.proc "foo9" $ \f -> body $ do
  st <- local (istruct [aFoo .= ival 0])
  a  <- deref (st ~> aFoo)
  L.assert (a ==? 0)
  store (f ~> aFoo) 3
  store (f ~> bFoo ! 0) 1
  store (f ~> aFoo) 4
  x <- deref (f ~> aFoo)
  y <- deref (f ~> bFoo ! 0)
  L.assert (x ==? 4 L..&& y ==? 1)

m9 :: Module
m9 = package "foo9" $ do
  defStruct (Proxy :: Proxy "foo2")
  incl foo9

-----------------------

foo10 :: Def ('[Uint8] :-> Uint8)
foo10 = L.proc "foo10" $ \x ->
        requires (x <? 10)
      $ ensures (\r -> r ==? x + 1)
      $ body $ do
        r <- assign $ x + 1
        ret r

m10 :: Module
m10 = package "foo10" (incl foo10)
    
-----------------------

foo11 :: Def ('[Ix 10] :-> ())
foo11 = L.proc "foo11" $ \n -> body $ do
          x <- local (ival (0 :: Sint8))
          for n $ \i -> do
            x' <- deref x
            store x $ x' + safeCast i

m11 :: Module
m11 = package "foo11" (incl foo11)

-----------------------

foo12 :: Def ('[Uint8] :-> Uint8)
foo12 = L.proc "foo12" $ \n -> 
        ensures (\r -> r ==? n)
      $ body $ do
          ifte_ (n ==? 0)
            (ret n)
            (do n' <- L.call foo12 (n-1)
                ret (n' + 1))

m12 :: Module
m12 = package "foo12" (incl foo12)

-----------------------

foo13 :: Def ('[Uint8, Uint8] :-> Uint8)
foo13 = L.proc "foo13" $ \x y -> 
        requires (x <=? 15)
      $ requires (y <=? 15)
      $ body $ ret (x * y)

m13 :: Module
m13 = package "foo13" (incl foo13)

-----------------------

foo14 :: Def ('[Uint8, Uint8] :-> Uint8)
foo14 = L.proc "foo14" $ \x y -> 
        body $ ret (x * y)

m14 :: Module
m14 = package "foo14" (incl foo14)

-----------------------

foo15 :: Def ('[Ix 10] :-> Uint8)
foo15 = L.proc "foo15" $ \n -> 
  ensures (\r -> r <=? 5) $
  body $ do
    n `times` \i -> do
      ifte_ (i >? 5) (ret 5) (ret $ safeCast i)

m15 :: Module
m15 = package "foo15" (incl foo15)

-----------------------

foo16 :: Def ('[]:->())
foo16 = L.proc "foo16" $ body $ do
  (stack_array :: Ref (Stack s) (Array 10 (Stored IFloat))) <- local (iarray [])
  store (stack_array ! 0) 5
  arrayMap $ \ix ->
    store (stack_array ! ix) 1

m16 :: Module
m16 = package "foo16" (incl foo16)

-----------------------

foo17 :: Def ('[ Ref Global (Array 10 (Stored Uint32))] :-> ())
foo17 = L.proc "foo17" $ \a -> body $ do
  b <- local (iarray [ival 0, ival 1])
  refCopy b a
  arrayMap (\ix -> store (a ! (ix :: Ix 10)) 1)
  retVoid

m17 :: Module
m17 = package "foo17" (incl foo17)

-----------------------

foo18 :: Def ('[Ref s (L.Struct "foo2")] :-> Ref s (L.Struct "foo2"))
foo18 = L.proc "foo18" $ \f -> 
    requires (checkStored (f ~> aFoo) (\a -> a >? 0))
  $ requires (checkStored (f ~> aFoo) (\a -> a <? 10))
  $ ensures (\r -> checkStored (r ~> aFoo) (\a -> a >? 1))
  $ body $ do
    a <- deref (f ~> aFoo)
    L.assert (a >? 0)

    store (f ~> aFoo) (a + 1)
    ret f

m18 :: Module
m18 = package "foo18" $ do
  defStruct (Proxy :: Proxy "foo2")
  incl foo18
