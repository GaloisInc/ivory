{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}

module Main where

import qualified Ivory.Language        as L
import           Ivory.Language        hiding (Struct, assert, true, false, proc)
import qualified Ivory.Language.Proc   as I
import qualified Ivory.Language.Syntax as I
import qualified Ivory.Stdlib          as L
import           Ivory.Compile.C.CmdlineFrontend

import           Control.Monad (when)
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Process

import           Ivory.QuickCheck (checkWith,contract)

import           Text.Printf

import           Test.Tasty (TestTree,defaultMain,testGroup)
import           Test.Tasty.HUnit (assertBool,testCase)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ shouldPass, shouldFail ]

shouldPass :: TestTree
shouldPass = testGroup "should be safe"
             [ mkSuccess foo6 m6
             , mkSuccess foo7 m7
             , mkSuccess foo8 m8
             , mkSuccess foo9 m9
             , mkSuccess foo10 m10
             , mkSuccess foo11 m11
             , mkSuccess foo12 m12
             , mkSuccess foo13 m13
             , mkSuccess foo15 m15
             , mkSuccess foo17 m17
             , mkSuccess foo18 m18
             , mkSuccess foo19 m19
             , mkSuccess test_max mmax
             ]

shouldFail :: TestTree
shouldFail = testGroup "should be unsafe"
             [ mkFailure foo1 m1
             , mkFailure foo14 m14
             ]

mkSuccess :: Def (args ':-> res) -> Module -> TestTree
mkSuccess d@(~(I.DefProc p)) m = testCase (I.procSym p) $ do
  (o, r) <- quickCheck d m
  assertBool ("Expected quickcheck to pass: output:\n" ++ o) r

mkFailure :: Def (args ':-> res) -> Module -> TestTree
mkFailure d@(~(I.DefProc p)) m = testCase (I.procSym p) $ do
  (o, r) <- quickCheck d m
  assertBool ("Expected quickcheck to fail: output:\n" ++ o) (not r)


quickCheck :: Def (args ':-> res) -> Module -> IO (String, Bool)
quickCheck d@(~(I.DefProc p)) m = do
  tmpDir <- getTemporaryDirectory
  let testDir = tmpDir </> "ivory-quickcheck" </> I.procSym p
  b <- doesDirectoryExist testDir
  when b $ removeDirectoryRecursive testDir
  checkWith 100 (initialOpts { outDir = Just testDir }) [] m (contract d)
  let cmd = printf "(cd %s && cc -std=c99 -DIVORY_TEST *.c && ./a.out)" testDir
  (_,oh,eh,pid) <- runInteractiveCommand cmd
  code <- waitForProcess pid
  out <- hGetContents oh
  err <- hGetContents eh
  return (out ++ "\n" ++ err, code == ExitSuccess)

--------------------------------------------------------------------------------
-- test modules

foo1 :: Def ('[Ix 10, Ix 10] ':-> ())
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

foo6 :: Def ('[Uint8] ':-> ())
foo6 = L.proc "foo6" $ \x -> body $ do
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

foo7 :: Def ('[Uint8, Uint8] ':-> Uint8)
foo7 = L.proc "foo7" $ \x y ->
       requires (x + y <=? 255)
     $ body $ do
         ret (x + y)

m7 :: Module
m7 = package "foo7" (incl foo7)

-----------------------

foo8 :: Def ('[Uint8] ':-> Uint8)
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

foo9 :: Def ('[Ref s ('L.Struct "foo2")] ':-> ())
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

foo10 :: Def ('[Uint8] ':-> Uint8)
foo10 = L.proc "foo10" $ \x ->
        requires (x <? 10)
      $ ensures (\r -> r ==? x + 1)
      $ body $ do
        r <- assign $ x + 1
        ret r

m10 :: Module
m10 = package "foo10" (incl foo10)
    
-----------------------

foo11 :: Def ('[Ix 10] ':-> ())
foo11 = L.proc "foo11" $ \n -> body $ do
          x <- local (ival (0 :: Sint8))
          for n $ \i -> do
            x' <- deref x
            store x $ x' + safeCast i

m11 :: Module
m11 = package "foo11" (incl foo11)

-----------------------

foo12 :: Def ('[Uint8] ':-> Uint8)
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

foo13 :: Def ('[Uint8, Uint8] ':-> Uint8)
foo13 = L.proc "foo13" $ \x y -> 
        requires (x <=? 15)
      $ requires (y <=? 15)
      $ body $ ret (x * y)

m13 :: Module
m13 = package "foo13" (incl foo13)

-----------------------

foo14 :: Def ('[Uint8, Uint8] ':-> Uint8)
foo14 = L.proc "foo14" $ \x y ->
        ensures (\r -> r >=? x L..&& r >=? y) $
        body $ ret (x * y)

m14 :: Module
m14 = package "foo14" (incl foo14)

-----------------------

foo15 :: Def ('[Ix 10] ':-> Uint8)
foo15 = L.proc "foo15" $ \n -> 
  ensures (\r -> r <=? 5) $
  body $ do
    n `times` \i -> do
      ifte_ (i >? 5) (ret 5) (ret $ safeCast i)
    ret 4 -- FIXME: Ivory.Opts.TypeCheck doesn't see above `ret`s

m15 :: Module
m15 = package "foo15" (incl foo15)

-----------------------

foo17 :: Def ('[ Ref 'Global ('Array 10 ('Stored Uint32))] ':-> ())
foo17 = L.proc "foo17" $ \a -> body $ do
  b <- local (iarray [ival 0, ival 1])
  refCopy b a
  arrayMap (\ix -> store (a ! (ix :: Ix 10)) 1)
  retVoid

m17 :: Module
m17 = package "foo17" (incl foo17)

-----------------------

foo18 :: Def ('[Ref s ('L.Struct "foo2")] ':-> Ref s ('L.Struct "foo2"))
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

-----------------------

ppm_valid_area :: MemArea ('Stored IBool)
ppm_valid_area = area "ppm_valid" Nothing

foo19 :: Def('[Ref s ('Array 1 ('Stored Uint32))] ':-> ())
foo19 = L.proc "foo19" $ \ppms -> body $ do
  all_good <- local (ival L.true)
  ppm_last <- local (iarray [])
  let ppm_valid = addrOf ppm_valid_area
  arrayMap $ \ix -> do
    x <- deref (ppms ! ix)
    L.unless (x >=? 800 L..&& x <=? 2000)
      (store all_good L.false)

  b <- deref all_good
  L.unless b (store ppm_valid L.false)
  L.when b $ do
    (arrayMap $ \ix -> (deref (ppms ! ix) >>= store (ppm_last ! ix)))
    store ppm_valid L.true
  
  valid <- deref ppm_valid
  ppm <- deref (ppm_last ! 0)
  L.assert (L.iNot valid .|| (ppm >=? 800 L..&& ppm <=? 2000))
  retVoid

m19 :: Module
m19 = package "foo19" $ do
  defMemArea ppm_valid_area
  incl foo19

-----------------------

[ivory|
int32_t test_max(int32_t a, int32_t b) {
  return (a > b) ? a : b;
}
{
  post(return >= a && return >= b);
  post(return == a || return == b);
}
|]

mmax :: Module
mmax = package "max" (incl test_max)
