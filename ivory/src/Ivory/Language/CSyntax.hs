-- XXX testing
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

--
-- C-like syntax for Ivory.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax where

import Ivory.Language.CSyntax.QQ

-- XXX testing
import Ivory.Language

foo :: Def ('[] :-> Sint32)
foo = proc "foo" $ body [c|
  alloc *x = 3;
  *x = 4;
  return *x + 4;

|]

bar :: Def ('[] :-> Sint32)
bar = proc "bar" $ body [c|
  if (true) {
    alloc a = 5;
    return a;
  -- goo
  }
  else {
    alloc b = 3;
    return b + 3;
  }
|]

bar2 :: Def ('[] :-> ())
bar2 = proc "bar" $ body [c|
  if (true) {
    return;
  }
  else {
    return ;
  }
|]

e = (4::Sint32) >? 3

bar3 :: Def ('[] :-> IBool)
bar3 = proc "bar" $ body [c|
  return :i e;
|]


-- bar6 :: Def ('[Ref s (Array 3 (Stored Uint32))] :-> Uint32)
-- bar6 = proc "bar" $ \arr -> body [c|
--   alloc arr[30] = {0};
--   return arr[1];
-- |]

bar7 :: Def ('[Ref s (Array 3 (Stored Uint32))] :-> Uint32)
bar7 = proc "bar" $ \arr -> body $ do
  foo <- local (iarray (map ival [1,2,3]))
  arrayMap $ \ix -> do
    x <- deref (foo ! ix)
    store (arr ! ix) x
  y <- deref (arr ! 1)
  ret y

-- bar8 :: Def ('[Ref s (Array 3 (Stored Uint32))] :-> Uint32)
-- bar8 = proc "bar" $ \arr -> body [c|
-- --  arr[30] = {0}
--   alloc foo[] = {1,2, 4};
--   return arr [1] ;

-- |]

