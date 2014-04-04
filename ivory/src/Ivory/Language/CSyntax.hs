-- XXX testing
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

[c|
int32_t foo0() {
  alloc *x = 3;
  *x = 4;
  return *x + 4;
}

int32_t foo1() {
  if (true) {
    let a = 5;
    return a;
  -- goo
  }
  else {
    let b = 3;
    return b + 3;
  }
}


void foo2() {
  if (true) {
    return;
  }
  else {
    return ;
  }
}
|]

-- e :: IBool
-- e = (4::Sint32) >? 3

-- foo3 :: Def ('[] :-> IBool)
-- foo3 = proc "foo" $ body [c|
--   return :i e;
-- |]

-- foo4 :: Def ('[Ref s (Array 3 (Stored Uint32))] :-> Uint32)
-- foo4 = proc "foo" $ \arr0 -> body $ do
--   arr1 <- local (iarray (map ival [1,2,3]))
--   arrayMap $ \ix -> do
--     x <- deref (arr1 ! ix)
--     store (arr0 ! ix) x
--   y <- deref (arr0 ! 1)
--   ret y

-- foo6 :: Def ('[Ref s (Array 3 (Stored Uint32))] :-> Uint32)
-- foo6 = proc "foo" $ \arr0 -> body [c|
--   alloc arr1[] = {1,2,3};
--   map ix {
--     arr0[ix] = arr1[ix];
--   }
--   return arr0[1];
-- |]

-- foo7 :: Def ('[IBool, Uint32] :-> Uint32)
-- foo7 = proc "foo" $ \b a -> body [c|
--     return (b && !b ? a+3 : signum abs a - 4);
-- |]

-- foo8 :: Def ('[IBool, Uint32] :-> Uint32)
-- foo8 = proc "foo" $ \b a -> body [c|
--     foo2();
--     x = foo7 (b, a);
--     foo7 (b, a);
--     return (b && !b ? a+3 : signum abs a - 4);
-- |]

-- foo9 :: Def ('[Uint32] :-> ())
-- foo9 = proc "foo" $ \a -> body [c|
--   forever {
--     let b = a + 3;
--     return;
--   }
-- |]

-- foo10 :: Def ('[Ref s (Array 3 (Stored Uint32)), Ref s (Array 3 (Stored Uint32))] :-> ())
-- foo10 = proc "foo" $ \r0 r1 -> body [c|
--   memcpy r0 r1;
-- |]

-- -- myMod :: Module
-- -- myMod = package "myMod" $ do
-- --   incl foo4

-- --runit = runCompiler [myMod] initialOpts {stdOut = True}

-- -- foo7 :: Def ('[Ref s (Array 3 (Stored Uint32))] :-> Uint32)
-- -- foo7 = proc "foo" $ \arr -> body [c|
-- --   alloc arr[] = {0};
-- --   return arr[1];
-- -- |]

