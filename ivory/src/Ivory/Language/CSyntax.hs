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

  -- a = 3;
  -- b = 4;

foo :: Def ('[] :-> Sint32)
foo = proc "foo" $ body [c|
  return 3 + 4;
|]


bar :: Def ('[] :-> Sint32)
bar = proc "bar" $ body [c|
  if (true) {
    a = 5;
    return a;
  }
  else {
    b = 3;
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

--   x := *v;
bar3 :: Def ('[] :-> Sint32)
bar3 = proc "bar" $ body [c|
  *x = ival 3;
  return *x;
|]

-- bar4 :: Def ('[] :-> Sint32)
-- bar4 = proc "bar" $ body $ do
--   v <- local (ival 3)
--   x <- deref v
--   ret x

 -- v <- local (ival 3)
 -- x <- deref v
 -- ret x
