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

-- foo
foo :: Def ('[] :-> Sint32)
foo = proc "foo" $ body [c|
  return 3 + 4;

|]


bar :: Def ('[] :-> Sint32)
bar = proc "bar" $ body [c|
  if (true) {
    a = 5;
  -- foo
  -- bar
    return a;
  -- goo
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


x = (4::Sint32) >? 3
  -- *x = ival 3;
  -- *y = ival 5;

--   x := *v;
bar3 :: Def ('[] :-> IBool)
bar3 = proc "bar" $ body [c|
  return :i x;
|]

  -- return *y + *x;
