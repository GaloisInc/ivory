-- XXX testing
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--
-- C-like syntax for Ivory.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete where

import Ivory.Language.Syntax.Concrete.QQ

-- XXX testing
import Ivory.Language

e :: IBool
e = (4::Sint32) >? 3

[ivory|

-- void mapProc(*uint8_t[4] arr) {
--   map ix {
--     let v = arr ! ix;
--     *v = *v;
--   }
-- }


int32_t foo0() {
  alloc *x = 3;
  *x = 4;
  return *x + 4;
}

int32_t foo1() {
  if (true) {
    let a = 5;
    return a;
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

--foo6 :: Def ('[Ref s (Array 3 (Stored Uint32))] :-> Uint32)
uint32_t foo6(v *uint32_t[3] arr0) {
  alloc arr1[] = {1,2,3};
  map ix {
    *arr0 ! ix = *arr1 ! ix;
  }
  return *arr0 ! 1 ;
}
{ pre(*arr0 ! 1 > 0);
}

--foo7 :: Def ('[IBool, Uint32] :-> Uint32)
uint32_t foo7(bool b, uint32_t a) {
    return (b && !b ? a+3 : signum abs a - 4);
}

--foo8 :: Def ('[IBool, Uint32] :-> Uint32)
uint32_t foo8(bool b, uint32_t a) {
    foo2();
    x = foo7 (b, a);
    foo7 (b, a);
    return (b && !b ? a+3 : signum abs a - 4);
}

void foo9(uint32_t a) {
  forever {
    let b = a + 3;
    return;
  }
}

void foo10(*uint32_t[3] r0, *uint32_t[3] r1) {
  memcpy r0 r1;
}

uint32_t foo11(const *uint32_t i) {
  return *i;
}

uint8_t foo12(* uint8_t a, g*uint8_t b, * uint8_t c, s* uint8_t d) {
  *b = *a;
  return *b;
}

struct Foo
 { aFoo :: Stored IBool
 ; aBar :: Array 4 (Stored Uint32)
 }

bool foo13(* struct Foo f) {
  return *(f &> aFoo);
}

-- Allocated on either the stack or globally, with user-provided type-variables.
xx* uint32_t foo14(xx* struct Foo f) {
  let a = f &> aBar;
  let u = a ! 2;
  return u;
}

uint32_t foo15(uint32_t a, * struct Foo f, * struct Foo g) {
        return a;
}
{ pre(a < 4);
  pre(a > 0);
  post(return > 5);
  pre(* f &> aFoo && * g &> aFoo);
}

-- Stack allocated
uint32_t foo16(S*uint32_t i) {
  return *i;
}
{ pre(*i > 3); }

-- Global allocated
uint32_t foo17(G*uint32_t i) {
  return *i;
}

-- Map over an array, adding x to each cell.
-- XXX??
-- void mapProc(*uint8_t[4] arr) {
--   map ix {
--     let v = arr ! ix;
--     *v = *v + 3;
--   }
-- }

-- abstract struct fooStruct "foobar/foo.h"

|]

bar :: Def ('[Ref s (Stored Uint32)] :-> ())
bar = proc "bar" $ \ref -> requires (checkStored (ref) (\x -> true)) $ body $ retVoid
