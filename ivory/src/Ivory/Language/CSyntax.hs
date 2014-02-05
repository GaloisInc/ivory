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
  a = 3;
  b = 4;
  return (a + b);
|]
