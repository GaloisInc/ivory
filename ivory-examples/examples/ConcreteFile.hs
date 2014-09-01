{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--
-- C-like syntax for Ivory, parsed from a file.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Concrete where

import Control.Monad (void)
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend


e :: IBool
e = (4::Sint32) >? 3

type SomeInt = Uint32

macroStmts x y = do
  a <- local (ival 0)
  store a (x + y)

macroStmtsRet x y = do
  a <- local (ival 0)
  store a (x + y)
  return =<< deref a

macroExp x y = do
  x <? y

[ivoryFile|examples/file.ivory|]

runFile :: IO ()
runFile = void $ runCompiler [examplesfile] initialOpts {stdOut = True, constFold = True}
