{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

--
-- C-like syntax for Ivory, parsed from a file.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module ConcreteFile where

import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Stdlib

e :: IBool
e = (4::Sint32) >? 3

type SomeInt = Uint32

macroStmts ::
     (Num a, IvoryStore a, IvoryInit a, GetAlloc eff ~ 'Scope s)
  => a -> a -> Ivory eff ()
macroStmts x y = do
  a <- local (ival 0)
  store a (x + y)

macroStmtsRet ::
     (Num a, IvoryStore a, IvoryInit a, GetAlloc eff ~ 'Scope s)
  => a -> a -> Ivory eff a
macroStmtsRet x y = do
  a <- local (ival 0)
  store a (x + y)
  deref a

macroExp :: IvoryOrd a => a -> a -> IBool
macroExp x y = x <? y

toIx' :: ANat n => Uint32 -> Ix n
toIx' ix = toIx (twosComplementCast ix)

concreteIvory :: Module
concreteIvory = package "concreteIvory" $ do
  incl printf
  incl printf2

[ivoryFile|examples/file.ivory|]

main :: IO ()
main = runCompiler [concreteIvory, examplesfile, stdlibStringModule] stdlibStringArtifacts
  initialOpts {outDir = Just "concrete-ivory", constFold = True}


