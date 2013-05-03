--
-- Main.hs --- Generate C for an Ivory module.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import Ivory.HW.Module (hwModule)

main :: IO ()
main = compile [hwModule]
