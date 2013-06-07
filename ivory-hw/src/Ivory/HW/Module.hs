--
-- Module.hs --- Ivory module for 'ivory-hw'.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.Module where

import Ivory.Language

hw_moduledef :: ModuleDef
hw_moduledef = do
  inclHeader "ivory_hw_prim.h"
  sourceDep  "ivory_hw_prim.h"
