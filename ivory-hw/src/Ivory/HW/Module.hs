--
-- Module.hs --- Ivory module for 'ivory-hw'.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.Module where

import Ivory.Language
import Ivory.Artifact
import qualified Paths_ivory_hw

hw_moduledef :: ModuleDef
hw_moduledef = inclHeader "ivory_hw_prim.h"

hw_artifacts :: [Artifact]
hw_artifacts =
  [artifactCabalFile Paths_ivory_hw.getDataDir "support/ivory_hw_prim.h"]
