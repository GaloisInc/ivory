--
-- Module.hs --- Ivory module for 'ivory-hw'.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.HW.Module (
  hw_moduledef,
  hw_artifacts
) where

import Ivory.HW.Prim
import Ivory.Artifact
import qualified Paths_ivory_hw

hw_artifacts :: [Located Artifact]
hw_artifacts =
  [Incl $ artifactCabalFile Paths_ivory_hw.getDataDir "support/ivory_hw_prim.h"]
