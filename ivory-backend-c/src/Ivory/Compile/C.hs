module Ivory.Compile.C
  ( compile
  , compileModule
  , runOpt
  , showModule
  , renderHdr
  , renderSrc
  , CompileUnits(..)
  , outputProcSyms
  ) where

-- Umbrella module

import Ivory.Compile.C.Gen
import Ivory.Compile.C.Modules
import Ivory.Compile.C.Types
