
module Ivory.Stdlib
  ( module Ivory.Stdlib.Control
  , module Ivory.Stdlib.Memory
  , module Ivory.Stdlib.Operators
  , module Ivory.Stdlib.String
  , module Ivory.Stdlib.Maybe
  , stdlibModules
  ) where

import Ivory.Language (Module)

import Ivory.Stdlib.Control
import Ivory.Stdlib.Memory
import Ivory.Stdlib.Operators
import Ivory.Stdlib.String
import Ivory.Stdlib.Maybe

stdlibModules :: [Module]
stdlibModules =
  [ stdlibStringModule
  ]

