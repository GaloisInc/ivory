
module Ivory.Stdlib
  ( module Ivory.Stdlib.Control
  , module Ivory.Stdlib.Memory
  , module Ivory.Stdlib.Operators
  , module Ivory.Stdlib.String
  , stdlibModules
  ) where

import Ivory.Language (Module)

import Ivory.Stdlib.Control
import Ivory.Stdlib.Memory
import Ivory.Stdlib.Operators
import Ivory.Stdlib.String

stdlibModules :: [Module]
stdlibModules =
  [ stdlibStringModule
  ]

