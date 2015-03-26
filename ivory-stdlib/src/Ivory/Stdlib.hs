
module Ivory.Stdlib
  ( module Ivory.Stdlib.Control
  , module Ivory.Stdlib.Maybe
  , module Ivory.Stdlib.Memory
  , module Ivory.Stdlib.Operators
  , module Ivory.Stdlib.String
  , module Ivory.Stdlib.Trig
  , stdlibModules
  ) where

import Ivory.Language (Module)

import Ivory.Stdlib.Control
import Ivory.Stdlib.Maybe
import Ivory.Stdlib.Memory
import Ivory.Stdlib.Operators
import Ivory.Stdlib.String
import Ivory.Stdlib.Trig

stdlibModules :: [Module]
stdlibModules =
  [ stdlibStringModule
  ]

