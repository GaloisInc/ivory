-- stack runghc

module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

main = compile [exampleModule] []

-- define the example module, our compilation unit
exampleModule = package "example" $
  return ()
