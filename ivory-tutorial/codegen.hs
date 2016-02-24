-- stack runghc

import Example
import Ivory.Compile.C.CmdlineFrontend

main = compile [exampleModule] []

