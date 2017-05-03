-- stack runghc

import Example (exampleModule)
import Ivory.Compile.C.CmdlineFrontend (compile)

main = compile [exampleModule] []
