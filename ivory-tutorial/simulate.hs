-- stack runghc

import Example
import Ivory.ModelCheck

main = modelCheck' [exampleModule] ivoryMain
