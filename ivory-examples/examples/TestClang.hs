import System.Environment

import qualified PID
import qualified FibLoop
import qualified Factorial
import qualified String
import qualified FunPtr
import qualified Overflow
import qualified Float
import qualified Alloc
import qualified Area
import qualified Cond
import qualified Forever
import qualified PublicPrivate
import qualified Bits
import qualified SizeOf

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language (Module(),moduleName)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  let opts = initialOpts { includeDir = path, srcDir = path
                         , rtIncludeDir = Nothing }

  mapM_ (compileExample opts) modules

  putStrLn "Compiling: Overflow"
  Overflow.writeOverflow opts

compileExample :: Opts -> Module -> IO ()
compileExample opts m = do
  putStrLn ("Compiling: " ++ moduleName m)
  runCompiler [m] opts

modules :: [Module]
modules = [ PID.cmodule
          , FibLoop.cmodule
          , Factorial.cmodule
          , String.cmodule
          , FunPtr.cmodule
          , Overflow.cmodule
          , Float.cmodule
          , Alloc.cmodule
          , Area.cmodule
          , Cond.cmodule
          , Forever.cmodule
          , PublicPrivate.cmodule
          , Bits.cmodule
          , SizeOf.cmodule
          ]
