import System.Environment

import qualified AddrOfRegression
import qualified Alloc
import qualified Area
import qualified Array
import qualified BitData
import qualified Bits
import qualified ClassHierarchy
import qualified ConcreteFile
import qualified Cond
import qualified ConstPtrRef
import qualified ConstRef
import qualified Coroutine
import qualified Extern
import qualified Factorial
import qualified FibLoop
import qualified FibTutorial
import qualified Float
import qualified Forever
import qualified FunPtr
import qualified Loop
import qualified Overflow
import qualified PID
import qualified PublicPrivate
import qualified SizeOf
import qualified String

import Control.Monad (when)
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Stdlib.String
import Ivory.Language (Module())
import Ivory.Stdlib (stdlibModules)

main :: IO ()
main = do
  args <- getArgs
  when (null args)
       (error "Binary takes a path to srcs and headers as an argument")
  let path = head args
  let opts = initialOpts { outDir = Just path, srcLocs = True }
  compileExample opts modules

compileExample :: Opts -> [Module] -> IO ()
compileExample opts ms = runCompiler ms stdlibStringArtifacts opts

modules :: [Module]
modules = [ AddrOfRegression.cmodule
          , Alloc.cmodule
          , Area.cmodule
          , Array.cmodule
          , BitData.cmodule
          , Bits.cmodule
          , ClassHierarchy.cmodule
          , ConcreteFile.concreteIvory
          , ConcreteFile.examplesfile
          , Cond.cmodule
          , ConstPtrRef.cmodule
          , ConstRef.cmodule
          , Coroutine.cmodule
          , Extern.cmodule
          , Factorial.cmodule
          , FibLoop.cmodule
          , FibTutorial.fib_tutorial_module
          , Float.cmodule
          , Forever.cmodule
          , FunPtr.cmodule
          , Loop.cmodule
          , Overflow.cmodule
          , PID.cmodule
          , PublicPrivate.cmodule
          , SizeOf.cmodule
          , String.cmodule
          ]
          ++ stdlibModules
