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
import qualified AddrOfRegression
import qualified Array
import qualified ConcreteFile
import qualified Coroutine
import qualified Loop

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
          , AddrOfRegression.cmodule
          , Array.cmodule
          , Overflow.cmodule
          , Coroutine.cmodule
          , ConcreteFile.concreteIvory
          , ConcreteFile.examplesfile
          , Loop.cmodule
          , stdlibStringModule
          ] ++ stdlibModules
