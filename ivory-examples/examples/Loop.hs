{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Loop where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

cmodule :: Module
cmodule = package "Loop" $ do
  incl loopTest

runLoopExample :: IO ()
runLoopExample = runCompiler [cmodule] [] initialOpts { outDir = Nothing }

loopTest :: Def ('[] ':-> ())
loopTest  = proc "loopTest" $ body $
  do (0 :: Ix 0) `times` \ _ -> return ()
     retVoid
