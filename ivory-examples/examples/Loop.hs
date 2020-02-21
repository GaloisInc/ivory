{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Loop where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

cmodule :: Module
cmodule = package "Loop" $ do
  incl loopTest0

runLoopExample :: IO ()
runLoopExample = runCompiler [cmodule] [] initialOpts { outDir = Nothing }

loopTest0 :: Def ('[] :-> ())
loopTest0  = proc "loopTest" $ body $
  do (0 :: Ix 1) `times` \ _ -> return ()
     retVoid
