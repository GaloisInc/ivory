{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module ConstRef where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

import Control.Monad ((<=<))


test :: Def ('[ConstRef (Stored Uint8)] :-> Uint8)
test  = proc "test" (ret <=< deref)

runConstRef :: IO ()
runConstRef = runCompiler
                [package "ConstRef" (incl test)]
                []
                initialOpts { outDir = Nothing }
