-- stack runghc
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

main = compile [exampleModule] []

-- define the example module, our compilation unit
exampleModule =
  package "example" $
    do incl ivoryMain

ivoryMain :: Def ('[] ':-> Sint32)
ivoryMain  =
  proc "main" $
  body $
    do ret 0
