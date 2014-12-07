module Main where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Compile.C.CmdlineFrontend.Options
import Ivory.Language

import Heartbeat
import PPM
import RingBuffer

opts :: Opts
opts = initialOpts
  { constFold     = True
  , overflow      = True
  , divZero       = True
  , ixCheck       = True
  , fpCheck       = True
  , bitShiftCheck = True
  , srcLocs       = True
  }

mk :: Module -> String -> IO ()
mk m dir = runCompiler [m] [] opts { outDir = Just dir }

main :: IO ()
main = do
  mk ppmModule "ppmOut"
  mk testModule "ringBufferOut"
  mk heartbeatModule "heartbeatOut"
