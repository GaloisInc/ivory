
module Ivory.Compile.AADL.Identifier where

identifier :: String -> String
identifier = map aux
  where
  aux '-' = '_'
  aux a   = a

