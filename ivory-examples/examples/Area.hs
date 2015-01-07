{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Area where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

[ivory|

struct val {
  field :: Stored Uint32
}

|]

val :: MemArea (Struct "val")
val  = area "val" (Just (istruct [field .= ival 0]))

cval :: ConstMemArea (Struct "val")
cval  = constArea "cval" (istruct [field .= ival 10])

getVal :: Def ('[] :-> Uint32)
getVal = proc "getVal" $ body $ do
  ret =<< deref (addrOf val ~> field)

setVal :: Def ('[Uint32] :-> ())
setVal = proc "setVal" $ \ n -> body $ do
  store (addrOf val ~> field) n
  retVoid

cmodule :: Module
cmodule = package "Area" $ do
  incl getVal
  incl setVal
  defMemArea val
  defConstMemArea cval
  defStruct (Proxy :: Proxy "val")

main :: IO ()
main = runCompiler [cmodule] [] initialOpts { outDir = Nothing, constFold = True }
