{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Example of a private struct, defined as a global memory area, with a public
-- access function.

module PublicPrivate where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

[ivory|
struct Foo { foo_i   :: Stored Sint32
           ; foo_cnt :: Stored Uint32
           }
|]

privateFoo :: MemArea ('Struct "Foo")
privateFoo  = area "private_foo" $
  Just (istruct [foo_i .= ival 0, foo_cnt .= ival 0])

privUpdate :: Def ('[Sint32] ':-> ())
privUpdate = proc "privUpdate" $ \v -> body $ do
  let foo = addrOf privateFoo
  curr <- deref (foo ~> foo_cnt)
  store (foo ~> foo_i) v
  store (foo~> foo_cnt) (curr+1)

pubUpdate :: Def ('[Sint32] ':-> ())
pubUpdate = proc "pubUpdate" $ \v -> body $ do
  call_ privUpdate v

cmodule :: Module
cmodule = package "PublicPrivate" $ do
  private $ do
    defStruct (Proxy :: Proxy "Foo")
    defMemArea privateFoo
    incl privUpdate
  public $ do
    incl pubUpdate

runPublicPrivate :: IO ()
runPublicPrivate  = runCompiler [cmodule] []
  initialOpts { outDir = Nothing, constFold = True }
