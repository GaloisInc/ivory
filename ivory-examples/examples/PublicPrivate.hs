{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module PublicPrivate where

import Control.Monad (void)
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend


[ivory|
struct Foo { i :: Stored Uint32 }
struct Bar { name :: Array 32 (Stored IChar) }
|]

privateHelper1 :: Def ('[Ref s (Struct "Foo")] :-> Ref s (Stored Uint32))
privateHelper1  = proc "private_helper1" (\s -> body (ret (s ~> i)))

privateHelper2 :: Def ('[] :-> ())
privateHelper2  = proc "private_helper2" $ body retVoid

publicFunction :: Def ('[Ref s (Struct "Bar")] :-> Uint32)
publicFunction = proc "public_function" $ \_ -> body $ do
  a <- call privateHelper1 (addrOf privateFoo)
  call_ privateHelper2
  ret =<< deref a

privateFoo :: MemArea (Struct "Foo")
privateFoo  = area "private_foo" Nothing

cmodule :: Module
cmodule = package "PublicPrivate" $ do
  private $ do
    defStruct (Proxy :: Proxy "Foo")
    defMemArea privateFoo
    incl privateHelper1
    incl privateHelper2
  public $ do
    defStruct (Proxy :: Proxy "Bar")
    incl publicFunction

runPublicPrivate :: IO ()
runPublicPrivate  = void $ runCompiler [cmodule] initialOpts { stdOut = True, constFold = True }
