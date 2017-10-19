{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SizeOf where

import Ivory.Language

[ivory|

struct foo
  { f1 :: Stored Uint8
  ; f2 :: Stored Uint32
  }

|]


test :: Def ('[] :-> Uint8)
test  = proc "sizeof_test" (body (ret (sizeOf (Proxy :: Proxy ('Struct "foo")))))

cmodule :: Module
cmodule  = package "SizeOf" $ do
  defStruct (Proxy :: Proxy "foo")
  incl test
