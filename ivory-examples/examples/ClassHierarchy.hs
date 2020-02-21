{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module ClassHierarchy where

import Ivory.Language

--------------------------------------------------------------------------------

[ivory|

-- Three structs defined in the quasiquoter, a base struct and two that'll
-- extend it.
struct StanagBase
  { paramA :: Stored IBool }

-- Annoyingly, the fields are in the same global namespace, so we give unique names to the fields pointing to the base struct.
struct StanagBaseMsg1
  { base1  :: Struct StanagBase
  ; paramC :: Stored IFloat
  }

struct StanagBaseMsg2
  { base2  :: Struct StanagBase
  ; paramD :: Stored IFloat
  }

|]

-- A Haskell class that has a method 'getBase'.  Given a reference to a struct,
-- getBase returns the base state.
--
-- XXX This is boilerplate that might be generated...
class (IvoryStruct sym) => ExtendBase sym where
  getBase :: forall ref s
           . ( IvoryExpr (ref s ('Struct sym))
             , IvoryExpr (ref s ('Struct "StanagBase"))
             , IvoryRef ref
             )
         => ref s ('Struct sym) -> ref s ('Struct "StanagBase")

-- For the parent, it's just a noop (identity).
instance ExtendBase "StanagBase" where
  getBase = id

-- Otherwise, we dereference the base field.
instance ExtendBase "StanagBaseMsg1" where
  getBase ref = ref ~> base1

instance ExtendBase "StanagBaseMsg2" where
  getBase ref = ref ~> base2

-- A polymorphic procedure Ivory macro for references to objects in the
-- hierachy.  Note: this cannot be a C function (or we'd have to specialize it
-- for each use type).
getBaseVal :: ExtendBase sym => Ref s ('Struct sym) -> Ivory eff IBool
getBaseVal ref = do
  let r = getBase ref
  deref (r ~> paramA)

-- A procedure that makes use of the polymorphism.  Regardless of whehter the
-- reference is to a parent or child, we can use the 'getBaseVal' Ivory
-- function.
bar :: Def ([ Ref s ('Struct "StanagBase")
            , Ref s ('Struct "StanagBaseMsg1")
            , Ref s ('Struct "StanagBaseMsg2")
            ] :-> IBool)
bar = proc "bar" $ \r0 r1 r2 -> body $ do
  b0 <- getBaseVal r0
  b1 <- getBaseVal r1
  b2 <- getBaseVal r2
  ret (b0 .&& b1 .&& b2)

cmodule :: Module
cmodule = package "ClassHierarchy" $ do
  defStruct (Proxy :: Proxy "StanagBase")
  defStruct (Proxy :: Proxy "StanagBaseMsg1")
  defStruct (Proxy :: Proxy "StanagBaseMsg2")
  incl bar
