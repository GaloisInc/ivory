{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Serialize.PackRep where

import Ivory.Language

data PackRep t = PackRep
  { packGet :: forall s0 s1 s2 r b. ConstRef s1 (CArray (Stored Uint8)) -> Uint32 -> Ref s2 t -> Ivory ('Effects r b (Scope s0)) ()
  , packSet :: forall s0 s1 s2 r b. Ref s1 (CArray (Stored Uint8)) -> Uint32 -> ConstRef s2 t -> Ivory ('Effects r b (Scope s0)) ()
  , packSize :: Integer
  }

repack :: (IvoryArea a, IvoryZero a)
       => (forall s1 s2 eff. ConstRef s1 a -> Ref s2 b -> Ivory eff ())
       -> (forall s1 s2 eff. ConstRef s1 b -> Ref s2 a -> Ivory eff ())
       -> PackRep a
       -> PackRep b
repack reget reset rep = PackRep
  { packGet = \ buf offs base -> do
      tmp <- local izero
      packGet rep buf offs tmp
      reget (constRef tmp) base
  , packSet = \ buf offs base -> do
      tmp <- local izero
      reset base tmp
      packSet rep buf offs (constRef tmp)
  , packSize = packSize rep
  }

repackV :: (IvoryZeroVal a, IvoryStore a, IvoryStore b)
        => (a -> b)
        -> (b -> a)
        -> PackRep (Stored a)
        -> PackRep (Stored b)
repackV reget reset = repack (liftRef reget) (liftRef reset)
  where
  liftRef f src dst = do
    v <- deref src
    store dst $ f v

data WrappedPackRep a = WrappedPackRep
  { wrappedPackRep :: PackRep a
  , wrappedPackMod :: ModuleDef
  }

wrapPackRep :: forall a. IvoryArea a => String -> PackRep a -> WrappedPackRep a
wrapPackRep name rep = WrappedPackRep (rep { packGet = call_ doGet, packSet = call_ doSet }) defs
  where
  doGet :: Def ('[ConstRef s1 ('CArray ('Stored Uint8)), Uint32, Ref s2 a] :-> ())
  doGet = proc (name ++ "_get") $ \ buf offs base -> body $ packGet rep buf offs base

  doSet :: Def ('[Ref s1 ('CArray ('Stored Uint8)), Uint32, ConstRef s2 a] :-> ())
  doSet = proc (name ++ "_set") $ \ buf offs base -> body $ packSet rep buf offs base

  defs = do
    incl doGet
    incl doSet
