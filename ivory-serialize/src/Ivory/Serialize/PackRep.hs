{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Serialize.PackRep where

import Ivory.Language

data PackRep t = PackRep
  { packGetLE :: forall s0 s1 s2 r b. ConstRef s1 (CArray (Stored Uint8)) -> Uint32 -> Ref s2 t -> Ivory ('Effects r b (Scope s0)) ()
  , packGetBE :: forall s0 s1 s2 r b. ConstRef s1 (CArray (Stored Uint8)) -> Uint32 -> Ref s2 t -> Ivory ('Effects r b (Scope s0)) ()
  , packSetLE :: forall s0 s1 s2 r b. Ref s1 (CArray (Stored Uint8)) -> Uint32 -> ConstRef s2 t -> Ivory ('Effects r b (Scope s0)) ()
  , packSetBE :: forall s0 s1 s2 r b. Ref s1 (CArray (Stored Uint8)) -> Uint32 -> ConstRef s2 t -> Ivory ('Effects r b (Scope s0)) ()
  , packSize :: Integer
  }

repack :: (IvoryArea a, IvoryZero a)
       => (forall s1 s2 eff. ConstRef s1 a -> Ref s2 b -> Ivory eff ())
       -> (forall s1 s2 eff. ConstRef s1 b -> Ref s2 a -> Ivory eff ())
       -> PackRep a
       -> PackRep b
repack reget reset rep = PackRep
  { packGetLE = \ buf offs base -> do
      tmp <- local izero
      packGetLE rep buf offs tmp
      reget (constRef tmp) base
  , packGetBE = \ buf offs base -> do
      tmp <- local izero
      packGetBE rep buf offs tmp
      reget (constRef tmp) base
  , packSetLE = \ buf offs base -> do
      tmp <- local izero
      reset base tmp
      packSetLE rep buf offs (constRef tmp)
  , packSetBE = \ buf offs base -> do
      tmp <- local izero
      reset base tmp
      packSetBE rep buf offs (constRef tmp)
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
wrapPackRep name rep = WrappedPackRep
  (rep { packGetLE = call_ doGetLE
       , packGetBE = call_ doGetBE
       , packSetLE = call_ doSetLE
       , packSetBE = call_ doSetBE })
  defs
  where
  doGetLE :: Def ('[ConstRef s1 ('CArray ('Stored Uint8)), Uint32, Ref s2 a] :-> ())
  doGetLE = proc (name ++ "_get_le") $ \ buf offs base -> body $ packGetLE rep buf offs base

  doGetBE :: Def ('[ConstRef s1 ('CArray ('Stored Uint8)), Uint32, Ref s2 a] :-> ())
  doGetBE = proc (name ++ "_get_be") $ \ buf offs base -> body $ packGetBE rep buf offs base

  doSetLE :: Def ('[Ref s1 ('CArray ('Stored Uint8)), Uint32, ConstRef s2 a] :-> ())
  doSetLE = proc (name ++ "_set_le") $ \ buf offs base -> body $ packSetLE rep buf offs base

  doSetBE :: Def ('[Ref s1 ('CArray ('Stored Uint8)), Uint32, ConstRef s2 a] :-> ())
  doSetBE = proc (name ++ "_set_be") $ \ buf offs base -> body $ packSetBE rep buf offs base

  defs = do
    incl doGetLE
    incl doGetBE
    incl doSetLE
    incl doSetBE
