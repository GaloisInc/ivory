{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- SafePack.hs --- Checked binary packing/unpacking.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.Serialize.Safe (
  -- * Classes
  MonadIvory, liftI,

  -- * Types
  PackM, UnpackM,

  -- * Packing
  mpack, mpackV, marrayPack, packInto, packInto_,

  -- * Unpacking
  munpack, marrayUnpack, unpackFrom, unpackFrom_
) where

import Ivory.Language
import Ivory.Serialize.Class
import MonadLib hiding (local)
import Control.Applicative

-- TODO: We probably need a "skip" function that adjusts the offset in
-- PackM/UnpackM in case we want to skip fields.

-- | Type class for lifting Ivory actions into pack/unpack monads.
--
-- This should live elsewhere if we use more monad transformers on top
-- of Ivory.
class MonadIvory m where
  -- | Lift an Ivory action into the monad "m".
  liftI :: Ivory eff a -> m eff a

----------------------------------------------------------------------
-- Packing Monad

-- | Monad for safely packing values into an array with bounds
-- checking (at code-generation time).
newtype PackM eff a =
  PackM {
    runPackM :: forall s len. (ANat len)
             => (StateT Int
                 (ReaderT (Ref s (Array len (Stored Uint8)))
                  (Ivory eff)) a)
  }


instance Functor (PackM eff) where
  fmap f (PackM x) = PackM (fmap f x)

instance Applicative (PackM eff) where
  pure x  = PackM (pure x)
  (PackM f) <*> (PackM x) = PackM (f <*> x)

instance Monad (PackM eff) where
  return x = PackM (return x)
  (PackM m) >>= f = PackM (m >>= runPackM . f)

instance MonadIvory PackM where
  liftI m = PackM (lift (lift m))

-- | Dereference a "ConstRef" and pack the value into the array stored
-- in the context established by "packInto".  An error will be thrown
-- at code generation time if too much data is packed into the array.
mpack :: forall a s eff. (IvorySizeOf a, SerializableRef a) => ConstRef s a -> PackM eff ()
mpack ref = PackM $ do
  buf    <- ask
  offset <- get
  let new_offset = offset + fromIntegral (sizeOfBytes (Proxy :: Proxy a))
  if new_offset > arrayLen buf
    then error $ "packing " ++ (show new_offset) ++ " bytes into "
              ++ "array of length " ++ (show (arrayLen buf :: Int))
    else return ()
  lift $ lift $ packRef (toCArray buf) (fromIntegral offset) ref
  set new_offset

mpackV :: forall a eff. (IvorySizeOf (Stored a), Serializable a) => a -> PackM eff ()
mpackV val = PackM $ do
  buf    <- ask
  offset <- get
  let new_offset = offset + fromIntegral (sizeOfBytes (Proxy :: Proxy (Stored a)))
  if new_offset > arrayLen buf
    then error $ "packing " ++ (show new_offset) ++ " bytes into "
              ++ "array of length " ++ (show (arrayLen buf :: Int))
    else return ()
  lift $ lift $  pack (toCArray buf) (fromIntegral offset) val
  set new_offset

-- | Pack an array of packable values into the array stored in the
-- context established by "packInto".  An error will be thrown at code
-- generation time if too much data is packed into the array.
--
-- XXX array ref should be const
marrayPack :: (IvorySizeOf (Stored a), Serializable a, ANat len)
           => Ref s (Array len (Stored a)) -> PackM eff ()
marrayPack = mpack . constRef

-- | Begin a context to pack values into a byte array, given an
-- initial offset.  Returns the final offset.
packInto :: (ANat len)
         => (Ref s (Array len (Stored Uint8))) -- buf
         -> Int                                -- offset
         -> PackM eff ()                       -- body
         -> Ivory eff Int
packInto buf offset m =
  runReaderT buf (liftM snd (runStateT offset (runPackM m)))

-- | Like "packInto" but doesn't return the final offset.
packInto_ :: (ANat len)
          => (Ref s (Array len (Stored Uint8))) -- buf
          -> Int                                -- offset
          -> PackM eff ()                       -- body
          -> Ivory eff ()
packInto_ buf offset m = packInto buf offset m >> return ()

----------------------------------------------------------------------
-- Unpacking Monad

-- | Monad for safely unpacking values from an array with bounds
-- checking (at code-generation time).
newtype UnpackM eff a =
  UnpackM {
    runUnpackM :: forall s len. (ANat len)
               => (StateT Int
                   (ReaderT (ConstRef s (Array len (Stored Uint8)))
                    (Ivory eff)) a)
  }

instance Functor (UnpackM eff) where
  fmap f (UnpackM x) = UnpackM (fmap f x)

instance Applicative (UnpackM eff) where
  pure x  = UnpackM (pure x)
  (UnpackM f) <*> (UnpackM x) = UnpackM (f <*> x)

instance Monad (UnpackM eff) where
  return x = UnpackM (return x)
  (UnpackM m) >>= f = UnpackM (m >>= runUnpackM . f)

instance MonadIvory UnpackM where
  liftI m = UnpackM (lift (lift m))

-- | Unpack a value from the array stored in the context established
-- by "unpackFrom" into a reference.  An error will be thrown at code
-- generation time if too much data is unpacked from the array.
munpack :: forall eff a s.
           (IvorySizeOf a, SerializableRef a)
        => Ref s a -> UnpackM eff ()
munpack ref = UnpackM $ do
  buf    <- ask
  offset <- get
  let new_offset = offset + fromIntegral (sizeOfBytes (Proxy :: Proxy a))
  if new_offset > arrayLen buf
    then error $ "unpacking " ++ (show new_offset) ++ " bytes from "
              ++ "array of length " ++ (show (arrayLen buf :: Int))
    else return ()
  lift $ lift $ unpackRef (toCArray buf) (fromIntegral offset) ref
  set new_offset

-- | Unpack an array of values from the array stored in the context
-- established by "unpackFrom".  An error will be thrown at code
-- generation time if too much data is unpacked from the array.
marrayUnpack :: (SerializableRef a, ANat len, IvorySizeOf a)
             => Ref s (Array len a)
             -> UnpackM eff ()
marrayUnpack = munpack

-- | Begin a context to unpack values from a byte array, given an
-- initial offset.  Returns the final offset.
unpackFrom :: (ANat len)
           => (ConstRef s (Array len (Stored Uint8))) -- buf
           -> Int                                     -- offset
           -> UnpackM eff ()                          -- body
           -> Ivory eff Int
unpackFrom buf offset m =
  runReaderT buf (liftM snd (runStateT offset (runUnpackM m)))

-- | Like "unpackFrom" but doesn't return the final offset.
unpackFrom_ :: (ANat len)
            => (ConstRef s (Array len (Stored Uint8))) -- buf
            -> Int                                     -- offset
            -> UnpackM eff ()                          -- body
            -> Ivory eff ()
unpackFrom_ buf offset m = unpackFrom buf offset m >> return ()
