{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
--
-- Monad.hs --- Bit field modification Monad.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BitData.Monad where

import Ivory.Language

import qualified MonadLib.Monads as M

import Ivory.BitData.Bits
import Ivory.BitData.BitData

-- | An action that modifies a bit data value of type "d" and returns
-- a "a" in the "Ivory s r" monad.  Values of this type are passed as
-- the "body" argument to "withBits" etc.
newtype BitDataM d a = BitDataM { runBitDataM :: M.State d a }
  deriving (Functor, Monad)

-- | Clear the value of the current bit data value.
clear :: BitData d => BitDataM d ()
clear = return () -- BitDataM $ M.set 0

-- XXX add getField and getBit?  it might get confusing if they are in
-- the monad.

-- | Set a single bit field in the current bit data value.
setBit :: BitData d => BitDataField d Bit -> BitDataM d ()
setBit f = BitDataM $ M.sets_ (setBitDataBit f)

-- | Clear a single bit.
clearBit :: BitData d => BitDataField d Bit -> BitDataM d ()
clearBit f = BitDataM $ M.sets_ (clearBitDataBit f)

-- | Set a field to a value.
setField :: (BitData d, BitData b,
             SafeCast (BitDataRep b) (BitDataRep d))
         => BitDataField d b -> b -> BitDataM d ()
setField f x = BitDataM $ M.sets_ (\v -> setBitDataField f v x)

-- | Execute a bitdata action given an initial value, returning the
-- new bitdata value and the result of the action.
runBits :: BitData d => BitDataRep d -> BitDataM d a -> (a, BitDataRep d)
runBits rep mf = (result, toRep val)
  where (result, val) = M.runState (fromRep rep) (runBitDataM mf)

-- | Execute a bitdata action given an initial value, returning the
-- new bitdata value.
withBits :: BitData d => BitDataRep d -> BitDataM d () -> BitDataRep d
withBits rep mf = snd (runBits rep mf)

-- | Execute a bit data action given a reference to a value, writing
-- the resulting value back to the reference upon completion and
-- returning the result of the action.
withBitsRef :: BitData d
            => Ref s1 (Stored (BitDataRep d))
            -> BitDataM d a
            -> Ivory eff a
withBitsRef ref mf = do
  rep <- deref ref
  let (result, rep') = runBits rep mf
  store ref rep'
  return result
