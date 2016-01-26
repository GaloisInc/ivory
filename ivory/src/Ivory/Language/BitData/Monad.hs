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

module Ivory.Language.BitData.Monad where

import Data.List (intercalate)

import qualified MonadLib as M

import qualified Control.Applicative as A

import Ivory.Language.BitData.Bits
import Ivory.Language.BitData.BitData
import Ivory.Language.Cast
import Ivory.Language.Ref
import Ivory.Language.Area
import Ivory.Language.Monad
import Ivory.Language.Comment

-- | An action that modifies a bit data value of type "d" and returns
-- a "a" in the "Ivory s r" monad.  Values of this type are passed as
-- the "body" argument to "withBits" etc.
newtype BitDataM d a = BitDataM
  { runBitDataM :: M.StateT d (M.WriterT [String] M.Id) a
  } deriving (Functor, Monad, A.Applicative)

-- | Clear the value of the current bit data value.
clear :: BitData d => BitDataM d ()
clear = return () -- BitDataM $ M.set 0

-- XXX add getField and getBit?  it might get confusing if they are in
-- the monad.

-- | Set a single bit field in the current bit data value.
setBit :: BitData d => BitDataField d Bit -> BitDataM d ()
setBit f = BitDataM $ do
  M.put ["setBit " ++ bitDataFieldName f]
  M.sets_ (setBitDataBit f)

-- | Clear a single bit.
clearBit :: BitData d => BitDataField d Bit -> BitDataM d ()
clearBit f = BitDataM $ do
  M.put ["clearBit " ++ bitDataFieldName f]
  M.sets_ (clearBitDataBit f)

-- | Set a field to a value.
setField :: (BitData d, BitData b,
             SafeCast (BitDataRep b) (BitDataRep d))
         => BitDataField d b -> b -> BitDataM d ()
setField f x = BitDataM $ do
  M.put ["setField " ++ bitDataFieldName f]
  M.sets_ (\v -> setBitDataField f v x)

-- | Execute a bitdata action given an initial value, returning the
-- new bitdata value and the result of the action.
runBits :: BitData d => BitDataRep d -> BitDataM d a -> (a, BitDataRep d, [String])
runBits rep mf = (res, toRep val, s)
  where
  ((res, val), s) = M.runId $ M.runWriterT $ M.runStateT (fromRep rep) (runBitDataM mf)

-- | Execute a bitdata action given an initial value, returning the
-- new bitdata value.
withBits :: BitData d => BitDataRep d -> BitDataM d () -> BitDataRep d
withBits rep mf = let (_, r, _) = runBits rep mf in r

-- | Execute a bit data action given a reference to a value, writing
-- the resulting value back to the reference upon completion and
-- returning the result of the action.
withBitsRef :: BitData d
            => Ref s1 ('Stored (BitDataRep d))
            -> BitDataM d a
            -> Ivory eff a
withBitsRef ref mf = do
  rep <- deref ref
  let (res, rep', ss) = runBits rep mf
  comment ("withBitsRef: " ++ intercalate ", " ss)
  store ref rep'
  return res
