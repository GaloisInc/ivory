{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

--
-- Safe.hs --- Checked binary packing/unpacking.
--
-- Copyright (C) 2015, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.Serialize.Safe where

import Ivory.Language
import Ivory.Serialize.Atoms
import Ivory.Serialize.PackRep

packInto :: (Packable a, ANat len)
         => Ref s1 (Array len (Stored Uint8)) -- ^ buf
         -> Uint32 -- ^ offset
         -> ConstRef s2 a -- ^ value
         -> Ivory ('Effects r b (Scope s3)) ()
packInto = packInto' packRep

packInto' :: ANat len
          => PackRep a -- ^ encoding
          -> Ref s1 (Array len (Stored Uint8)) -- ^ buf
          -> Uint32 -- ^ offset
          -> ConstRef s2 a -- ^ value
          -> Ivory ('Effects r b (Scope s3)) ()
packInto' rep buf offs val = do
  assert $ offs + fromIntegral (packSize rep) <=? arrayLen buf
  packSet rep (toCArray buf) offs val

packVInto :: (Packable (Stored a), ANat len, IvoryInit a)
          => Ref s1 (Array len (Stored Uint8)) -- ^ buf
          -> Uint32 -- ^ offset
          -> a -- ^ value
          -> Ivory ('Effects r b (Scope s2)) ()
packVInto = packVInto' packRep

packVInto' :: (ANat len, IvoryInit a)
           => PackRep (Stored a) -- ^ encoding
           -> Ref s1 (Array len (Stored Uint8)) -- ^ buf
           -> Uint32 -- ^ offset
           -> a -- ^ value
           -> Ivory ('Effects r b (Scope s2)) ()
packVInto' rep buf offs val = do
  tmp <- local $ ival val
  packInto' rep buf offs (constRef tmp)


unpackFrom :: (Packable a, ANat len)
           => ConstRef s1 (Array len (Stored Uint8)) -- ^ buf
           -> Uint32 -- ^ offset
           -> Ref s2 a -- ^ value
           -> Ivory ('Effects r b (Scope s3)) ()
unpackFrom = unpackFrom' packRep

unpackFrom' :: ANat len
            => PackRep a -- ^ encoding
            -> ConstRef s1 (Array len (Stored Uint8)) -- ^ buf
            -> Uint32 -- ^ offset
            -> Ref s2 a -- ^ value
            -> Ivory ('Effects r b (Scope s3)) ()
unpackFrom' rep buf offs val = do
  assert $ offs + fromIntegral (packSize rep) <=? arrayLen buf
  packGet rep (toCArray buf) offs val

unpackVFrom :: (Packable (Stored a), ANat len, IvoryStore a, IvoryZeroVal a)
            => ConstRef s1 (Array len (Stored Uint8)) -- ^ buf
            -> Uint32 -- ^ offset
            -> Ivory ('Effects r b (Scope s2)) a
unpackVFrom = unpackVFrom' packRep

unpackVFrom' :: (Packable (Stored a), ANat len, IvoryStore a, IvoryZeroVal a)
             => PackRep (Stored a)
             -> ConstRef s1 (Array len (Stored Uint8)) -- ^ buf
             -> Uint32 -- ^ offset
             -> Ivory ('Effects r b (Scope s2)) a
unpackVFrom' rep buf offs = do
  tmp <- local izero
  unpackFrom' rep buf offs tmp
  deref tmp
