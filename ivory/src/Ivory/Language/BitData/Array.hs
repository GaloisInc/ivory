{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
--
-- Array.hs --- Bit data array types.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.Language.BitData.Array where

import Ivory.Language.Bits
import Ivory.Language.Proxy

import Ivory.Language.BitData.Bits
import Ivory.Language.BitData.BitData

import GHC.TypeLits(Nat)

-- NOTE: This type family is used to calculate the total size of a bit
-- array by multiplying "n" by the size of "a" in bits.  Once we have
-- the type-nats solver in place, we should no longer need this.
--
-- The quasiquoter may generate multiple instances of "ArraySize" with
-- the same "n", "a" and result, which is allowed by the type family
-- overlapping rules.  It does seem like a bit of a hack though.
type family ArraySize (n :: Nat) (a :: *) :: Nat

-- | An array of "n" bit data elements of type "a".
data BitArray (n :: Nat) a = BitArray { unArray :: Bits (ArraySize n a) }

-- | Return the number of elements in a "BitArray".
bitLength :: forall a n. ANat n => BitArray n a -> Int
bitLength _ = fromIntegral (fromTypeNat (aNat :: NatType n))

instance (ANat n,
          ANat (ArraySize n a),
          BitData a,
          IvoryRep (BitRep (ArraySize n a)))
    => BitData (BitArray n a) where
  type BitType (BitArray n a) = Bits (ArraySize n a)
  toBits = unArray
  fromBits = BitArray

-- | Return the "n"th element of a "BitArray".
(#!) :: forall a n.
        (BitData a,
         ANat n,
         ANat (BitSize a),
         ANat (ArraySize n a),
         BitCast (BitRep (ArraySize n a)) (BitDataRep a),
         IvoryRep (BitRep (ArraySize n a)))
     => BitArray n a -> Int -> a
BitArray bits #! i =
  if (i < 0) || (i >= n')
    then error "bit array index out of bounds"
    else bits #. field
  where
    n'       = fromIntegral (fromTypeNat (aNat :: NatType n)) :: Int
    elemSize = fromIntegral (fromTypeNat (aNat :: NatType (BitSize a))) :: Int
    field    = BitDataField (i * elemSize) elemSize ixname
    ixname = "[" ++ show i ++ "]"

-- | Return a "BitDataField" that accesses the "n"th element of a
-- "BitArray".  This can be composed with other field accessors using
-- "#>".
bitIx :: forall a n.
         (BitData a,
          ANat n,
          ANat (BitSize a),
          ANat (ArraySize n a))
      => Int -> BitDataField (BitArray n a) a
bitIx i = BitDataField (i * elemSize) elemSize ixname
  where
    ixname = "[" ++ show i ++ "]"
    elemSize = fromIntegral (fromTypeNat (aNat :: NatType (BitSize a))) :: Int
