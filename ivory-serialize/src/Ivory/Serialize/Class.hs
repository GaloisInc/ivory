{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Serialize.Class where

import Ivory.Language

class SerializableRef t where
  -- Given a ref and a byte offset into an array, pack the value of t starting
  -- at the offset.
  packRef :: Ref s1 (CArray (Stored Uint8)) -> Uint32 -> ConstRef s2 t -> Ivory eff ()
  default packRef :: (Serializable s, t ~ Stored s, IvoryStore s)
                  => Ref s1 (CArray (Stored Uint8))
                  -> Uint32
                  -> ConstRef s2 t
                  -> Ivory eff ()
  packRef into at from = deref from >>= pack into at

  -- Given a ref and a byte offset into an array, unpack the value of t starting
  -- at the offset.
  unpackRef :: ConstRef s1 (CArray (Stored Uint8))
            -> Uint32
            -> Ref s2 t
            -> Ivory eff ()
  default unpackRef :: (IvoryStore s, Serializable s, t ~ Stored s)
                    => ConstRef s1 (CArray (Stored Uint8))
                    -> Uint32 -> Ref s2 t -> Ivory eff ()
  unpackRef from at to = unpack from at >>= store to

instance (ANat len, IvoryArea a, SerializableRef a)
      => SerializableRef (Array len a) where
  packRef dst offs src = arrayMap $ \ix ->
    packRef dst (offs + safeCast ix) (src ! ix)

  unpackRef src offs dst = arrayMap $ \ix ->
    unpackRef src (offs + safeCast ix) (dst ! ix)

class (IvoryVar t, SerializableRef (Stored t)) => Serializable t where
  pack     :: Ref s (CArray (Stored Uint8))      -> Uint32 -> t -> Ivory eff ()
  unpack   :: ConstRef s (CArray (Stored Uint8)) -> Uint32      -> Ivory eff t
