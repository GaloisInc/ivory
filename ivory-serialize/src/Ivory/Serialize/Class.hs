{-# LANGUAGE DataKinds #-}

module Ivory.Serialize.Class where

import Ivory.Language

class (IvoryType t, IvoryVar t) => Serializable t where
  -- Given a ref and a byte offset into an array, pack the value of t starting
  -- at the offset.
  pack     :: Ref s (CArray (Stored Uint8))      -> Uint32 -> t -> Ivory eff ()
  -- Given a ref and a byte offset into an array, unpack the value of t starting
  -- at the offset.
  unpack   :: ConstRef s (CArray (Stored Uint8)) -> Uint32      -> Ivory eff t
  -- Number of bytes used to pack
  packedSize :: t -> Int

