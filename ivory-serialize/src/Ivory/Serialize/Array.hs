{-# LANGUAGE DataKinds #-}

module Ivory.Serialize.Array where

import Ivory.Language
import Ivory.Serialize.Class

arrayPack :: (ANat len, IvoryArea rep, SerializableRef rep)
          => Ref s1 (CArray (Stored Uint8))
          -> Uint32
          -> ConstRef s2 (Array len rep)
          -> Ivory eff ()
arrayPack = packRef

arrayUnpack :: (ANat len, IvoryArea rep, SerializableRef rep)
            => ConstRef s1 (CArray (Stored Uint8))
            -> Uint32
            -> Ref s (Array len rep)
            -> Ivory eff ()
arrayUnpack = unpackRef
