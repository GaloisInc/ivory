{-# LANGUAGE DataKinds #-}

module Ivory.Serialize.Array where

import Ivory.Language
import Ivory.Serialize.Class

arrayPack :: (ANat len, Serializable rep)
          => Ref s1 (CArray (Stored Uint8))
          -> Uint32
          -> ConstRef s2 (Array len (Stored rep))
          -> Ivory eff ()
arrayPack dst offs src = do
  arr <- assign src  -- Give the source array a local name
  arrayMap $ \ix -> do -- Produce a loop of pack calls
    pack dst (offs + safeCast ix) =<< deref (arr ! ix)

arrayUnpack :: (ANat len, Serializable rep, IvoryStore rep)
            => ConstRef s1 (CArray (Stored Uint8))
            -> Uint32
            -> Ref s (Array len (Stored rep))
            -> Ivory eff ()
arrayUnpack src offs dest = do
  arrayMap $ \ix -> do
    val <- unpack src (offs + safeCast ix)
    store (dest ! ix) val

