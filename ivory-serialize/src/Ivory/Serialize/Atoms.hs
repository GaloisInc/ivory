{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Serialize.Atoms where

import Ivory.Language
import Ivory.Artifact
import Ivory.Serialize.Class
import qualified Paths_ivory_serialize as P

serializeHeader :: String
serializeHeader = "ivory_serialize_prim.h"

serializeModule :: Module
serializeModule = package "ivory_serialize" $ do
  inclHeader serializeHeader

searializeArtifacts :: [Artifact]
searializeArtifacts = [ a serializeHeader ]
  where
  a f = artifactCabalFile P.getDataDir ("support/" ++ f)

instance Serializable Uint8 where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Uint8] :-> ())
    pack_proc = importProc "ivory_serialize_pack_uint8" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Uint8)
    unpack_proc = importProc "ivory_serialize_unpack_uint8" serializeHeader

  packedSize _      = 1

instance Serializable Sint8 where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Sint8] :-> ())
    pack_proc = importProc "ivory_serialize_pack_int8" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint8)
    unpack_proc = importProc "ivory_serialize_unpack_int8" serializeHeader

  packedSize _      = 1


instance Serializable Uint16 where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Uint16] :-> ())
    pack_proc = importProc "ivory_serialize_pack_uint16" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Uint16)
    unpack_proc = importProc "ivory_serialize_unpack_uint16" serializeHeader

  packedSize _      = 2

instance Serializable Sint16 where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Sint16] :-> ())
    pack_proc = importProc "ivory_serialize_pack_int16" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint16)
    unpack_proc = importProc "ivory_serialize_unpack_int16" serializeHeader

  packedSize _      = 2

instance Serializable Uint32 where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Uint32] :-> ())
    pack_proc = importProc "ivory_serialize_pack_uint32" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Uint32)
    unpack_proc = importProc "ivory_serialize_unpack_uint32" serializeHeader

  packedSize _      = 4

instance Serializable Sint32 where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Sint32] :-> ())
    pack_proc = importProc "ivory_serialize_pack_int32" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint32)
    unpack_proc = importProc "ivory_serialize_unpack_int32" serializeHeader

  packedSize _      = 4

instance Serializable IFloat where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, IFloat] :-> ())
    pack_proc = importProc "ivory_serialize_pack_float" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> IFloat)
    unpack_proc = importProc "ivory_serialize_unpack_float" serializeHeader

  packedSize _      = 4

instance Serializable Uint64 where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Uint64] :-> ())
    pack_proc = importProc "ivory_serialize_pack_uint64" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Uint64)
    unpack_proc = importProc "ivory_serialize_unpack_uint64" serializeHeader

  packedSize _      = 8

instance Serializable Sint64 where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Sint64] :-> ())
    pack_proc = importProc "ivory_serialize_pack_int64" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint64)
    unpack_proc = importProc "ivory_serialize_unpack_int64" serializeHeader

  packedSize _      = 8

instance Serializable IDouble where
  pack dst offs src = call_ pack_proc dst offs src
    where
    pack_proc :: Def('[Ref s (CArray (Stored Uint8)), Uint32, IDouble] :-> ())
    pack_proc = importProc "ivory_serialize_pack_double" serializeHeader

  unpack src offs   = call unpack_proc src offs
    where
    unpack_proc :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> IDouble)
    unpack_proc = importProc "ivory_serialize_unpack_double" serializeHeader

  packedSize _      = 8

