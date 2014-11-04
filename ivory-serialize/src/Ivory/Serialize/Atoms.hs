{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Serialize.Atoms (serializeHeader, serializeModule) where

import Prelude hiding ((!!))

import Ivory.Language
import qualified Ivory.Language.Array as I
import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Type as I
import qualified Ivory.Language.Uint as I
import Ivory.Serialize.Class

serializeHeader :: String
serializeHeader = "ivory_serialize_prim.h"

serializeModule :: Module
serializeModule = package "ivory_serialize" $ do
  inclHeader serializeHeader
  sourceDep  serializeHeader
  incl packU8
  incl unpackU8
  incl packS8
  incl unpackS8
  incl packU16
  incl unpackU16
  incl packS16
  incl unpackS16
  incl packU32
  incl unpackU32
  incl packS32
  incl unpackS32
  incl packU64
  incl unpackU64
  incl packS64
  incl unpackS64
  incl packF
  incl unpackF
  incl packD
  incl unpackD


instance Serializable Uint8 where
  pack dst offs src = call_ packU8 dst offs src
  unpack src offs   = call unpackU8 src offs
  packedSize _      = 1

packU8 :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Uint8] :-> ())
packU8 = mkPack "ivory_serialize_pack_uint8"

unpackU8 :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Uint8)
unpackU8 = mkUnpack "ivory_serialize_unpack_uint8"


instance Serializable Sint8 where
  pack dst offs src = call_ packS8 dst offs src
  unpack src offs   = call unpackS8 src offs
  packedSize _      = 1

packS8 :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Sint8] :-> ())
packS8 = mkPack "ivory_serialize_pack_int8"

unpackS8 :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint8)
unpackS8 = mkUnpack "ivory_serialize_unpack_int8"


instance Serializable Uint16 where
  pack dst offs src = call_ packU16 dst offs src
  unpack src offs   = call unpackU16 src offs
  packedSize _      = 2

packU16 :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Uint16] :-> ())
packU16 = mkPack "ivory_serialize_pack_uint16"

unpackU16 :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Uint16)
unpackU16 = mkUnpack "ivory_serialize_unpack_uint16"


instance Serializable Sint16 where
  pack dst offs src = call_ packS16 dst offs src
  unpack src offs   = call unpackS16 src offs
  packedSize _      = 2

packS16 :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Sint16] :-> ())
packS16 = mkPack "ivory_serialize_pack_int16"

unpackS16 :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint16)
unpackS16 = mkUnpack "ivory_serialize_unpack_int16"


instance Serializable Uint32 where
  pack dst offs src = call_ packU32 dst offs src
  unpack src offs   = call unpackU32 src offs
  packedSize _      = 4

packU32 :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Uint32] :-> ())
packU32 = mkPack "ivory_serialize_pack_uint32"

unpackU32 :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Uint32)
unpackU32 = mkUnpack "ivory_serialize_unpack_uint32"


instance Serializable Sint32 where
  pack dst offs src = call_ packS32 dst offs src
  unpack src offs   = call unpackS32 src offs
  packedSize _      = 4

packS32 :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Sint32] :-> ())
packS32 = mkPack "ivory_serialize_pack_int32"

unpackS32 :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint32)
unpackS32 = mkUnpack "ivory_serialize_unpack_int32"


instance Serializable IFloat where
  pack dst offs src = call_ packF dst offs src
  unpack src offs   = call unpackF src offs
  packedSize _      = 4

packF :: Def('[Ref s (CArray (Stored Uint8)), Uint32, IFloat] :-> ())
packF = mkPack "ivory_serialize_pack_float"
unpackF :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> IFloat)
unpackF = mkUnpack "ivory_serialize_unpack_float"


instance Serializable Uint64 where
  pack dst offs src = call_ packU64 dst offs src
  unpack src offs   = call unpackU64 src offs
  packedSize _      = 8

packU64 :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Uint64] :-> ())
packU64 = mkPack "ivory_serialize_pack_uint64"

unpackU64 :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Uint64)
unpackU64 = mkUnpack "ivory_serialize_unpack_uint64"


instance Serializable Sint64 where
  pack dst offs src = call_ packS64 dst offs src
  unpack src offs   = call unpackS64 src offs
  packedSize _      = 8

packS64 :: Def('[Ref s (CArray (Stored Uint8)), Uint32, Sint64] :-> ())
packS64 = mkPack "ivory_serialize_pack_int64"

unpackS64 :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> Sint64)
unpackS64 = mkUnpack "ivory_serialize_unpack_int64"


instance Serializable IDouble where
  pack dst offs src = call_ packD dst offs src
  unpack src offs   = call unpackD src offs
  packedSize _      = 8

packD :: Def('[Ref s (CArray (Stored Uint8)), Uint32, IDouble] :-> ())
packD = mkPack "ivory_serialize_pack_double"

unpackD :: Def('[ConstRef s (CArray (Stored Uint8)), Uint32] :-> IDouble)
unpackD = mkUnpack "ivory_serialize_unpack_double"



mkPack
  :: (IvoryArea area, IvoryEq a, IvoryRef ref,
      IvoryExpr (ref s ('CArray area))) =>
     I.Sym -> Def ('[ref s ('CArray area), Uint32, a] ':-> ())
mkPack nm = proc nm
          $ \arr ix v -> ensures_ ((arr !! ix) ==? v)
          $ importFrom serializeHeader

mkUnpack
  :: (IvoryArea area, IvoryEq ret, IvoryRef ref,
      IvoryExpr (ref s ('CArray area))) =>
     I.Sym -> Def ('[ref s ('CArray area), Uint32] ':-> ret)
mkUnpack nm = proc nm
            $ \arr ix -> ensures (\r -> r ==? arr !! ix)
            $ importFrom serializeHeader

(!!) :: forall s area ref a.
        ( IvoryArea area, IvoryRef ref
        , IvoryExpr (ref s (CArray area)), IvoryExpr a)
     => ref s (CArray area) -> Uint32 -> a
arr !! ix = I.wrapExpr (I.ExpIndex ty (I.unwrapExpr arr) I.ixRep (I.getUint32 ix))
  where
  ty    =  I.TyCArray (I.TyWord I.Word8)
