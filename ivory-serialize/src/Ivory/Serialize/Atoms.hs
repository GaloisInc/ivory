{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Serialize.Atoms
  ( serializeHeader
  , serializeModule
  , serializeArtifacts
  , Packable(..)
  ) where

import Prelude hiding ((!!))

import Ivory.Language
import qualified Ivory.Language.Array as I
import Ivory.Language.Proxy
import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Type as I
import qualified Ivory.Language.Uint as I
import Ivory.Artifact
import Ivory.Serialize.PackRep
import qualified Paths_ivory_serialize as P

class Packable a where
  packRep :: PackRep a

instance (ANat len, IvoryArea area, Packable area) => Packable (Array len area) where
  packRep = PackRep
    { packGetLE = \ buf offs arr -> arrayMap $ \ ix -> packGetLE elRep buf (offs + fromInteger (packSize elRep) * safeCast ix) (arr ! ix)
    , packGetBE = \ buf offs arr -> arrayMap $ \ ix -> packGetBE elRep buf (offs + fromInteger (packSize elRep) * safeCast ix) (arr ! ix)
    , packSetLE = \ buf offs arr -> arrayMap $ \ ix -> packSetLE elRep buf (offs + fromInteger (packSize elRep) * safeCast ix) (arr ! ix)
    , packSetBE = \ buf offs arr -> arrayMap $ \ ix -> packSetBE elRep buf (offs + fromInteger (packSize elRep) * safeCast ix) (arr ! ix)
    , packSize = packSize elRep * fromTypeNat (aNat :: NatType len)
    }
    where
    elRep = packRep :: PackRep area

serializeModule :: Module
serializeModule = package "ivory_serialize" $ do
  wrappedPackMod ibool
  wrappedPackMod uint8
  wrappedPackMod int8
  wrappedPackMod uint16
  wrappedPackMod int16
  wrappedPackMod uint32
  wrappedPackMod int32
  wrappedPackMod float
  wrappedPackMod uint64
  wrappedPackMod int64
  wrappedPackMod double

serializeHeader :: String
serializeHeader = "ivory_serialize_prim.h"

serializeArtifacts :: [Located Artifact]
serializeArtifacts = [ Incl $ a serializeHeader ]
  where
  a f = artifactCabalFile P.getDataDir ("support/" ++ f)

ibool :: WrappedPackRep (Stored IBool)
ibool = wrapPackRep "ibool" (repackV (/=? 0) (? ((1 :: Uint8), 0)) (packRep :: PackRep (Stored Uint8)))
instance Packable (Stored IBool) where
  packRep = wrappedPackRep ibool

uint8 :: WrappedPackRep (Stored Uint8)
uint8 = mkPackRep "uint8" 1
instance Packable (Stored Uint8) where
  packRep = wrappedPackRep uint8

int8 :: WrappedPackRep (Stored Sint8)
int8 = mkPackRep "int8" 1
instance Packable (Stored Sint8) where
  packRep = wrappedPackRep int8

uint16 :: WrappedPackRep (Stored Uint16)
uint16 = mkPackRep "uint16" 2
instance Packable (Stored Uint16) where
  packRep = wrappedPackRep uint16

int16 :: WrappedPackRep (Stored Sint16)
int16 = mkPackRep "int16" 2
instance Packable (Stored Sint16) where
  packRep = wrappedPackRep int16

uint32 :: WrappedPackRep (Stored Uint32)
uint32 = mkPackRep "uint32" 4
instance Packable (Stored Uint32) where
  packRep = wrappedPackRep uint32

int32 :: WrappedPackRep (Stored Sint32)
int32 = mkPackRep "int32" 4
instance Packable (Stored Sint32) where
  packRep = wrappedPackRep int32

float :: WrappedPackRep (Stored IFloat)
float = mkPackRep "float" 4
instance Packable (Stored IFloat) where
  packRep = wrappedPackRep float

uint64 :: WrappedPackRep (Stored Uint64)
uint64 = mkPackRep "uint64" 8
instance Packable (Stored Uint64) where
  packRep = wrappedPackRep uint64

int64 :: WrappedPackRep (Stored Sint64)
int64 = mkPackRep "int64" 8
instance Packable (Stored Sint64) where
  packRep = wrappedPackRep int64

double :: WrappedPackRep (Stored IDouble)
double = mkPackRep "double" 8
instance Packable (Stored IDouble) where
  packRep = wrappedPackRep double


mkPackRep :: forall a. (IvoryArea (Stored a), IvoryEq a) => String -> Integer -> WrappedPackRep (Stored a)
mkPackRep ty sz = WrappedPackRep
  (PackRep { packGetLE = call_ doGetLE
           , packGetBE = call_ doGetBE
           , packSetLE = call_ doSetLE
           , packSetBE = call_ doSetBE
           , packSize = sz })
           defs
  where
  doGetLE :: Def ('[ConstRef s1 ('CArray ('Stored Uint8)), Uint32, Ref s2 (Stored a)] :-> ())
  doGetLE = proc ("ivory_serialize_unpack_" ++ ty ++ "_le") $ \ buf offs base -> ensures_ (checkStored base $ \ v -> (buf !! offs) ==? v) $ importFrom serializeHeader

  doGetBE :: Def ('[ConstRef s1 ('CArray ('Stored Uint8)), Uint32, Ref s2 (Stored a)] :-> ())
  doGetBE = proc ("ivory_serialize_unpack_" ++ ty ++ "_be") $ \ buf offs base -> ensures_ (checkStored base $ \ v -> (buf !! offs) ==? v) $ importFrom serializeHeader

  doSetLE :: Def ('[Ref s1 ('CArray ('Stored Uint8)), Uint32, ConstRef s2 (Stored a)] :-> ())
  doSetLE = proc ("ivory_serialize_pack_" ++ ty ++ "_le") $ \ buf offs base -> ensures_ (checkStored base $ \ v -> (buf !! offs) ==? v) $ importFrom serializeHeader

  doSetBE :: Def ('[Ref s1 ('CArray ('Stored Uint8)), Uint32, ConstRef s2 (Stored a)] :-> ())
  doSetBE = proc ("ivory_serialize_pack_" ++ ty ++ "_be") $ \ buf offs base -> ensures_ (checkStored base $ \ v -> (buf !! offs) ==? v) $ importFrom serializeHeader

  defs = do
    incl doGetLE
    incl doGetBE
    incl doSetLE
    incl doSetBE

(!!) :: (IvoryRef ref, IvoryExpr (ref s (CArray (Stored Uint8))), IvoryExpr a)
     => ref s (CArray (Stored Uint8)) -> Uint32 -> a
arr !! ix = I.wrapExpr (I.ExpIndex ty (I.unwrapExpr arr) I.ixRep (I.getUint32 ix))
  where
  ty    =  I.TyCArray (I.TyWord I.Word8)
