{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-- Needed to check (SafeCast to from) in the instance constraints for
-- RuntimeCast.
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Safe casting.  We assume Floats have 32 bits and Doubles have 64.

module Ivory.Language.Cast
  ( safeCast
  , ivoryCast -- do not export from "Ivory.Language"
  , castWith
  , castDefault
  , signCast
  , SafeCast(), RuntimeCast(), Default(), SignCast()
  , toMaxSize
  , toMinSize
  ) where

import Ivory.Language.Float
import Ivory.Language.IBool
import Ivory.Language.IChar
import Ivory.Language.IIntegral
import Ivory.Language.Proxy
import Ivory.Language.Sint
import Ivory.Language.Type
import Ivory.Language.Uint
import qualified Ivory.Language.Syntax as AST

import Data.Word
import Data.Int

--------------------------------------------------------------------------------
-- Interface functions and methods.

-- | Statically safe casts.
class (IvoryExpr from, IvoryExpr to) => SafeCast from to where
  safeCast :: from -> to
  safeCast = ivoryCast

-- | Cast with a default value if the casted value is too large.
castWith :: RuntimeCast from to => to -> from -> to
castWith deflt from = inBounds deflt from ? (ivoryCast from, deflt)

-- | `CastWith 0` for types for which 0 is defined.
castDefault :: (Default to, RuntimeCast from to) => from -> to
castDefault = castWith defaultVal

-- | SignCast takes a unsigned number into its signed form iff safe,
-- otherwise 0, and same with signed into unsigned
class (IvoryExpr from, IvoryExpr to) => SignCast from to where
  signCast :: from -> to

-- | upperBoundCast implements signCast from unsigned to signed integers
upperBoundCast :: forall from to
                . (IvoryOrd from, IvoryExpr from, IvoryExpr to, Num to, Bounded to)
               => from -> to
upperBoundCast f = (f <=? bound) ? (ivoryCast f, 0)
  where bound = ivoryCast (maxBound :: to)

-- | lowerBoundCast implements signCast from signed to unsigned integers
lowerBoundCast :: forall from to
                . (IvoryOrd from, IvoryExpr from, IvoryExpr to, Num to, Bounded to)
               => from -> to
lowerBoundCast f = (f >? bound) ? (ivoryCast f, 0)
  where bound = ivoryCast (minBound :: to)
--------------------------------------------------------------------------------

-- | Casts requiring runtime checks.
class (IvoryExpr from, IvoryExpr to, Default to) => RuntimeCast from to where
  -- Does the from value fit within the to type?
  inBounds :: to -> from -> IBool

--------------------------------------------------------------------------------
-- Statically safe instances.

-- Booleans.
instance SafeCast IBool IBool     where
  safeCast     = id
instance SafeCast IBool IChar
instance SafeCast IBool Uint16
instance SafeCast IBool Uint8
instance SafeCast IBool Uint32
instance SafeCast IBool Uint64
instance SafeCast IBool Sint8
instance SafeCast IBool Sint16
instance SafeCast IBool Sint32
instance SafeCast IBool Sint64
instance SafeCast IBool IFloat
instance SafeCast IBool IDouble

-- Uint8.
instance SafeCast Uint8 Uint8     where
  safeCast     = id
instance SafeCast Uint8 Uint16
instance SafeCast Uint8 Uint32
instance SafeCast Uint8 Uint64
instance SafeCast Uint8 Sint16
instance SafeCast Uint8 Sint32
instance SafeCast Uint8 Sint64
instance SafeCast Uint8 IFloat
instance SafeCast Uint8 IDouble
instance SignCast Uint8 Sint8 where
  signCast = upperBoundCast

-- Uint16.
instance SafeCast Uint16 Uint16   where
  safeCast     = id
instance SafeCast Uint16 Uint32
instance SafeCast Uint16 Uint64
instance SafeCast Uint16 Sint32
instance SafeCast Uint16 Sint64
instance SafeCast Uint16 IFloat
instance SafeCast Uint16 IDouble
instance SignCast Uint16 Sint16 where
  signCast = upperBoundCast

-- Uint32.
instance SafeCast Uint32 Uint32   where
  safeCast     = id
instance SafeCast Uint32 Uint64
instance SafeCast Uint32 Sint64
instance SafeCast Uint32 IFloat
instance SafeCast Uint32 IDouble
instance SignCast Uint32 Sint32 where
  signCast = upperBoundCast

-- Uint64.
instance SafeCast Uint64 Uint64   where
  safeCast     = id
instance SafeCast Uint64 IDouble
instance SignCast Uint64 Sint64 where
  signCast = upperBoundCast

-- Sint8.
instance SafeCast Sint8 Sint8     where
  safeCast     = id
instance SafeCast Sint8 Sint16
instance SafeCast Sint8 Sint32
instance SafeCast Sint8 Sint64
instance SafeCast Sint8 IFloat
instance SafeCast Sint8 IDouble
instance SignCast Sint8 Uint8 where
  signCast = lowerBoundCast

-- Sint16.
instance SafeCast Sint16 Sint16   where
  safeCast     = id
instance SafeCast Sint16 Sint32
instance SafeCast Sint16 Sint64
instance SafeCast Sint16 IFloat
instance SafeCast Sint16 IDouble
instance SignCast Sint16 Uint16 where
  signCast = lowerBoundCast

-- Sint32.
instance SafeCast Sint32 Sint32   where
  safeCast     = id
instance SafeCast Sint32 Sint64
instance SafeCast Sint32 IFloat
instance SafeCast Sint32 IDouble
instance SignCast Sint32 Uint32 where
  signCast = lowerBoundCast

-- Sint64.
instance SafeCast Sint64 Sint64   where
  safeCast     = id
instance SafeCast Sint64 IDouble
instance SignCast Sint64 Uint64 where
  signCast = lowerBoundCast

-- IFloat.
instance SafeCast IFloat IFloat   where
  safeCast     = id
instance SafeCast IFloat IDouble

-- IDouble.
instance SafeCast IDouble IDouble where
  safeCast     = id

-- IChar.
instance SafeCast IChar IChar     where
  safeCast     = id

-- By the C standard, we can't assume they're unsigned or how big they are (we
-- just know they're at least 8 bits).  So this is the only cast for Char.

--------------------------------------------------------------------------------
-- Runtime check instances.

-- All other casts, for going to a Num type.
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
         ( Bounded   from, Bounded   to
         , IvoryOrd  from, IvoryOrd  to
         , IvoryExpr from, IvoryExpr to
         , Default   from, Default   to
         -- Important constraint!  This means we can compare the values in the
         -- `from` type, since it must be able to hold all the values of the
         -- `to` type.  Alas, it requires undeciable instances....
         , SafeCast  to from
         ) => RuntimeCast from to where

  -- We can assume that comparison in the `from` type is safe due to the above
  -- constraint.
  inBounds = boundPred

--------------------------------------------------------------------------------
-- | Default values for expression types.
class Default a where
  defaultVal :: a

instance Default Uint8  where defaultVal = 0
instance Default Uint16 where defaultVal = 0
instance Default Uint32 where defaultVal = 0
instance Default Uint64 where defaultVal = 0
instance Default Sint8  where defaultVal = 0
instance Default Sint16 where defaultVal = 0
instance Default Sint32 where defaultVal = 0
instance Default Sint64 where defaultVal = 0

instance Default IFloat  where defaultVal = 0
instance Default IDouble where defaultVal = 0

--------------------------------------------------------------------------------
-- Floating

-- Have to define instances for Float and Double separately or you'll get
-- overlapping instances.

-- | Casting from a floating to a `Integral` type always results in truncation.
instance ( Default to
         , Bounded to
         , IvoryIntegral to
         -- Important constraint!  This means we can compare the values in the
         -- `from` type, since it must be able to hold all the values of the
         -- `to` type.  Alas, it requires undeciable instances....
         , SafeCast to IFloat
         ) => RuntimeCast IFloat to where
  inBounds to from = iNot (isnan from) .&& boundPred to from

instance ( Default to
         , Bounded to
         , IvoryIntegral to
         -- Important constraint!  This means we can compare the values in the
         -- `from` type, since it must be able to hold all the values of the
         -- `to` type.  Alas, it requires undeciable instances....
         , SafeCast to IDouble
         ) => RuntimeCast IDouble to where
  inBounds to from = iNot (isnan from) .&& boundPred to from

--------------------------------------------------------------------------------
-- Utils.

boundPred :: forall from to .
  ( IvoryExpr from
  , IvoryExpr to
  , Bounded to
  , IvoryOrd from
  ) => to -> from -> IBool
boundPred _ from = (from <=? ivoryCast (maxBound :: to))
               .&& (from >=? ivoryCast (minBound :: to))

-- XXX Don't export.
-- Type is what we're casting from.
ivoryCast :: forall a b. (IvoryExpr a, IvoryExpr b) => a -> b
ivoryCast x = wrapExpr (AST.ExpSafeCast ty (unwrapExpr x))
  where ty = ivoryType (Proxy :: Proxy a)

toMaxSize :: AST.Type -> Maybe Integer
toMaxSize ty =
  case ty of
    AST.TyInt i  -> Just $ case i of
                    AST.Int8  -> fromIntegral (maxBound :: Int8)
                    AST.Int16 -> fromIntegral (maxBound :: Int16)
                    AST.Int32 -> fromIntegral (maxBound :: Int32)
                    AST.Int64 -> fromIntegral (maxBound :: Int64)
    AST.TyWord w -> Just $ case w of
                    AST.Word8  -> fromIntegral (maxBound :: Word8)
                    AST.Word16 -> fromIntegral (maxBound :: Word16)
                    AST.Word32 -> fromIntegral (maxBound :: Word32)
                    AST.Word64 -> fromIntegral (maxBound :: Word64)
    AST.TyIndex n -> Just n
    _          -> Nothing

toMinSize :: AST.Type -> Maybe Integer
toMinSize ty =
  case ty of
    AST.TyInt i  -> Just $ case i of
                    AST.Int8  -> fromIntegral (minBound :: Int8)
                    AST.Int16 -> fromIntegral (minBound :: Int16)
                    AST.Int32 -> fromIntegral (minBound :: Int32)
                    AST.Int64 -> fromIntegral (minBound :: Int64)
    AST.TyWord w -> Just $ case w of
                    AST.Word8  -> fromIntegral (minBound :: Word8)
                    AST.Word16 -> fromIntegral (minBound :: Word16)
                    AST.Word32 -> fromIntegral (minBound :: Word32)
                    AST.Word64 -> fromIntegral (minBound :: Word64)
    AST.TyIndex _ -> Just 0
    _          -> Nothing

--------------------------------------------------------------------------------
