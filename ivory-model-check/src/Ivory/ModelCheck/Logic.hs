{-# LANGUAGE GADTs, KindSignatures, EmptyDataDecls, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Ivory.ModelCheck.Logic where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Typeable
import Data.Int
import Data.Word
-- import qualified Data.Bits as Bits

import Data.Binding.Hobbits
import Data.Type.HList

import           Ivory.Language.Syntax.Type
import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.Pretty

----------------------------------------------------------------------
-- The first-order types
----------------------------------------------------------------------

-- | The first-order types, i.e., the base types
data L1Type (a :: *) where
  L1Type_Unit :: L1Type ()
  L1Type_Bool :: L1Type Bool
  L1Type_Integer :: L1Type Integer
  L1Type_Int8 :: L1Type Int8
  L1Type_Int16 :: L1Type Int16
  L1Type_Int32 :: L1Type Int32
  L1Type_Int64 :: L1Type Int64
  L1Type_Word8 :: L1Type Word8
  L1Type_Word16 :: L1Type Word16
  L1Type_Word32 :: L1Type Word32
  L1Type_Word64 :: L1Type Word64

-- | Typeclass for 'L1Type'
class L1Typeable a where
  l1typeRep :: L1Type a

instance L1Typeable () where l1typeRep = L1Type_Unit
instance L1Typeable Bool where l1typeRep = L1Type_Bool
instance L1Typeable Integer where l1typeRep = L1Type_Integer
instance L1Typeable Int8 where l1typeRep = L1Type_Int8
instance L1Typeable Int16 where l1typeRep = L1Type_Int16
instance L1Typeable Int32 where l1typeRep = L1Type_Int32
instance L1Typeable Int64 where l1typeRep = L1Type_Int64
instance L1Typeable Word8 where l1typeRep = L1Type_Word8
instance L1Typeable Word16 where l1typeRep = L1Type_Word16
instance L1Typeable Word32 where l1typeRep = L1Type_Word32
instance L1Typeable Word64 where l1typeRep = L1Type_Word64

-- Build a NuMatching instance for L1Type, needed for Liftable
$(mkNuMatching [t| forall a. L1Type a |])

-- Liftable instance, to lift L1Types out of binding contexts
instance Liftable (L1Type a) where
  mbLift [nuP| L1Type_Unit |] = L1Type_Unit
  mbLift [nuP| L1Type_Bool |] = L1Type_Bool
  mbLift [nuP| L1Type_Integer |] = L1Type_Integer
  mbLift [nuP| L1Type_Int8 |] = L1Type_Int8
  mbLift [nuP| L1Type_Int16 |] = L1Type_Int16
  mbLift [nuP| L1Type_Int32 |] = L1Type_Int32
  mbLift [nuP| L1Type_Int64 |] = L1Type_Int64
  mbLift [nuP| L1Type_Word8 |] = L1Type_Word8
  mbLift [nuP| L1Type_Word16 |] = L1Type_Word16
  mbLift [nuP| L1Type_Word32 |] = L1Type_Word32
  mbLift [nuP| L1Type_Word64 |] = L1Type_Word64

-- | Test if two 'L1Type's are equal.
l1typeEq :: L1Type a -> L1Type b -> Maybe (a :~: b)
l1typeEq L1Type_Unit L1Type_Unit = Just Refl
-- NOTE: we write the cases in this particular style so that we do not forget to
-- add new cases if we add more constructors to L1Type
l1typeEq L1Type_Unit _ = Nothing
l1typeEq L1Type_Bool L1Type_Bool = Just Refl
l1typeEq L1Type_Bool _ = Nothing
l1typeEq L1Type_Integer L1Type_Integer = Just Refl
l1typeEq L1Type_Integer _ = Nothing
l1typeEq L1Type_Int8 L1Type_Int8 = Just Refl
l1typeEq L1Type_Int8 _ = Nothing
l1typeEq L1Type_Int16 L1Type_Int16 = Just Refl
l1typeEq L1Type_Int16 _ = Nothing
l1typeEq L1Type_Int32 L1Type_Int32 = Just Refl
l1typeEq L1Type_Int32 _ = Nothing
l1typeEq L1Type_Int64 L1Type_Int64 = Just Refl
l1typeEq L1Type_Int64 _ = Nothing
l1typeEq L1Type_Word8 L1Type_Word8 = Just Refl
l1typeEq L1Type_Word8 _ = Nothing
l1typeEq L1Type_Word16 L1Type_Word16 = Just Refl
l1typeEq L1Type_Word16 _ = Nothing
l1typeEq L1Type_Word32 L1Type_Word32 = Just Refl
l1typeEq L1Type_Word32 _ = Nothing
l1typeEq L1Type_Word64 L1Type_Word64 = Just Refl
l1typeEq L1Type_Word64 _ = Nothing

----------------------------------------------------------------------
-- The types of functions over first-order types
----------------------------------------------------------------------

-- | Types for functions over first-order types
data L1FunType a where
  L1FunType_base :: L1Type a -> L1FunType a
  L1FunType_cons :: L1Type a -> L1FunType b -> L1FunType (a -> b)

-- | Typeclass for 'L1FunType'
class L1FunTypeable a where
  l1funTypeRep :: L1FunType a

instance L1Typeable a => L1FunTypeable a where
  l1funTypeRep = L1FunType_base l1typeRep
instance (L1Typeable a, L1FunTypeable b) => L1FunTypeable (a -> b) where
  l1funTypeRep = L1FunType_cons l1typeRep l1funTypeRep

-- Build a NuMatching instance for L1FunType, needed for Liftable
$(mkNuMatching [t| forall a. L1FunType a |])

-- Liftable instance, to lift L1FunTypes out of binding contexts
instance Liftable (L1FunType a) where
  mbLift [nuP| L1FunType_base t |] = L1FunType_base (mbLift t)
  mbLift [nuP| L1FunType_cons arg t |] = L1FunType_cons (mbLift arg) (mbLift t)

----------------------------------------------------------------------
-- The types of our logic
----------------------------------------------------------------------

-- | Dummy type for propositions
data Prop deriving Typeable

-- | Dummy type for predicate monads, i.e., set of program transitions
data PM (a :: *) deriving Typeable

-- | A GADT for the types allowed in our logic
data LType (a :: *) where
  LType_Base :: L1Type a -> LType a
  LType_Prop :: LType Prop
  LType_Fun :: LType a -> LType b -> LType (a -> b)
  LType_PM :: LType a -> LType (PM a)

-- | Typeclass for 'LType'
class LTypeable a where
  ltypeRep :: LType a

instance L1Typeable a => LTypeable a where ltypeRep = LType_Base l1typeRep
instance LTypeable Prop where ltypeRep = LType_Prop
instance (LTypeable a, LTypeable b) => LTypeable (a -> b) where
  ltypeRep = LType_Fun ltypeRep ltypeRep
instance LTypeable a => LTypeable (PM a) where ltypeRep = LType_PM ltypeRep

-- Build a NuMatching instance for LType, needed for Liftable
$(mkNuMatching [t| forall a. LType a |])

-- Liftable instance, to lift LTypes out of binding contexts
instance Liftable (LType a) where
  mbLift [nuP| LType_Base t |] = LType_Base (mbLift t)
  mbLift [nuP| LType_Prop |] = LType_Prop
  mbLift [nuP| LType_Fun t1 t2 |] = LType_Fun (mbLift t1) (mbLift t2)
  mbLift [nuP| LType_PM t |] = LType_PM (mbLift t)

-- | Test if two 'LType's are equal
ltypeEq :: LType a -> LType b -> Maybe (a :~: b)
ltypeEq (LType_Base t1) (LType_Base t2) = l1typeEq t1 t2
ltypeEq (LType_Base _) _ = Nothing
ltypeEq LType_Prop LType_Prop = Just Refl
ltypeEq LType_Prop _ = Nothing
ltypeEq (LType_Fun t1 t1') (LType_Fun t2 t2') =
  case (ltypeEq t1 t2, ltypeEq t1' t2') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
ltypeEq (LType_Fun _ _) _ = Nothing
ltypeEq (LType_PM t1) (LType_PM t2) =
  case ltypeEq t1 t2 of
    Just Refl -> Just Refl
    _ -> Nothing
ltypeEq (LType_PM _) _ = Nothing


----------------------------------------------------------------------
-- The expressions of our logic
----------------------------------------------------------------------

-- | Typed, named function symbols, which include literals
data FunSym a
  = FunSym String (LType a)
  | Literal (L1Type a) a

-- | The expressions of our logic as a GADT. This is essentially the typed
-- lambda-calculus with function symbols. All expressions are in beta-normal
-- form, which is enforced by restricting the expressions to only the
-- lambda-abstractions and the applications of variables and function symbols.
data LExpr a where
  LLambda :: LType a -> Binding a (LExpr b) -> LExpr (a -> b)
  LAppExpr :: LAppExpr a -> LExpr a

-- | Expressions that are applications of variables or function symbols.
data LAppExpr a where
  LVar :: Name a -> LAppExpr a
  LFunSym :: FunSym a -> LAppExpr a
  LApp :: LAppExpr (a -> b) -> LExpr a -> LAppExpr b

-- | Helper function for building lambda-abstractions
mkLambda :: LTypeable a => (LExpr a -> LExpr b) -> LExpr (a -> b)
mkLambda f = LLambda ltypeRep $ nu $ \x -> f (LAppExpr (LVar x))

-- | Helper typeclass for making expression-building functions
class EtaExprBuilder out a | out -> a where
  etaBuild :: LAppExpr a -> out

instance EtaExprBuilder (LExpr a) a where
  etaBuild e = LAppExpr e

instance (LTypeable a, EtaExprBuilder out b) =>
         EtaExprBuilder (LExpr a -> out) (a -> b) where
  etaBuild e =
    \x -> etaBuild (LApp e x)

-- | Helper function for building expression functions from function symbols
mkFunSym :: (LTypeable a, EtaExprBuilder out a) => String -> out
mkFunSym str = etaBuild $ LFunSym $ FunSym str ltypeRep

-- | Specialization of 'mkFunSym' to 0-argument functions
mkFunSym0 :: LTypeable a => String -> LExpr a
mkFunSym0 str = mkFunSym str

-- | Specialization of 'mkFunSym' to 1-argument functions
mkFunSym1 :: (LTypeable a, LTypeable b) => String -> LExpr a -> LExpr b
mkFunSym1 str = mkFunSym str

-- | Specialization of 'mkFunSym' to 2-argument functions
mkFunSym2 :: (LTypeable a, LTypeable b, LTypeable c) =>
             String -> LExpr a -> LExpr b -> LExpr c
mkFunSym2 str = mkFunSym str

-- | Helper function for building literal expressions
mkLiteral :: L1Typeable a => a -> LExpr a
mkLiteral a = etaBuild $ LFunSym $ Literal l1typeRep a

