{-# LANGUAGE GADTs, KindSignatures, EmptyDataDecls, TypeOperators, EmptyCase #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds, PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns, DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.ModelCheck.Logic where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Proxy
import Data.Bits
import Data.Ratio
import Data.Typeable
import Data.Int
import Data.Word
import Data.List
import Data.Maybe
import Data.Functor.Identity
import Data.Type.Equality
import Numeric.Natural

import Control.Applicative
import MonadLib

import Data.Binding.Hobbits

-- Debug flag to turn off logical simplifications (should usually be True)
doSimplifications = True


----------------------------------------------------------------------
-- * Helper definitions
----------------------------------------------------------------------

groupAList :: Eq key => [(key, val)] -> [(key, [val])]
groupAList = foldr insertHelper [] where
  insertHelper (k,v) ((k',vs):ksvs) =
    if k == k' then ((k',v:vs):ksvs) else (k',vs):(insertHelper (k,v) ksvs)
  insertHelper (k,v) [] = [(k,[v])]

-- | FIXME: documentation, move this
mbLower1 :: Mb ctx b -> Mb (ctx ':> a) b
mbLower1 mb_expr = mbCombine $ fmap (\e -> nu $ \_ -> e) mb_expr

-- | FIXME: documentation, move this
clMbList :: NuMatching a => Closed (Mb ctx [a]) -> [Closed (Mb ctx a)]
clMbList [clNuP| [] |] = []
clMbList [clNuP| (x:xs) |] = x : clMbList xs

-- | Map a 'Closed' function via a 'Functor' instance
cl_fmap :: Functor f => Closed (a -> b) -> Closed (f a) -> Closed (f b)
cl_fmap f_cl x_cl =
  $(mkClosed [| \f x -> fmap f x |]) `clApply` f_cl `clApply` x_cl

-- | Apply 'mbCombine' to a 'Closed' name-binding
clMbCombine :: Closed (Mb ctx1 (Mb ctx2 a)) ->
               Closed (Mb (ctx1 :++: ctx2) a)
clMbCombine = clApply $(mkClosed [| mbCombine |])

-- | GADT version of the 'Liftable' class
data LiftableTp a where
  LiftableTp :: Liftable a => LiftableTp a

instance NuMatching Natural where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Natural where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i

instance NuMatching Word8 where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Word8 where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i
instance NuMatching Word16 where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Word16 where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i
instance NuMatching Word32 where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Word32 where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i
instance NuMatching Word64 where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Word64 where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i

instance NuMatching Int8 where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Int8 where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i
instance NuMatching Int16 where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Int16 where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i
instance NuMatching Int32 where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Int32 where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i
instance NuMatching Int64 where
  nuMatchingProof = isoMbTypeRepr toInteger fromInteger
instance Liftable Int64 where
  mbLift mb_i = fromInteger $ mbLift $ fmap toInteger mb_i


----------------------------------------------------------------------
-- * Utilities for Type Lists
----------------------------------------------------------------------

-- | A heterogeneous list containing one element of type @f a@ for each type @a@
-- in the given type list.
data MapList f (l :: [*]) where
  Nil :: MapList f '[]
  Cons :: f a -> MapList f l -> MapList f (a ': l)

-- | Get the first element of a 'MapList'
ml_first :: MapList f (a ': l) -> f a
ml_first (Cons x _) = x

-- | Get all but the the first element of a 'MapList'
ml_rest :: MapList f (a ': l) -> MapList f l
ml_rest (Cons _ l) = l

-- | Split the first from the remaining elements of a 'MapList'
ml_first_rest :: MapList f (a ': l) -> (f a, MapList f l)
ml_first_rest l = (ml_first l, ml_rest l)

-- | Get the first and second elements of a 'MapList'
ml_12 :: MapList f (a ': b ': l) -> (f a, f b)
ml_12 l = (ml_first l, ml_first $ ml_rest l)

-- | Get the first through third elements of a 'MapList'
ml_123 :: MapList f (a ': b ': c ': l) -> (f a, f b, f c)
ml_123 l = (ml_first l, ml_first $ ml_rest l, ml_first $ ml_rest $ ml_rest l)

-- | Get the first through fourth elements of a 'MapList'
ml_1234 :: MapList f (a ': b ': c ': d ': l) -> (f a, f b, f c, f d)
ml_1234 l = (ml_first l, ml_first $ ml_rest l, ml_first $ ml_rest $ ml_rest l,
             ml_first $ ml_rest $ ml_rest $ ml_rest l)

-- | Map a function over a 'MapList'
ml_map :: (forall x. f x -> g x) -> MapList f l -> MapList g l
ml_map _ Nil = Nil
ml_map f (Cons x l) = Cons (f x) (ml_map f l)

-- | Proof that type @a@ is an element of the list @l@ of types
data ElemPf (l :: [*]) a where
  Elem_base :: ElemPf (a ': l) a
  Elem_cons :: ElemPf l b -> ElemPf (a ': l) b

-- Make the NuMatching instance for ElemPf
$(mkNuMatching [t| forall l a. ElemPf l a |])

-- Make the Liftable instance for ElemPf
instance Liftable (ElemPf l a) where
  mbLift [nuP| Elem_base |] = Elem_base
  mbLift [nuP| Elem_cons pf |] = Elem_cons $ mbLift pf

-- | Type class for building 'ElemPf's
class Elem (l :: [*]) a where
  elemPf :: ElemPf l a

{-
instance {-# OVERLAPPING #-} Elem (a ': l) a where elemPf = Elem_base
instance Elem l b => Elem (a ': l) b where elemPf = Elem_cons elemPf
-}

instance {-# OVERLAPPING #-} Elem (a ': l) a where elemPf = Elem_base
instance ((a == b) ~ 'False, Elem l b) => Elem (a ': l) b where
  elemPf = Elem_cons elemPf

-- | Look up an element of a 'MapList' using an 'ElemPf'
ml_lookup :: MapList f l -> ElemPf l a -> f a
ml_lookup l Elem_base = ml_first l
ml_lookup l (Elem_cons pf) = ml_lookup (ml_rest l) pf

-- | Apply a function to a specific element of a 'MapList' given by an 'ElemPf'
ml_map1 :: (f a -> f a) -> MapList f l -> ElemPf l a -> MapList f l
ml_map1 f l Elem_base = Cons (f $ ml_first l) (ml_rest l)
ml_map1 f l (Elem_cons pf) = Cons (ml_first l) $ ml_map1 f (ml_rest l) pf


----------------------------------------------------------------------
-- * Helper Types Used by Our Logic
----------------------------------------------------------------------

-- | Container type for literals
newtype Literal a = Literal { unLiteral :: a }
                  deriving (Eq)

-- | Dummy type for propositions
data Prop deriving Typeable

-- | Untyped pointers = 'Integer's
newtype Ptr = Ptr { unPtr :: Integer } deriving (Typeable, Eq, Ord)

-- | The null pointer value
nullPtr :: Ptr
nullPtr = Ptr 0


----------------------------------------------------------------------
-- * The Literal Types
----------------------------------------------------------------------

-- | A GADT for the /literal types/ in our logic, which are the base types that
-- are isomorphic to their Haskell counterparts
data LitType a where
  LitType_unit :: LitType ()
  -- ^ The unit type
  LitType_bool :: LitType Bool
  -- ^ The type of Booleans that can be computed; i.e., not the type of
  -- formulas. For formulas (e.g., that contain quantifiers), use 'Prop'.
  LitType_int :: LitType Integer
  -- ^ Our logic has support for unbounded integers
  LitType_rat :: LitType Rational
  -- ^ Our logic also has support for arbitrary-precision rational numbers
  -- (which SMT solvers often call "real numbers")
  LitType_bits :: (Typeable a, Integral a, FiniteBits a, Liftable a) => LitType a
  -- ^ Any bit-vector type can be used as a literal type

-- Printing LitTypes
instance Show (LitType a) where
  show LitType_unit = "unit"
  show LitType_bool = "Bool"
  show LitType_int = "int"
  show LitType_rat = "rational"
  show (LitType_bits :: LitType bv) =
    (if isSigned (0 :: bv) then "sbits" else "ubits")
    ++ "[" ++ show (finiteBitSize (0 :: bv)) ++ "]"

-- | Typeclass for 'LitType'
class LitTypeable a where
  litTypeRep :: LitType a

instance LitTypeable () where litTypeRep = LitType_unit
instance LitTypeable Bool where litTypeRep = LitType_bool
instance LitTypeable Integer where litTypeRep = LitType_int
instance LitTypeable Rational where litTypeRep = LitType_rat
instance LitTypeable Int8 where litTypeRep = LitType_bits
instance LitTypeable Int16 where litTypeRep = LitType_bits
instance LitTypeable Int32 where litTypeRep = LitType_bits
instance LitTypeable Int64 where litTypeRep = LitType_bits
instance LitTypeable Word8 where litTypeRep = LitType_bits
instance LitTypeable Word16 where litTypeRep = LitType_bits
instance LitTypeable Word32 where litTypeRep = LitType_bits
instance LitTypeable Word64 where litTypeRep = LitType_bits
--instance (Typeable a, FiniteBits a) => LitTypeable a where
--  litTypeRep = LitType_bits

-- | Typeclass giving a default value for each 'LitType'
class LitTypeable a => LitDefault a where
  litDefault :: a

instance LitDefault () where litDefault = ()
instance LitDefault Bool where litDefault = True
instance LitDefault Integer where litDefault = 0
instance LitDefault Rational where litDefault = 0
instance LitDefault Word64 where litDefault = 0

-- | Build a default element of a literal type, given explicitly
litDefaultTp :: LitType a -> a
litDefaultTp LitType_unit = litDefault
litDefaultTp LitType_bool = litDefault
litDefaultTp LitType_int = litDefault
litDefaultTp LitType_rat = litDefault
litDefaultTp LitType_bits = 0

-- Build a NuMatching instance for LitType, needed for Liftable
$(mkNuMatching [t| forall a. LitType a |])

-- Liftable instance, to lift LitTypes out of binding contexts
instance Liftable (LitType a) where
  mbLift [nuP| LitType_unit |] = LitType_unit
  mbLift [nuP| LitType_bool |] = LitType_bool
  mbLift [nuP| LitType_int |] = LitType_int
  mbLift [nuP| LitType_rat |] = LitType_rat
  mbLift [nuP| LitType_bits |] = LitType_bits

-- | Build a 'Liftable' instance for any 'LitType'
litTypeLiftable :: LitType a -> LiftableTp a
litTypeLiftable LitType_unit = LiftableTp
litTypeLiftable LitType_bool = LiftableTp
litTypeLiftable LitType_int = LiftableTp
litTypeLiftable LitType_rat = LiftableTp
litTypeLiftable LitType_bits = LiftableTp

-- | Test if two 'LitType's are equal
litTypeEq :: LitType a -> LitType b -> Maybe (a :~: b)
litTypeEq LitType_unit LitType_unit = Just Refl
litTypeEq LitType_unit _ = Nothing
litTypeEq LitType_bool LitType_bool = Just Refl
litTypeEq LitType_bool _ = Nothing
litTypeEq LitType_int LitType_int = Just Refl
litTypeEq LitType_int _ = Nothing
litTypeEq LitType_rat LitType_rat = Just Refl
litTypeEq LitType_rat _ = Nothing
litTypeEq LitType_bits LitType_bits = eqT
litTypeEq LitType_bits _ = Nothing


----------------------------------------------------------------------
-- * The First-Order Types
----------------------------------------------------------------------

-- | A GADT for the /first-order types/ in our logic, which are the types that
-- are not function types or transition relations.
data L1Type a where
  L1Type_lit :: LitType a -> L1Type (Literal a)
  -- ^ The type @'Literal' a@ is a first-order type when @a@ is a literal type
  L1Type_ptr :: L1Type Ptr
  -- ^ The type of pointers, which are not literal types because, intuitively,
  -- we disallow pointer literals
  L1Type_prop :: L1Type Prop
  -- ^ The type of formulas that may or may not be decidable; e.g., formulas
  -- with quantifiers

-- Printing L1Types
instance Show (L1Type a) where
  show (L1Type_lit lit_tp) = "Literal " ++ show lit_tp
  show L1Type_ptr = "Ptr"
  show L1Type_prop = "Prop"

-- | Typeclass for 'L1Type'
class L1Typeable a where
  l1typeRep :: L1Type a

instance LitTypeable a => L1Typeable (Literal a) where
  l1typeRep = L1Type_lit litTypeRep
instance L1Typeable Ptr where l1typeRep = L1Type_ptr
instance L1Typeable Prop where l1typeRep = L1Type_prop

-- Build a NuMatching instance for L1Type, needed for Liftable
$(mkNuMatching [t| forall a. L1Type a |])

-- Liftable instance, to lift L1Types out of binding contexts
instance Liftable (L1Type a) where
  mbLift [nuP| L1Type_lit ltp |] = L1Type_lit $ mbLift ltp
  mbLift [nuP| L1Type_ptr |] = L1Type_ptr
  mbLift [nuP| L1Type_prop |] = L1Type_prop

-- | Test if two 'L1Type's are equal
l1TypeEq :: L1Type a -> L1Type b -> Maybe (a :~: b)
l1TypeEq (L1Type_lit ltp1) (L1Type_lit ltp2) =
  case litTypeEq ltp1 ltp2 of
    Just Refl ->  Just Refl
    _ -> Nothing
l1TypeEq (L1Type_lit _) _ = Nothing
l1TypeEq L1Type_ptr L1Type_ptr = Just Refl
l1TypeEq L1Type_ptr _ = Nothing
l1TypeEq L1Type_prop L1Type_prop = Just Refl
l1TypeEq L1Type_prop _ = Nothing

-- | "Proof" that there is no element of the type @'L1Type' (a -> b)@
no_functional_l1type :: L1Type (a -> b) -> c
no_functional_l1type l1tp = case l1tp of { }


----------------------------------------------------------------------
-- * The Types of Our Logic
----------------------------------------------------------------------

-- | A GADT for the types allowed in our logic, which include the first-order
-- types (also called base types) and the function types
data LType (a :: *) where
  LType_base :: L1Type a -> LType a
  -- ^ Any first-order type is a type
  LType_fun :: LType a -> LType b -> LType (a -> b)
  -- ^ Function types

-- | Typeclass for 'LType'
class LTypeable a where
  ltypeRep :: LType a

-- Leads to overlapping instances...
-- instance L1Typeable a => LTypeable a where ltypeRep = LType_base l1typeRep
instance LitTypeable a => LTypeable (Literal a) where
  ltypeRep = LType_base $ L1Type_lit litTypeRep
instance LTypeable Ptr where ltypeRep = LType_base $ l1typeRep
instance LTypeable Prop where ltypeRep = LType_base $ l1typeRep
instance (LTypeable a, LTypeable b) => LTypeable (a -> b) where
  ltypeRep = LType_fun ltypeRep ltypeRep

-- Build a NuMatching instance for LType, needed for Liftable
$(mkNuMatching [t| forall a. LType a |])

-- Liftable instance, to lift LTypes out of binding contexts
instance Liftable (LType a) where
  mbLift [nuP| LType_base l1tp |] = LType_base $ mbLift l1tp
  mbLift [nuP| LType_fun t1 t2 |] = LType_fun (mbLift t1) (mbLift t2)

-- | Test if two 'LType's are equal
ltypeEq :: LType a -> LType b -> Maybe (a :~: b)
ltypeEq (LType_base l1tp1) (LType_base l1tp2) =
  case l1TypeEq l1tp1 l1tp2 of
    Just Refl -> Just Refl
    _ -> Nothing
ltypeEq (LType_base _) _ = Nothing
ltypeEq (LType_fun t1 t1') (LType_fun t2 t2') =
  case (ltypeEq t1 t2, ltypeEq t1' t2') of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
ltypeEq (LType_fun _ _) _ = Nothing


----------------------------------------------------------------------
-- * Extracting the Argument and Return Types of a Function Type
----------------------------------------------------------------------

-- | Type family to add a list of types as arguments to a function return type
type family AddArrows (args :: [*]) (ret :: *) :: *
type instance AddArrows '[] ret = ret
type instance AddArrows (a ': args) ret = a -> AddArrows args ret

-- | Type family to extract the argument types from a function type. This is the
-- inverse of 'AddArrows', in that @'ArgTypes' ('AddArrows' args ret) = args@
-- and that @'AddArrows' ('ArgTypes' a) ('RetType' a) = a@ when all three of the
-- type functions involved are defined.
type family ArgTypes a :: [*]
type instance ArgTypes (Literal a) = '[]
type instance ArgTypes Ptr = '[]
type instance ArgTypes Prop = '[]
type instance ArgTypes (a -> b) = a ': ArgTypes b

-- | Type family to extract the return type from a function type; see 'ArgTypes'
type family RetType a :: *
type instance RetType (Literal a) = (Literal a)
type instance RetType Ptr = Ptr
type instance RetType Prop = Prop
type instance RetType (a -> b) = RetType b

-- | "Proof" that @a = AddArrows args ret@
data LTypeArgs a args ret where
  LTypeArgs_base :: L1Type a -> LTypeArgs a '[] a
  LTypeArgs_fun :: LType arg -> LTypeArgs a args ret ->
                   LTypeArgs (arg -> a) (arg ': args) ret

-- | Typeclass for generating 'LTypeArgs' proofs
class LTypeableArgs a args ret | a -> args ret , args ret -> a where
  ltypeArgsRep :: LTypeArgs a args ret

instance LitTypeable a => LTypeableArgs (Literal a) '[] (Literal a) where
  ltypeArgsRep = LTypeArgs_base l1typeRep
instance LTypeableArgs Ptr '[] Ptr where
  ltypeArgsRep = LTypeArgs_base l1typeRep
instance LTypeableArgs Prop '[] Prop where
  ltypeArgsRep = LTypeArgs_base l1typeRep
instance (LTypeable arg, LTypeableArgs a args ret) =>
         LTypeableArgs (arg -> a) (arg ': args) ret where
  ltypeArgsRep = LTypeArgs_fun ltypeRep ltypeArgsRep

-- | Generate an 'LTypeArgs' proof for a given type @a@
mkLTypeArgs :: LType a -> LTypeArgs a (ArgTypes a) (RetType a)
mkLTypeArgs (LType_base l1tp@(L1Type_lit _)) = LTypeArgs_base l1tp
mkLTypeArgs (LType_base L1Type_prop) = LTypeArgs_base l1typeRep
mkLTypeArgs (LType_base L1Type_ptr) = LTypeArgs_base l1typeRep
mkLTypeArgs (LType_fun tp_a tp_b) =
  LTypeArgs_fun tp_a (mkLTypeArgs tp_b)

-- | Convert an @'LTypeArgs' a args ret@ to an @'LType' a@
ltypeArgsFullType :: LTypeArgs a args ret -> LType a
ltypeArgsFullType (LTypeArgs_base l1tp) = LType_base l1tp
ltypeArgsFullType (LTypeArgs_fun tp tp_args) =
  LType_fun tp $ ltypeArgsFullType tp_args

-- | Convert an 'LTypeArgs' to an 'LType' for the return type
ltypeArgsRetType :: LTypeArgs a args ret -> LType ret
ltypeArgsRetType (LTypeArgs_base l1tp) = LType_base l1tp
ltypeArgsRetType (LTypeArgs_fun _ tp_args) = ltypeArgsRetType tp_args

-- | Proof that the return type of an 'LTypeArgs' is never a function type
no_functional_type_args_ret :: LTypeArgs a args (b -> c) -> d
no_functional_type_args_ret (LTypeArgs_base l1tp) = no_functional_l1type l1tp
no_functional_type_args_ret (LTypeArgs_fun _ tp_args) =
  no_functional_type_args_ret tp_args

-- Build a NuMatching instance for LTypeArgs, needed for Liftable
$(mkNuMatching [t| forall a args ret. LTypeArgs a args ret |])

instance Liftable (LTypeArgs a args ret) where
  mbLift [nuP| LTypeArgs_base l1tp |] = LTypeArgs_base $ mbLift l1tp
  mbLift [nuP| LTypeArgs_fun tp tp_args |] =
    LTypeArgs_fun (mbLift tp) (mbLift tp_args)


----------------------------------------------------------------------
-- * The Types of Functions over First-Order Types
----------------------------------------------------------------------

-- | Types for functions over first-order types
data L1FunType a where
  L1FunType_base :: L1Type a -> L1FunType a
  L1FunType_cons :: L1Type a -> L1FunType b -> L1FunType (a -> b)

-- | Typeclass for 'L1FunType'
class L1FunTypeable a where
  l1funTypeRep :: L1FunType a

instance LitTypeable a => L1FunTypeable (Literal a) where
  l1funTypeRep = L1FunType_base l1typeRep
instance L1FunTypeable Ptr where
  l1funTypeRep = L1FunType_base l1typeRep
instance L1FunTypeable Prop where
  l1funTypeRep = L1FunType_base l1typeRep
instance (L1Typeable a, L1FunTypeable b) => L1FunTypeable (a -> b) where
  l1funTypeRep = L1FunType_cons l1typeRep l1funTypeRep

$(mkNuMatching [t| forall a. L1FunType a |])

-- Liftable instance, to lift L1FunTypes out of binding contexts
instance Liftable (L1FunType a) where
  mbLift [nuP| L1FunType_base ltp |] = L1FunType_base $ mbLift ltp
  mbLift [nuP| L1FunType_cons ltp t |] = L1FunType_cons (mbLift ltp) (mbLift t)

-- | Convert an 'L1FunType' to an 'LType'
funType_to_type :: L1FunType a -> LType a
funType_to_type (L1FunType_base l1tp) = LType_base l1tp
funType_to_type (L1FunType_cons l1tp t) =
  LType_fun (LType_base l1tp) (funType_to_type t)

-- | Get the 'L1Type's for the argument types of an 'L1FunType'
funType_arg_types :: L1FunType a -> MapList L1Type (ArgTypes a)
funType_arg_types (L1FunType_base (L1Type_lit _)) = Nil
funType_arg_types (L1FunType_base L1Type_ptr) = Nil
funType_arg_types (L1FunType_base L1Type_prop) = Nil
funType_arg_types (L1FunType_cons l1tp ftp) =
  Cons l1tp (funType_arg_types ftp)

-- | Get the 'L1Type' for the output type of an 'L1FunType'
funType_ret_type :: L1FunType a -> L1Type (RetType a)
funType_ret_type (L1FunType_base l1tp@(L1Type_lit _)) = l1tp
funType_ret_type (L1FunType_base l1tp@L1Type_ptr) = l1tp
funType_ret_type (L1FunType_base l1tp@L1Type_prop) = l1tp
funType_ret_type (L1FunType_cons _ ftp) = funType_ret_type ftp

-- | The type of first-order contexts of types. This is just a helper type so
-- that we can build a 'NuMatching' instance for @'MapRList' 'L1FunType' ctx@
data L1Ctx ctx where
  L1CtxNil :: L1Ctx 'RNil
  L1CtxCons :: L1Ctx ctx -> L1FunType a -> L1Ctx (ctx ':> a)

$(mkNuMatching [t| forall ctx. L1Ctx ctx |])

instance Liftable (L1Ctx ctx) where
  mbLift [nuP| L1CtxNil |] = L1CtxNil
  mbLift [nuP| L1CtxCons ctx a |] = L1CtxCons (mbLift ctx) (mbLift a)

-- | Convert an 'L1Ctx' to a 'MapRList' of first-order function types
l1CtxToRList :: L1Ctx ctx -> MapRList L1FunType ctx
l1CtxToRList L1CtxNil = MNil
l1CtxToRList (L1CtxCons ctx a) = l1CtxToRList ctx :>: a

-- | Convert a 'MapRList' of first-order function types to an 'L1Ctx'
rListToL1Ctx :: MapRList L1FunType ctx -> L1Ctx ctx
rListToL1Ctx MNil = L1CtxNil
rListToL1Ctx (ctx :>: a) = L1CtxCons (rListToL1Ctx ctx) a

instance {-# OVERLAPPING #-} NuMatching (MapRList L1FunType ctx) where
  nuMatchingProof = isoMbTypeRepr rListToL1Ctx l1CtxToRList

instance Liftable (MapRList L1FunType ctx) where
  mbLift = l1CtxToRList . mbLift . fmap rListToL1Ctx


----------------------------------------------------------------------
-- * Finite Functions
----------------------------------------------------------------------

-- | Build the right-nested tuple type @(a1, (.., (an, ()) .. ))@ from the type
-- list @[a1, .., an]@
type family TupleType (as :: [*]) :: *
type instance TupleType '[] = ()
type instance TupleType (a ': as) = (a, TupleType as)

-- | A finite function, i.e., a function that has the same value for all but
-- finitely many inputs. These finite functions are un-Curried, i.e., they take
-- 0 or more arguments at once.
data FinFun (args :: [*]) b =
  FinFun { finfunDef :: b, finfunMap :: [(TupleType args, b)] }

-- | Make a finite function with a given default value
mkFinFun :: b -> FinFun args b
mkFinFun def = FinFun { finfunDef = def, finfunMap = [] }

-- | Apply a 'FinFun'
applyFinFun :: Eq (TupleType args) => FinFun args b -> TupleType args -> b
applyFinFun f args =
  case lookup args (finfunMap f) of
    Just ret -> ret
    Nothing -> finfunDef f

-- | Update a 'FinFun'
updateFinFun :: Eq (TupleType args) => FinFun args b -> TupleType args -> b ->
                FinFun args b
updateFinFun f args newval =
  FinFun { finfunDef = finfunDef f,
           finfunMap =
             (args, newval):(deleteBy
                             (\p1 p2 -> fst p1 == fst p2)
                             (args,newval)
                             (finfunMap f))}


----------------------------------------------------------------------
-- * The Memory Model of our Logic
----------------------------------------------------------------------

-- | Type class stating that @mm@ is a list of the types that are considered
-- storable, and also a list of default values for each of these types. Note
-- that, if type @a@ is in @mm@, then it is type @'Literal' a@ that is actually
-- storable. Additionally, the 'Ptr' type is always storable, and so is not
-- contained in the @mm@ list.
class MemoryModel (mm :: [*]) where
  memoryDefaults :: MapList Literal mm
  memoryLitTypes :: MapList LitType mm

instance MemoryModel '[] where
  memoryDefaults = Nil
  memoryLitTypes = Nil

instance (MemoryModel mm, LitDefault a) => MemoryModel (a ': mm) where
  memoryDefaults = Cons (Literal litDefault) memoryDefaults
  memoryLitTypes = Cons litTypeRep memoryLitTypes

-- | An /array store/ represents a set of (infinite) arrays of some given type,
-- while a /literal array store/ is an array store for some 'Literal'
-- type. Array stores are represented as 'FinFun's that map 'Ptr' pointer
-- values, combined with 'Word64' indices, to the given type.
newtype LitArrayStore a =
  LitArrayStore { unLitArrayStore ::
                    FinFun '[ Ptr, Literal Word64 ] (Literal a) }

-- | Build a default, empty 'LitArrayStore'
defaultLitArrayStore :: Literal a -> LitArrayStore a
defaultLitArrayStore = LitArrayStore . mkFinFun

-- | A memory is a collection of 'LitArrayStore's, one for each type in the
-- memory model, as well as an array store for 'Ptr's. The memory model is given
-- as a type-level list @mm@ of types @a@ such that @'Literal' a@ is considered
-- storable. Memories also associate a length with each 'Ptr', and additionally
-- track that last allocated 'Ptr' value: any 'Ptr' greater than the last
-- allocated one is not considered allcated, i.e., is an invalid pointer.
data Memory mm =
  Memory { memArrays :: MapList LitArrayStore mm,
           memPtrArray :: FinFun '[ Ptr, Literal Word64 ] Ptr,
           memLengths :: FinFun '[ Ptr ] (Literal Word64),
           memLastAlloc :: Ptr }

-- | Build a default, empty 'Memory'
defaultMemory :: MemoryModel mm => Memory mm
defaultMemory =
  Memory { memArrays = ml_map defaultLitArrayStore memoryDefaults,
           memPtrArray = mkFinFun (nullPtr),
           memLengths = mkFinFun (Literal 0),
           memLastAlloc = Ptr 0 }

-- FIXME HERE: remove ReadOp and UpdateOp types!

-- | The read operations for 'Memory's
data ReadOp mm args ret where
  -- | Read the nth element of an array whose type is in @mm@
  ReadOp_array :: ElemPf mm a -> ReadOp mm '[ Ptr, Literal Word64 ] (Literal a)
  -- | Read the nth element of the 'Ptr' array
  ReadOp_ptr_array :: ReadOp mm '[ Ptr, Literal Word64 ] Ptr
  -- | Get the length of an array as a 'Word64'
  ReadOp_length :: ReadOp mm '[ Ptr ] (Literal Word64)
  -- | Get the last-allocated pointer
  ReadOp_last_alloc :: ReadOp mm '[] Ptr

-- | The update operations for 'Memory's
data UpdateOp mm args where
  -- | Update the nth element of a 'LitArrayStore' whose type is in @mm@
  UpdateOp_array :: ElemPf mm a ->
                    UpdateOp mm '[ Ptr, Literal Word64, Literal a ]
  -- | Update the nth element of the 'Ptr' array store
  UpdateOp_ptr_array :: UpdateOp mm '[ Ptr, Literal Word64, Ptr ]
  -- | Copy the contents of one array to another, like a memcopy (NOTE: the
  -- first pointer is the source, the second is the destination)
  UpdateOp_ptr_copy :: UpdateOp mm '[ Ptr, Literal Word64, Ptr, Literal Word64 ]
  -- | Allocate a new array with a given length
  UpdateOp_alloc :: UpdateOp mm '[ Literal Word64 ]

-- | Perform a read generic operation on a 'Memory'
readMemory :: ReadOp mm args ret -> Memory mm -> MapList Identity args -> ret
readMemory (ReadOp_array elem_pf) mem (ml_12 -> (Identity ptr, Identity ix)) =
  applyFinFun
    (unLitArrayStore $ ml_lookup (memArrays mem) elem_pf)
    (ptr, (ix, ()))
readMemory ReadOp_ptr_array mem (ml_12 -> (Identity ptr, Identity ix)) =
  applyFinFun (memPtrArray mem) (ptr, (ix, ()))
readMemory ReadOp_length mem (ml_first -> Identity ptr) =
  applyFinFun (memLengths mem) (ptr, ())
readMemory ReadOp_last_alloc mem _ = memLastAlloc mem

-- | Read a 'Literal' value from an array in a 'Memory'
readMemoryLit :: Elem mm ret => Ptr -> Word64 -> Memory mm -> ret
readMemoryLit ptr ix mem =
  unLiteral $
  readMemory (ReadOp_array elemPf) mem $
  Cons (Identity ptr) (Cons (Identity (Literal ix)) Nil)

-- | Read a 'Ptr' value from an array in a 'Memory'
readMemoryPtr :: Ptr -> Word64 -> Memory mm -> Ptr
readMemoryPtr ptr ix mem =
  readMemory ReadOp_ptr_array mem $
  Cons (Identity ptr) (Cons (Identity (Literal ix)) Nil)

-- | Get the length of the array pointed to by a 'Ptr' in a 'Memory'
readMemoryLen :: Ptr -> Memory mm -> Word64
readMemoryLen ptr mem =
  unLiteral $ readMemory ReadOp_length mem (Cons (Identity ptr) Nil)

-- | Perform an update operation on a 'Memory'
updateMemory :: UpdateOp mm args -> Memory mm -> MapList Identity args ->
                Memory mm
updateMemory (UpdateOp_array elem_pf) mem (ml_123 ->
                                           (Identity ptr, Identity ix,
                                            Identity newval)) =
  mem
  { memArrays =
      ml_map1
        (\las -> LitArrayStore $
                 updateFinFun (unLitArrayStore las) (ptr, (ix, ())) newval)
        (memArrays mem)
        elem_pf
  }
updateMemory UpdateOp_ptr_array mem
             (Cons (Identity ptr)
              (Cons (Identity ix) (Cons (Identity newval) _))) =
  mem { memPtrArray = updateFinFun (memPtrArray mem) (ptr, (ix, ())) newval }
updateMemory UpdateOp_ptr_copy mem
             (Cons (Identity src_ptr)
              (Cons (Identity src_ix)
              (Cons (Identity dest_ptr)
               (Cons (Identity dest_ix) _)))) =
  error "FIXME HERE: write the updateMemory case for pointer copy!"
updateMemory UpdateOp_alloc mem
             (Cons (Identity new_len) _) =
  let new_last_alloc = Ptr (unPtr (memLastAlloc mem) + 1) in
  mem
  {
    memLengths = updateFinFun (memLengths mem) (new_last_alloc, ()) new_len,
    memLastAlloc = new_last_alloc
  }
updateMemory UpdateOp_alloc mem
             (Cons (Identity new_len) _) =
  let new_last_alloc = Ptr (unPtr (memLastAlloc mem) + 1) in
  mem
  {
    memLengths = updateFinFun (memLengths mem) (new_last_alloc, ()) new_len,
    memLastAlloc = new_last_alloc
  }


----------------------------------------------------------------------
-- * The Built-in Operations of our Logic
----------------------------------------------------------------------

-- | The unary arithmetic operations
data ArithOp1
  = Op1_Abs
    -- ^ Absolute value
  | Op1_Signum
    -- ^ The signum function
  | Op1_Neg
    -- ^ Integer negation, or the "not" function on Booleans
  | Op1_Complement
    -- ^ Bit complementation
  | Op1_Incr
    -- ^ Helper operation: increment by 1
  | Op1_Decr
    -- ^ Helper operation: decrement by 1

-- | The binary arithmetic operations
data ArithOp2
  = Op2_Add
    -- ^ Addition, or the "or" function on Booleans
  | Op2_Sub
    -- ^ Subtraction
  | Op2_Mult
    -- ^ Multiplication, or the "and" function on Booleans
  | Op2_Div
    -- ^ Division
  | Op2_Mod
    -- ^ The modulo function
  | Op2_BitAnd
    -- ^ Bitwise and
  | Op2_BitOr
    -- ^ Bitwise or
  | Op2_BitXor
    -- ^ Bitwise xor

-- | The arithmetic comparison operations
data ArithCmp
  = OpCmp_EQ -- ^ Equality
  | OpCmp_LT -- ^ Less than
  | OpCmp_LE -- ^ Less than or equal to

-- | The operations / function symbols of our logic
data Op a where
  -- | Literals that are lifted from Haskell
  Op_Literal :: Liftable a => LitType a -> a -> Op (Literal a)

  -- * First-order operations on data

  -- | Unary arithmetic
  Op_arith1 :: LitType a -> ArithOp1 -> Op (Literal a -> Literal a)
  -- | Binary arithmetic
  Op_arith2 :: LitType a -> ArithOp2 ->
               Op (Literal a -> Literal a -> Literal a)
  -- | Coercion between types
  Op_coerce :: LitType a -> LitType b -> Op (Literal a -> Literal b)
  -- | Comparison operations
  Op_cmp :: LitType a -> ArithCmp ->
            Op (Literal a -> Literal a -> Literal Bool)
  -- | The condition, i.e., if-then-else; note that it operates on Booleans, not
  -- propositions, as it cannot take in, e.g., forall formulas
  Op_cond :: L1Type a -> Op (Literal Bool -> a -> a -> a)

  -- | The null pointer
  Op_null_ptr :: Op Ptr
  -- | A global variable, referred to by reference, named by (natural) number
  Op_global_var :: Natural -> Op Ptr
  -- | Bump a free pointer
  Op_next_ptr :: Op (Ptr -> Ptr)
  -- | Pointer comparisons
  Op_ptr_cmp :: ArithCmp -> Op (Ptr -> Ptr -> Literal Bool)

  -- * Propositional operations
  -- | The true proposition
  Op_true :: Op Prop
  -- | The false proposition
  Op_false :: Op Prop
  -- | Logical and
  Op_and :: Op (Prop -> Prop -> Prop)
  -- | Logical or
  Op_or :: Op (Prop -> Prop -> Prop)
  -- | Logical negation
  Op_not :: Op (Prop -> Prop)
  -- | Equality at base type
  Op_eq :: L1Type a -> Op (a -> a -> Prop)
  -- | Lift a 'Bool' to a 'Prop'
  Op_istrue  :: Op (Literal Bool -> Prop)

  -- | Universal quantification
  Op_forall :: L1Type a -> Op ((a -> Prop) -> Prop)
  -- | Existential quantification
  Op_exists :: L1Type a -> Op ((a -> Prop) -> Prop)

  -- | Let-bindings, which are only allowed in propositions
  -- Op_let :: L1Type a -> Op (a -> (a -> Prop) -> Prop)

-- | Get the 'LType' for an 'Op'
opType :: Op a -> LType a
opType (Op_Literal lit_tp _) = LType_base $ L1Type_lit lit_tp
opType (Op_arith1 lit_tp _) =
  LType_fun (LType_base $ L1Type_lit lit_tp) (LType_base $ L1Type_lit lit_tp)
opType (Op_arith2 lit_tp _) =
  LType_fun (LType_base $ L1Type_lit lit_tp) $
  LType_fun (LType_base $ L1Type_lit lit_tp) (LType_base $ L1Type_lit lit_tp)
opType (Op_coerce lit_tp_from lit_tp_to) =
  LType_fun (LType_base $ L1Type_lit lit_tp_from)
  (LType_base $ L1Type_lit lit_tp_to)
opType (Op_cmp lit_tp _) =
  LType_fun (LType_base $ L1Type_lit lit_tp) $
  LType_fun (LType_base $ L1Type_lit lit_tp) ltypeRep
opType (Op_cond l1tp) =
  LType_fun ltypeRep $
  LType_fun (LType_base l1tp) $
  LType_fun (LType_base l1tp) $
  LType_base l1tp
opType Op_null_ptr = ltypeRep
opType (Op_global_var _) = ltypeRep
opType Op_next_ptr = ltypeRep
opType (Op_ptr_cmp _) = ltypeRep
opType Op_true = ltypeRep
opType Op_false = ltypeRep
opType Op_and = ltypeRep
opType Op_or = ltypeRep
opType Op_not = ltypeRep
opType (Op_eq l1tp) =
  LType_fun (LType_base $ l1tp) $ LType_fun (LType_base $ l1tp) ltypeRep
opType Op_istrue = ltypeRep
opType (Op_forall l1tp) =
  LType_fun (LType_fun (LType_base l1tp) ltypeRep) ltypeRep
opType (Op_exists l1tp) =
  LType_fun (LType_fun (LType_base l1tp) ltypeRep) ltypeRep
{-
opType (Op_let l1tp) =
  LType_fun (LType_base l1tp) $
  LType_fun (LType_fun (LType_base l1tp) ltypeRep) ltypeRep
-}

-- | Build an 'LTypeArgs' for the type arguments of an 'Op'
opTypeArgs :: Op a -> LTypeArgs a (ArgTypes a) (RetType a)
opTypeArgs op = mkLTypeArgs $ opType op

-- | Get the 'LType' for the return type of an 'Op'
opRetType :: Op a -> LType (RetType a)
opRetType op = ltypeArgsRetType $ opTypeArgs op

-- Build a NuMatching instances for Op and friends
$(mkNuMatching [t| ArithOp1 |])
$(mkNuMatching [t| ArithOp2 |])
$(mkNuMatching [t| ArithCmp |])
$(mkNuMatching [t| forall mm args ret. ReadOp mm args ret |])
$(mkNuMatching [t| forall mm args. UpdateOp mm args |])
$(mkNuMatching [t| forall a. Op a |])

-- Build Liftable instances for Op and friends
instance Liftable ArithOp1 where
  mbLift [nuP| Op1_Abs |] = Op1_Abs
  mbLift [nuP| Op1_Signum |] = Op1_Signum
  mbLift [nuP| Op1_Neg |] = Op1_Neg
  mbLift [nuP| Op1_Complement |] = Op1_Complement
  mbLift [nuP| Op1_Incr |] = Op1_Incr
  mbLift [nuP| Op1_Decr |] = Op1_Decr
instance Liftable ArithOp2 where
  mbLift [nuP| Op2_Add |] = Op2_Add
  mbLift [nuP| Op2_Sub |] = Op2_Sub
  mbLift [nuP| Op2_Mult |] = Op2_Mult
  mbLift [nuP| Op2_Div |] = Op2_Div
  mbLift [nuP| Op2_Mod |] = Op2_Mod
  mbLift [nuP| Op2_BitAnd |] = Op2_BitAnd
  mbLift [nuP| Op2_BitOr |] = Op2_BitOr
  mbLift [nuP| Op2_BitXor |] = Op2_BitXor
instance Liftable ArithCmp where
  mbLift [nuP| OpCmp_EQ |] = OpCmp_EQ
  mbLift [nuP| OpCmp_LT |] = OpCmp_LT
  mbLift [nuP| OpCmp_LE |] = OpCmp_LE
instance Liftable (ReadOp mm args ret) where
  mbLift [nuP| ReadOp_array elem_pf |] = ReadOp_array $ mbLift elem_pf
  mbLift [nuP| ReadOp_ptr_array |] = ReadOp_ptr_array
  mbLift [nuP| ReadOp_length |] = ReadOp_length
  mbLift [nuP| ReadOp_last_alloc |] = ReadOp_last_alloc
instance Liftable (UpdateOp mm args) where
  mbLift [nuP| UpdateOp_array elem_pf |] = UpdateOp_array $ mbLift elem_pf
  mbLift [nuP| UpdateOp_ptr_array |] = UpdateOp_ptr_array
  mbLift [nuP| UpdateOp_ptr_copy |] = UpdateOp_ptr_copy
  mbLift [nuP| UpdateOp_alloc |] = UpdateOp_alloc
instance Liftable (Op a) where
  mbLift [nuP| Op_Literal ltp x |] = Op_Literal (mbLift ltp) (mbLift x)
  mbLift [nuP| Op_arith1 ltp aop |] = Op_arith1 (mbLift ltp) (mbLift aop)
  mbLift [nuP| Op_arith2 ltp aop |] = Op_arith2 (mbLift ltp) (mbLift aop)
  mbLift [nuP| Op_coerce ltp1 ltp2 |] = Op_coerce (mbLift ltp1) (mbLift ltp2)
  mbLift [nuP| Op_cmp ltp acmp |] = Op_cmp (mbLift ltp) (mbLift acmp)
  mbLift [nuP| Op_cond l1tp |] = Op_cond (mbLift l1tp)
  mbLift [nuP| Op_null_ptr |] = Op_null_ptr
  mbLift [nuP| Op_global_var i |] = Op_global_var $ mbLift i
  mbLift [nuP| Op_next_ptr |] = Op_next_ptr
  mbLift [nuP| Op_ptr_cmp acmp |] = Op_ptr_cmp $ mbLift acmp
  mbLift [nuP| Op_true |] = Op_true
  mbLift [nuP| Op_false |] = Op_false
  mbLift [nuP| Op_and |] = Op_and
  mbLift [nuP| Op_or |] = Op_or
  mbLift [nuP| Op_not |] = Op_not
  mbLift [nuP| Op_eq l1tp |] = Op_eq $ mbLift l1tp
  mbLift [nuP| Op_istrue |] = Op_istrue
  mbLift [nuP| Op_forall l1tp |] = Op_forall $ mbLift l1tp
  mbLift [nuP| Op_exists l1tp |] = Op_exists $ mbLift l1tp
  --mbLift [nuP| Op_let tp |] = Op_let $ mbLift tp


----------------------------------------------------------------------
-- * The Expressions of Our Logic
----------------------------------------------------------------------

-- | The expressions of our logic as a GADT. This is essentially the typed
-- lambda-calculus with function symbols. All expressions are in beta-eta-normal
-- form, which is enforced by restricting the expressions to only the
-- lambda-abstractions and the full applications of variables and function
-- symbols to all of their arguments.
data LExpr a where
  LLambda :: LType a -> Binding a (LExpr b) -> LExpr (a -> b)
  LOp :: Op a -> LExprs (ArgTypes a) -> LExpr (RetType a)
  LVar :: LTypeArgs a args ret -> Name a -> LExprs args -> LExpr ret

-- | Lists of 'LExpr's
data LExprs as where
  LExprs_nil :: LExprs '[]
  LExprs_cons :: LExpr a -> LExprs as -> LExprs (a ': as)

$(mkNuMatching [t| forall a. LExpr a |])
$(mkNuMatching [t| forall as. LExprs as |])

-- | The type of propositions in our logic
type LProp = LExpr Prop

-- | Get the 'LType' of an expression-in-binding
mbExprType :: Mb ctx (LExpr a) -> LType a
mbExprType [nuP| LLambda tp_a body |] =
  LType_fun (mbLift tp_a) $ mbExprType $ mbCombine body
mbExprType [nuP| LOp op _ |] = opRetType $ mbLift op
mbExprType [nuP| LVar tp_args _ _ |] = ltypeArgsRetType $ mbLift tp_args


----------------------------------------------------------------------
-- * General Facilities for Building and Manipulating Expressions
----------------------------------------------------------------------

-- | Apply type function @f@ to the input and outputs of a function type, i.e.,
-- replace @a1 -> ... -> an -> b@ with @f a1 -> ... -> f an -> f b@.
type family ApplyToArgs (f :: * -> *) a :: *
type instance ApplyToArgs f (Literal a) = f (Literal a)
type instance ApplyToArgs f Ptr = f Ptr
type instance ApplyToArgs f Prop = f Prop
type instance ApplyToArgs f (a -> b) = f a -> ApplyToArgs f b

-- | Curry a @'LExprs' args -> 'LExpr' ret@ function
curryLExprsFun :: LType a ->
                  (LExprs (ArgTypes a) -> LExpr (RetType a)) ->
                  ApplyToArgs LExpr a
curryLExprsFun (LType_base (L1Type_lit _)) f = f LExprs_nil
curryLExprsFun (LType_base L1Type_prop) f = f LExprs_nil
curryLExprsFun (LType_base L1Type_ptr) f = f LExprs_nil
curryLExprsFun (LType_fun _ tp_b) f =
  \e -> curryLExprsFun tp_b (\es -> f (LExprs_cons e es))

-- | Build an expression for a variable applied to its arguments, given an
-- explicit type for the variable
mkVarAppTp :: LType a -> Name a -> ApplyToArgs LExpr a
mkVarAppTp (tp :: LType a) n =
  curryLExprsFun tp (LVar (mkLTypeArgs tp) n
                     :: LExprs (ArgTypes a) -> LExpr (RetType a))

-- | Build an expression for a variable applied to its arguments
mkVarApp :: LTypeable a => Name a -> ApplyToArgs LExpr a
mkVarApp n = mkVarAppTp ltypeRep n

-- | Build an expression from a variable of first-order type
mkVar1 :: L1Typeable a => Name a -> LExpr a
mkVar1 n = LVar (LTypeArgs_base l1typeRep) n LExprs_nil

-- | Explicitly-typed version of 'mkVar1'
mkVar1Tp :: L1Type a -> Name a -> LExpr a
mkVar1Tp l1tp n = LVar (LTypeArgs_base l1tp) n LExprs_nil

-- | Explicitly-typed version of 'mkVar1', for 'Literal' types
mkLitVarTp :: LitType a -> Name (Literal a) -> LExpr (Literal a)
mkLitVarTp lit_tp n = LVar (LTypeArgs_base (L1Type_lit lit_tp)) n LExprs_nil

-- | Make a variable into an expression, rather than a function
mkVarExprTp :: LType a -> Name a -> LExpr a
mkVarExprTp tp n = etaExpandLExprsFun tp (LVar (mkLTypeArgs tp) n)

-- | Build an expression from a variable of first-order function type
mkVarFOFunTp :: L1FunType a -> Name a -> LExpr a
mkVarFOFunTp ftp n = mkVarExprTp (funType_to_type ftp) n

-- | Helper function for building expression functions from 'Op's with an
-- explicit 'LType'
mkOp :: Op a -> ApplyToArgs LExpr a
mkOp op = curryLExprsFun (opType op) (LOp op)

-- | Helper function for building literal expressions
mkLiteralTp :: LitType a -> a -> LExpr (Literal a)
mkLiteralTp lit_tp a =
  case litTypeLiftable lit_tp of
    LiftableTp -> LOp (Op_Literal lit_tp a) LExprs_nil

-- | Helper function for building literal expressions
mkLiteral :: LitTypeable a => a -> LExpr (Literal a)
mkLiteral a = mkLiteralTp litTypeRep a

-- | Helper function for building literal 'Word64' expressions from any integral
mkLiteral64 :: Integral a => a -> LExpr (Literal Word64)
mkLiteral64 i = mkLiteral $ fromInteger $ toInteger i

-- | Helper function for building lambda-abstractions
mkLambda :: LTypeable a => (ApplyToArgs LExpr a -> LExpr b) -> LExpr (a -> b)
mkLambda (f :: _ -> LExpr b) =
  LLambda ltypeRep $ nu $ \x -> f (mkVarApp x)

-- | Helper function for building lambda-abstractions
mkLambdaTp :: LType a -> (LExpr a -> LExpr b) -> LExpr (a -> b)
mkLambdaTp tp_a (f :: LExpr a -> LExpr b) =
  LLambda tp_a $ nu $ \x -> f (mkVarExprTp tp_a x)

-- | Eta-expand a @'LExprs' args -> 'LExpr' ret@ function into an
-- expression with functional type
etaExpandLExprsFun :: LType a ->
                      (LExprs (ArgTypes a) -> LExpr (RetType a)) ->
                      LExpr a
etaExpandLExprsFun (LType_base (L1Type_lit _)) f = f LExprs_nil
etaExpandLExprsFun (LType_base L1Type_prop) f = f LExprs_nil
etaExpandLExprsFun (LType_base L1Type_ptr) f = f LExprs_nil
etaExpandLExprsFun (LType_fun tp_a tp_b) f =
  LLambda tp_a $ nu $ \n ->
  etaExpandLExprsFun tp_b (\es -> f (LExprs_cons (mkVarExprTp tp_a n) es))

-- FIXME: this requires Op_let to have an arbitrary RHS type;
-- OR: we have to do arbitrary substitution...
{-
-- | Apply an 'LExpr' to another
(@@) :: LExpr (a -> b) -> LExpr a -> LExpr b
f@(LLambda tp body) @@ arg = LAppExpr $ LApp (LApp (LOp $ Op_let tp) arg) f
(LAppExpr app_expr) @@ arg = LAppExpr $ LApp app_expr arg
-}

-- | Match the body of a lambda-expression
matchLambda :: LExpr (a -> b) -> Binding a (LExpr b)
matchLambda (LLambda _ body) = body
matchLambda (LVar tp_args _ _) = no_functional_type_args_ret tp_args
matchLambda (LOp op _) = no_functional_type_args_ret $ opTypeArgs op

-- | Same as 'matchLambda', but on an expression inside a binding
mbMatchLambda :: Mb ctx (LExpr (a -> b)) -> Mb (ctx ':> a) (LExpr b)
mbMatchLambda [nuP| LLambda _ body |] = mbCombine body
mbMatchLambda [nuP| LVar tp_args _ _ |] =
  no_functional_type_args_ret $ mbLift tp_args
mbMatchLambda [nuP| LOp op _ |] =
  no_functional_type_args_ret $ opTypeArgs $ mbLift op


----------------------------------------------------------------------
-- * Building Numeric, Boolean, and Pointer Expressions
----------------------------------------------------------------------

-- | Apply an 'ArithOp1' to an expression
mkArithOp1 :: LitType a -> ArithOp1 -> LExpr (Literal a) ->
              LExpr (Literal a)
mkArithOp1 lit_tp aop = mkOp (Op_arith1 lit_tp aop)

-- | Apply an 'ArithOp2' to an expression
mkArithOp2 :: LitType a -> ArithOp2 -> LExpr (Literal a) ->
              LExpr (Literal a) -> LExpr (Literal a)
mkArithOp2 lit_tp aop = mkOp (Op_arith2 lit_tp aop)

-- Num instance allows us to use arithmetic operations to build expressions.
instance (LitTypeable a, Num a) => Num (LExpr (Literal a)) where
  (+) = mkOp (Op_arith2 litTypeRep Op2_Add)
  (-) = mkOp (Op_arith2 litTypeRep Op2_Sub)
  (*) = mkOp (Op_arith2 litTypeRep Op2_Mult)
  abs = mkOp (Op_arith1 litTypeRep Op1_Abs)
  signum = mkOp (Op_arith1 litTypeRep Op1_Signum)
  fromInteger i = mkLiteral (fromInteger i)

-- | Build a negation expression
mkNeg :: LitTypeable a => LExpr (Literal a) -> LExpr (Literal a)
mkNeg e = mkOp (Op_arith1 litTypeRep Op1_Neg) e

-- | Smart constructor for building coercions
mkCoerce :: LitType a -> LitType b -> LExpr (Literal a) ->
            LExpr (Literal b)
mkCoerce lit_tp_from lit_tp_to expr
  | Just Refl <- litTypeEq lit_tp_from lit_tp_to = expr
mkCoerce lit_tp_from lit_tp_to expr =
  mkOp (Op_coerce lit_tp_from lit_tp_to) expr

-- | Build a null pointer expression
mkNullPtr :: LExpr Ptr
mkNullPtr = mkOp Op_null_ptr

-- | Build a global variable expression
mkGlobalVar :: Natural -> LExpr Ptr
mkGlobalVar n = mkOp (Op_global_var n)

-- | Make a Boolean less-than expression over a 'Literal' type
mkLtBool :: LitTypeable a => LExpr (Literal a) -> LExpr (Literal a) ->
            LExpr (Literal Bool)
mkLtBool e1 e2 = mkOp (Op_cmp litTypeRep OpCmp_LT) e1 e2

-- | Make a Boolean less-than-or-equal expression over a 'Literal' type
mkLeBool :: LitTypeable a => LExpr (Literal a) -> LExpr (Literal a) ->
            LExpr (Literal Bool)
mkLeBool e1 e2 = mkOp (Op_cmp litTypeRep OpCmp_LE) e1 e2

-- | Make a Boolean comparison at a first-order type, raising an error if that
-- first-order type is 'Prop'
mkArithCmpTp :: L1Type a -> ArithCmp -> LExpr a -> LExpr a ->
                LExpr (Literal Bool)
mkArithCmpTp (L1Type_lit lit_tp) acmp e1 e2 =
  mkOp (Op_cmp lit_tp acmp) e1 e2
mkArithCmpTp L1Type_ptr acmp e1 e2 =
  mkOp (Op_ptr_cmp acmp) e1 e2
mkArithCmpTp L1Type_prop _ _ _ =
  error "mkArithCmpTp: arithmetic comparison of propositions!"

-- | Negate a Boolean
mkNotBool :: LExpr (Literal Bool) -> LExpr (Literal Bool)
mkNotBool = mkOp (Op_arith1 LitType_bool Op1_Neg)

-- | Conjoin two Booleans: represented using '*'
mkAndBool :: LExpr (Literal Bool) -> LExpr (Literal Bool) ->
             LExpr (Literal Bool)
mkAndBool = mkOp (Op_arith2 LitType_bool Op2_Mult)

-- | Disjoin two Booleans: represented using '+'
mkOrBool :: LExpr (Literal Bool) -> LExpr (Literal Bool) ->
             LExpr (Literal Bool)
mkOrBool = mkOp (Op_arith2 LitType_bool Op2_Add)

-- | Build a conditional expression
mkCond :: L1Type a -> LExpr (Literal Bool) -> LExpr a -> LExpr a ->
          LExpr a
mkCond l1tp@(L1Type_lit _) = mkOp (Op_cond l1tp)
mkCond l1tp@L1Type_ptr = mkOp (Op_cond l1tp)
mkCond l1tp@L1Type_prop = mkOp (Op_cond l1tp)


----------------------------------------------------------------------
-- * Building Proposition Expressions
----------------------------------------------------------------------

-- | Get the head of an 'LExprs' list
lexprs_head :: LExprs (a ': as) -> LExpr a
lexprs_head (LExprs_cons e _) = e

-- | Get the tail of an 'LExprs' list
lexprs_tail :: LExprs (a ': as) -> LExprs as
lexprs_tail (LExprs_cons _ es) = es

-- | Get the first and second arguments of an 'LExprs' list
lexprs_12 :: LExprs (a ': b ': any) -> (LExpr a, LExpr b)
lexprs_12 es = (lexprs_head es, lexprs_head $ lexprs_tail es)

-- | Check if an expression is a literal, and if so, return the literal value
matchLiteral :: LExpr (Literal a) -> Maybe a
matchLiteral (LOp (Op_Literal _ x) _) = Just x
matchLiteral _ = Nothing

-- | Check if a proposition is the "true" proposition
isTrue :: LProp -> Bool
isTrue (LOp Op_true _) = True
isTrue _ = False

-- | Check if a proposition is the "false" proposition
isFalse :: LProp -> Bool
isFalse (LOp Op_false _) = True
isFalse _ = False

-- | Destructure a proposition into all of its (recursive) conjuncts
getConjuncts :: LProp -> [LProp]
getConjuncts (LOp Op_and (lexprs_12 -> (p1, p2))) =
  getConjuncts p1 ++ getConjuncts p2
getConjuncts p = [p]

-- | Destructure a proposition into all of its (recursive) disjuncts
getDisjuncts :: LProp -> [LProp]
getDisjuncts (LOp Op_or (lexprs_12 -> (p1, p2))) =
  getDisjuncts p1 ++ getDisjuncts p2
getDisjuncts p = [p]

-- | Check if a proposition is a conjunction, and, if so, return its conjuncts.
-- This is done recursively, so any sub-conjuncts are destructured as well.
matchAnd :: LProp -> Maybe [LProp]
matchAnd (LOp Op_and (lexprs_12 -> (p1, p2))) =
  Just (getConjuncts p1 ++ getConjuncts p2)
matchAnd _ = Nothing

-- | Check if a proposition is a disjunction, and, if so, return its disjuncts.
-- This is done recursively, so any sub-disjuncts are destructured as well.
matchOr :: LProp -> Maybe [LProp]
matchOr (LOp Op_or (lexprs_12 -> (p1, p2))) =
  Just (getDisjuncts p1 ++ getDisjuncts p2)
matchOr _ = Nothing

-- | The true proposition
mkTrue :: LProp
mkTrue = mkOp Op_true

-- | The false proposition
mkFalse :: LProp
mkFalse = mkOp Op_false

-- | Negate a proposition (FIXME: handle quantifiers)
mkNot :: LProp -> LProp
mkNot p | not doSimplifications = mkOp Op_not p
mkNot p | isTrue p = mkFalse
mkNot p | isFalse p = mkTrue
mkNot (matchAnd -> Just ps) = mkOr $ map mkNot ps
mkNot (matchOr -> Just ps) = mkAnd $ map mkNot ps
mkNot p = mkOp Op_not p

-- | Build a right-nested conjunction, removing any "true"s and returning
-- "false" if any conjuncts are "false"
mkAnd :: [LProp] -> LProp
mkAnd ps | not doSimplifications = foldl' (mkOp Op_and) mkTrue ps
mkAnd ps =
  let ps1 = concatMap getConjuncts ps
      ps2 = filter (not . isTrue) ps1 in
  if any isFalse ps2 then mkFalse else helper ps2 where
    helper [] = mkTrue
    helper [p] = p
    helper (p : ps') = mkOp Op_and p $ helper ps'

-- | Build a right-nested disjunction, removing any "true"s and returning
-- "false" if any disjuncts are "false"
mkOr :: [LProp] -> LProp
mkOr ps | not doSimplifications = foldl' (mkOp Op_or) mkFalse ps
mkOr ps =
  let ps1 = concatMap getDisjuncts ps
      ps2 = filter (not . isFalse) ps1 in
  if any isTrue ps2 then mkTrue else helper ps2 where
    helper [] = mkFalse
    helper [p] = p
    helper (p : ps') = mkOp Op_or p $ helper ps'

-- | Build an expression stating that the given Boolean is 'True' (FIXME: make
-- this "smart" by recognizing and, or, and not)
mkIsTrue :: LExpr (Literal Bool) -> LProp
mkIsTrue = mkOp Op_istrue

-- | Build a less-than expression as a 'Prop'
mkLt :: LitTypeable a => LExpr (Literal a) -> LExpr (Literal a) ->
        LExpr Prop
mkLt e1 e2 = mkIsTrue $ mkLtBool e1 e2

-- | Build a less-than-or-equal expression as a 'Prop'
mkLe :: LitTypeable a => LExpr (Literal a) -> LExpr (Literal a) ->
        LExpr Prop
mkLe e1 e2 = mkIsTrue $ mkLeBool e1 e2

-- | Build a universal quantifier into an 'LProp'
mkForall :: L1Typeable a => (LExpr a -> LProp) -> LProp
mkForall body =
  mkOp (Op_forall l1typeRep) $ mkLambdaTp (LType_base l1typeRep) body

-- | Explicitly-typed version of 'mkForall'
mkForallTp :: L1Type a -> (LExpr a -> LProp) -> LProp
mkForallTp l1tp body =
  mkOp (Op_forall l1tp) $ mkLambdaTp (LType_base l1tp) body

-- | Build an existential quantifier into an 'LProp'
mkExists :: L1Typeable a => (LExpr a -> LProp) -> LProp
mkExists body =
  mkOp (Op_exists l1typeRep) $ mkLambdaTp (LType_base l1typeRep) body

-- | Explicitly-typed version of 'mkExists'
mkExistsTp :: L1Type a -> (LExpr a -> LProp) -> LProp
mkExistsTp l1tp body =
  mkOp (Op_exists l1tp) $ mkLambdaTp (LType_base l1tp) body

-- | Build a let-binding into an 'LProp'
{-
mkLet :: L1Type a -> LExpr a -> (LExpr a -> LProp) -> LProp
mkLet l1tp rhs body =
  mkOp (Op_let l1tp) rhs $ mkLambdaTp (LType_base l1tp) body
-}

-- | Build an equality at a first-order type which is given explicitly.  Does
-- some simplification.
mkEq1Tp :: L1Type a -> LExpr a -> LExpr a -> LProp
mkEq1Tp l1tp e1 e2
  | not doSimplifications
  = LOp (Op_eq l1tp) (LExprs_cons e1 $ LExprs_cons e2 $ LExprs_nil)
mkEq1Tp (L1Type_lit LitType_unit) e1 e2 =
  -- Elements of the unit type are always equal
  mkTrue
mkEq1Tp (L1Type_lit LitType_bool) (matchLiteral -> Just x1)
  (matchLiteral -> Just x2) =
  if x1 == x2 then mkTrue else mkFalse
mkEq1Tp (L1Type_lit LitType_int) (matchLiteral -> Just x1)
  (matchLiteral -> Just x2) =
  if x1 == x2 then mkTrue else mkFalse
mkEq1Tp (L1Type_lit LitType_rat) (matchLiteral -> Just x1)
  (matchLiteral -> Just x2) =
  if x1 == x2 then mkTrue else mkFalse
mkEq1Tp (L1Type_lit LitType_bits) (matchLiteral -> Just x1)
  (matchLiteral -> Just x2) =
  if x1 == x2 then mkTrue else mkFalse
mkEq1Tp l1tp e1 e2 =
  LOp (Op_eq l1tp) (LExprs_cons e1 $ LExprs_cons e2 $ LExprs_nil)

-- | Build an equality at an arbitrary second-order type
mkEqTp :: L1FunType a -> ApplyToArgs LExpr a ->
          ApplyToArgs LExpr a -> LProp
mkEqTp (L1FunType_base l1tp@(L1Type_lit _)) e1 e2 =
  mkEq1Tp l1tp e1 e2
mkEqTp (L1FunType_base l1tp@L1Type_ptr) e1 e2 =
  mkEq1Tp l1tp e1 e2
mkEqTp (L1FunType_base l1tp@L1Type_prop) e1 e2 =
  mkEq1Tp l1tp e1 e2
mkEqTp (L1FunType_cons l1tp_a tp_b) f1 f2 =
  mkForallTp l1tp_a $ \x -> mkEqTp tp_b (f1 x) (f2 x)

-- | Build an equality at a first-order type
mkEq1 :: L1Typeable a => LExpr a -> LExpr a -> LProp
mkEq1 e1 e2 = mkEq1Tp l1typeRep e1 e2

-- | Build an equality for two 'Name's, short-circuiting the equality for
-- syntactically identical names
mkNameEq :: L1FunType a -> Name a -> Name a -> LProp
mkNameEq ftp n1 n2 =
  if n1 == n2 && doSimplifications then mkTrue else
    mkEqTp ftp (mkVarAppTp (funType_to_type ftp) n1)
    (mkVarAppTp (funType_to_type ftp) n2)


----------------------------------------------------------------------
-- * Eliminating Expressions via Expression Algebras
----------------------------------------------------------------------

-- | Type class stating that @f@ commutes with the arrow type constructor
class CommutesWithArrow f where
  interpApply :: f (a -> b) -> f a -> f b
  interpLambda :: LType a -> (f a -> f b) -> f (a -> b)

-- | An expression @f@-algebra shows how to convert any 'Op' of type @a@ to an
-- element of type @f a@. It also requires that @f@ commutes with arrow.
class CommutesWithArrow f => LExprAlgebra (f :: * -> *) where
  interpOp :: Op a -> f a

-- | Interpret an 'LExpr' to another functor @f@ using an @f@-algebra
interpExpr :: LExprAlgebra f => MapRList f ctx -> Mb ctx (LExpr a) -> f a
interpExpr ctx [nuP| LLambda ltp body |] =
  interpLambda (mbLift ltp) $ \x ->
  interpExpr (ctx :>: x) (mbCombine body)
interpExpr ctx [nuP| LVar tp_args n args |] =
  case mbNameBoundP n of
    Left memb ->
      interpExprsApply ctx (mapRListLookup memb ctx) (mbLift tp_args) args
    Right _ -> error "interpExpr: unbound name!"
interpExpr ctx [nuP| LOp mb_op args |] =
  let op = mbLift mb_op in
  interpExprsApply ctx (interpOp op) (opTypeArgs op) args

-- | Helper for 'interpExpr'
interpExprsApply :: LExprAlgebra f => MapRList f ctx ->
                    f a -> LTypeArgs a args ret ->
                    Mb ctx (LExprs args) -> f ret
interpExprsApply _ f (LTypeArgs_base _) _ = f
interpExprsApply ctx f (LTypeArgs_fun _ tp_args) [nuP| LExprs_cons e es |] =
  interpExprsApply ctx (interpApply f (interpExpr ctx e)) tp_args es

instance CommutesWithArrow LExpr where
  interpApply e1 e2 = substExpr (MNil :>: e2) (matchLambda e1)
  interpLambda ltp f = mkLambdaTp ltp f

-- LExpr itself forms an @f@-algebra, useful for substitution
instance LExprAlgebra LExpr where
  interpOp op = etaExpandLExprsFun (opType op) (LOp op)

-- | Substitute into an 'LExpr'
substExpr :: MapRList LExpr ctx -> Mb ctx (LExpr a) -> LExpr a
substExpr = interpExpr


----------------------------------------------------------------------
-- * Eliminating Expressions via Expression Binding-Algebras
----------------------------------------------------------------------

-- | Form the type @f (ctx :> a1 :> ... :> an) b@ for @a = a1 -> ... -> an -> b@
type family BindingApplyF (f :: RList * -> * -> *) (ctx :: RList *) a :: *
type instance BindingApplyF f ctx (Literal a) = f ctx (Literal a)
type instance BindingApplyF f ctx Ptr = f ctx Ptr
type instance BindingApplyF f ctx Prop = f ctx Prop
type instance BindingApplyF f ctx (a -> b) = BindingApplyF f (ctx ':> a) b

-- | Helper for building a 'BindingApplyF' at a return type of an 'Op' or
-- variable. This is just the identity function, but it needs to examine the
-- 'LTypeArgs' to convince Haskell.
mkRetBindingApplyF :: LTypeArgs a args ret -> f ctx ret -> BindingApplyF f ctx ret
mkRetBindingApplyF (LTypeArgs_base (L1Type_lit _)) x = x
mkRetBindingApplyF (LTypeArgs_base L1Type_ptr) x = x
mkRetBindingApplyF (LTypeArgs_base L1Type_prop) x = x
mkRetBindingApplyF (LTypeArgs_fun _ tp_args) x =
  mkRetBindingApplyF tp_args x

-- | Helper for eliminating a 'BindingApplyF' at base type. This is just the
-- identity function, but it needs to examine the type to convince Haskell.
elimL1BindingApplyF :: L1Type a -> BindingApplyF f ctx a -> f ctx a
elimL1BindingApplyF (L1Type_lit _) x = x
elimL1BindingApplyF L1Type_ptr x = x
elimL1BindingApplyF L1Type_prop x = x

-- | The type associated with the type family 'BindingApplyF'
newtype BindingApply f ctx a =
  BindingApply { unBindingApply :: BindingApplyF f ctx a }

-- | Type-class for interpreting expressions using expression binding-algebras
class LBindingExprAlgebra (f :: RList * -> * -> *) where
  interpOpB :: Op a ->
               MapList (BindingApply f ctx) (ArgTypes a) ->
               f ctx (RetType a)
  interpVarB :: LTypeArgs a args ret -> Member ctx a ->
                MapList (BindingApply f ctx) args ->
                f ctx ret

-- | Interpret an expression using a binding-algebra
interpMbExprB :: LBindingExprAlgebra f => Proxy f ->
                 Mb ctx (LExpr a) -> BindingApplyF f ctx a
interpMbExprB proxy [nuP| LLambda _ body |] =
  interpMbExprB proxy $ mbCombine body
interpMbExprB proxy [nuP| LOp mb_op args |] =
  let op = mbLift mb_op in
  mkRetBindingApplyF (opTypeArgs op) $
  interpOpB op $ interpMbExprsB proxy args
interpMbExprB proxy ([nuP| LVar mb_tp_args n args |] :: Mb ctx (LExpr a)) =
  let tp_args = mbLift mb_tp_args in
  case mbNameBoundP n of
    Left memb ->
      mkRetBindingApplyF tp_args $
      interpVarB tp_args memb $
      interpMbExprsB proxy args
    Right _ -> error "interpMbExprB: unbound name!"

-- | Interpret a list of 'LExprs' using a binding-algebra
interpMbExprsB :: LBindingExprAlgebra f =>
                  Proxy f -> Mb ctx (LExprs args) ->
                  MapList (BindingApply f ctx) args
interpMbExprsB _ [nuP| LExprs_nil |] = Nil
interpMbExprsB proxy [nuP| LExprs_cons e es |] =
  Cons (BindingApply $ interpMbExprB proxy e) (interpMbExprsB proxy es)

-- | Top-level function for interpreting expressions via binding-algebras
interpExprB :: LBindingExprAlgebra f => Proxy f ->
               LExpr a -> BindingApplyF f 'RNil a
interpExprB proxy e = interpMbExprB proxy $ emptyMb e


----------------------------------------------------------------------
-- * Annotated Expressions
----------------------------------------------------------------------

-- | The type of expressions-in-context where every subterm of type @a@ in
-- binding context @ctx@ is annotated with a value of type @f ctx a@
data MbAnnotExpr (f :: RList * -> * -> *) (ctx :: RList *) a where
  MbAnnotExprFun :: MbAnnotExpr f (ctx ':> a) b ->
                    MbAnnotExpr f ctx (a -> b)
  MbAnnotExprOp :: Op a ->
                   MapList (MbAnnotExpr f ctx) args ->
                   f ctx (RetType a) ->
                   MbAnnotExpr f ctx (RetType a)
  MbAnnotExprVar :: LTypeArgs a args ret -> Member ctx a ->
                    MapList (MbAnnotExpr f ctx) args ->
                    f ctx ret ->
                    MbAnnotExpr f ctx ret

-- | Extract the @f ctx a@ from an 'MbAnnotExpr'
elimMbAnnotExpr :: MbAnnotExpr f ctx a -> BindingApplyF f ctx a
elimMbAnnotExpr (MbAnnotExprFun annot_expr) =
  elimMbAnnotExpr annot_expr
elimMbAnnotExpr (MbAnnotExprOp op _ x) =
  mkRetBindingApplyF (opTypeArgs op) x
elimMbAnnotExpr (MbAnnotExprVar tp_args _ _ x) =
  mkRetBindingApplyF tp_args x

-- | Convert a @'BindingApplyF' ('MbAnnotExpr' f ) ctx a@ to an
-- @'MbAnnotExpr' f ctx@
mkBindingMbAnnotExpr :: LType a -> BindingApplyF (MbAnnotExpr f) ctx a ->
                        MbAnnotExpr f ctx a
mkBindingMbAnnotExpr (LType_base l1tp) annot_expr =
  elimL1BindingApplyF l1tp annot_expr
mkBindingMbAnnotExpr (LType_fun _ tp2) x =
  MbAnnotExprFun $ mkBindingMbAnnotExpr tp2 x

-- | Convert a list of @'BindingApply' ('MbAnnotExpr' f) ctx a@ to a list of
-- @'MbAnnotExpr' f ctx@
mkBindingMbAnnotExprs :: LTypeArgs a args ret ->
                         MapList (BindingApply (MbAnnotExpr f) ctx) args ->
                         MapList (MbAnnotExpr f ctx) args
mkBindingMbAnnotExprs (LTypeArgs_fun tp tp_args) (ml_first_rest ->
                                                  (BindingApply arg, args)) =
  Cons (mkBindingMbAnnotExpr tp arg) (mkBindingMbAnnotExprs tp_args args)
mkBindingMbAnnotExprs (LTypeArgs_base _) _ = Nil

-- This instance lets us take any binding-algebra and use it to annotate an
-- expression, saving all the intermediate results at each subterm
instance LBindingExprAlgebra f =>
         LBindingExprAlgebra (MbAnnotExpr f) where
  interpOpB op binding_args =
    let tp_args = opTypeArgs op in
    let args = mkBindingMbAnnotExprs tp_args binding_args in
    MbAnnotExprOp op args $
    interpOpB op (ml_map (BindingApply . elimMbAnnotExpr) args)
  interpVarB tp_args memb binding_args =
    let args = mkBindingMbAnnotExprs tp_args binding_args in
    MbAnnotExprVar tp_args memb args $
    interpVarB tp_args memb $
    ml_map (BindingApply . elimMbAnnotExpr) args

-- | Top-level annotation function
annotateExpr :: (LBindingExprAlgebra f, LTypeable a) => Proxy f ->
                LExpr a -> MbAnnotExpr f 'RNil a
annotateExpr (_ :: Proxy f) (expr :: LExpr a) =
  mkBindingMbAnnotExpr ltypeRep $
  interpExprB (Proxy :: Proxy (MbAnnotExpr f)) expr


{- FIXME: Update or remove the following stuff about contextual algebras

----------------------------------------------------------------------
-- * Expression Interpretations that Use the Context
----------------------------------------------------------------------

-- | A @'CtxExt' ctx1 ctx2@ is a proof that @ctx2@ is an /extension/ of @ctx1@,
-- meaning that the former is the result of inserting zero or more types into
-- the latter, preserving the order of the remaining types.
data CtxExt ctx1 ctx2 where
  CtxExt_nil :: CtxExt RNil RNil
  -- ^ Proof that the empty context extends the empty context
  CtxExt_cons :: CtxExt ctx1 ctx2 -> CtxExt (ctx1 :> a) (ctx2 :> a)
  -- ^ Proof that context extension is preserved by adding a type to both sides
  CtxExt_insert :: CtxExt ctx1 ctx2 -> CtxExt ctx1 (ctx2 :> a)
  -- ^ Proof step that inserts a type into the extended context

-- | Build a proof that any context is an extension of itself from a proof that
-- that context is an extension of some other context
ctxExtRefl_right :: CtxExt ctx1 ctx2 -> CtxExt ctx2 ctx2
ctxExtRefl_right CtxExt_nil = CtxExt_nil
ctxExtRefl_right (CtxExt_cons ext) = CtxExt_cons $ ctxExtRefl_right ext
ctxExtRefl_right (CtxExt_insert ext) = CtxExt_cons $ ctxExtRefl_right ext

-- | The type of some 'CtxExt' proof for an extension of a given context, i.e.,
-- existential quantification over the second type argument of 'CtxExt'
data SomeCtxExt ctx1 where
  SomeCtxExt :: CtxExt ctx1 ctx2 -> SomeCtxExt ctx1

-- | Take a 'CtxExt' proof that @ctx1 :> a@ is extended by @ctx2@ and prove that
-- @ctx1@ itself is extended by @ctx2@
ctxExtSnocLeft :: CtxExt (ctx1 :> a) ctx2 -> CtxExt ctx1 ctx2
ctxExtSnocLeft (CtxExt_cons ctx_ext) = CtxExt_insert ctx_ext
ctxExtSnocLeft (CtxExt_insert ctx_ext) = CtxExt_insert $ ctxExtSnocLeft ctx_ext

-- | Map a 'Member' proof to an extended context
ctxExtMember :: CtxExt ctx1 ctx2 -> Member ctx1 a -> Member ctx2 a
ctxExtMember CtxExt_nil memb = memb -- Technically, this is impossible...
ctxExtMember (CtxExt_insert ext) memb = Member_Step $ ctxExtMember ext memb
ctxExtMember (CtxExt_cons ext) Member_Base = Member_Base
ctxExtMember (CtxExt_cons ext) (Member_Step memb) =
  Member_Step $ ctxExtMember ext memb

-- | Append two 'CtxExt' proofs
ctxExtAppend :: CtxExt ctx1 ctx2 -> CtxExt ctx2 ctx3 -> CtxExt ctx1 ctx3
ctxExtAppend CtxExt_nil ext2 = ext2
ctxExtAppend ext1 (CtxExt_insert ext2) = CtxExt_insert $ ctxExtAppend ext1 ext2
ctxExtAppend (CtxExt_cons ext1) (CtxExt_cons ext2) =
  CtxExt_cons $ ctxExtAppend ext1 ext2
ctxExtAppend (CtxExt_insert ext1) (CtxExt_cons ext2) =
  CtxExt_insert $ ctxExtAppend ext1 ext2

-- | The type of objects that are defined for all extensions of a given context
newtype InExtCtx f ctx a =
  InExtCtx { runInExtCtx :: forall ctx'. CtxExt ctx ctx' -> f ctx' a }

-- | Lower an 'InExtCtx' into a context extended by a single variable type
lowerInExtCtx1 :: InExtCtx f ctx b -> InExtCtx f (ctx :> a) b
lowerInExtCtx1 inExtCtx =
  InExtCtx $ \ctx_ext -> runInExtCtx inExtCtx (ctxExtSnocLeft ctx_ext)

-- | Use a 'CtxExt' proof to lower the context of an 'InExtCtx'
extendInExtCtx :: CtxExt ctx1 ctx2 -> InExtCtx f ctx1 a -> InExtCtx f ctx2 a
extendInExtCtx ctx_ext inExtCtx =
  InExtCtx $ \ctx_ext' -> runInExtCtx inExtCtx (ctxExtAppend ctx_ext ctx_ext')

-- | Map the body of an 'InExtCtx's with a context-polymorphic function
mapInExtCtx :: (forall ctx'. f ctx' a -> g ctx' b) ->
               InExtCtx f ctx a -> InExtCtx g ctx b
mapInExtCtx f inExtCtx =
  InExtCtx $ \ctx_ext -> f $ runInExtCtx inExtCtx ctx_ext

-- | Map the bodies of two 'InExtCtx's with a context-polymorphic function
map2InExtCtx :: (forall ctx'. f ctx' a -> g ctx' b -> h ctx' c) ->
                InExtCtx f ctx a -> InExtCtx g ctx b -> InExtCtx h ctx c
map2InExtCtx f inExtCtx1 inExtCtx2 =
  InExtCtx $ \ctx_ext ->
  f (runInExtCtx inExtCtx1 ctx_ext) (runInExtCtx inExtCtx2 ctx_ext)

-- | The result of interpreting a term of a given type in a given context, using
-- a given type function @f@ on contexts and result types
newtype InterpRes (f :: RList * -> * -> *) (ctx :: RList *) (a :: *) =
  InterpRes { unInterpRes :: InExtCtx (InterpResH f) ctx a }

-- | Helper type for 'InterpRes'
data InterpResH (f :: RList * -> * -> *) (ctx :: RList *) (a :: *) where
  InterpRes_base :: L1Type a -> f ctx a -> InterpResH f ctx a
  InterpRes_fun :: (InterpRes f ctx a -> InterpRes f ctx b) ->
                   InterpResH f ctx (a -> b)
  InterpRes_pm :: f ctx (PM a) -> InterpResH f ctx (PM a)

-- | Combine 'unInterpRes' with 'runInExtCtx' to apply an 'InterpRes' to a
-- 'CtxExt' proof
runInterpRes :: InterpRes f ctx a -> CtxExt ctx ctx' -> InterpResH f ctx' a
runInterpRes interpRes = runInExtCtx $ unInterpRes interpRes

-- | Lower an 'InterpRes' into a context with one more type
lowerInterpRes1 :: InterpRes f ctx b -> InterpRes f (ctx :> a) b
lowerInterpRes1 (InterpRes inExtCtx) = InterpRes $ lowerInExtCtx1 inExtCtx

-- | Use a 'CtxExt' proof to lower the context of an 'InterpRes'
extendInterpRes :: CtxExt ctx1 ctx2 -> InterpRes f ctx1 a -> InterpRes f ctx2 a
extendInterpRes ctx_ext (InterpRes inExtCtx) =
  InterpRes $ extendInExtCtx ctx_ext inExtCtx

-- | Build a functional 'InterpRes'
lambdaInterpRes :: (forall ctx'. CtxExt ctx ctx' ->
                    InterpRes f ctx' a -> InterpRes f ctx' b) ->
                   InterpRes f ctx (a -> b)
lambdaInterpRes f =
  InterpRes $ InExtCtx $ \ctx_ext -> InterpRes_fun $ f ctx_ext

-- | Extract the function from a functional 'InterpResH'
applyInterpResH :: InterpResH f ctx (a -> b) -> InterpRes f ctx a ->
                   InterpRes f ctx b
applyInterpResH (InterpRes_fun f) = f

-- | Extract the @f ctx a@ from an 'InterpResH f ctx a' when @a@ is first-order
extractInterpResH :: L1Type a -> InterpResH f ctx a -> f ctx a
extractInterpResH (L1Type_lit _) (InterpRes_base _ res) = res
extractInterpResH L1Type_ptr (InterpRes_base _ res) = res
extractInterpResH L1Type_prop (InterpRes_base _ res) = res

-- | Extract the @f ctx (PM a) @ from an 'InterpResH f ctx (PM a)'
extractInterpResH_pm :: InterpResH f ctx (PM a) -> f ctx (PM a)
extractInterpResH_pm (InterpRes_pm res) = res
extractInterpResH_pm (InterpRes_base _ res) =
  -- NOTE: this case is actually impossible, but we don't really care...
  res

-- | Apply a functional 'InterpRes'
applyInterpRes :: InterpRes f ctx (a -> b) -> InterpRes f ctx a ->
                  InterpRes f ctx b
applyInterpRes f arg =
  InterpRes $ InExtCtx $ \ctx_ext ->
  runInterpRes (applyInterpResH (runInterpRes f ctx_ext) $
                extendInterpRes ctx_ext arg) $
  ctxExtRefl_right ctx_ext

-- | Turn an 'InterpRes' function into an 'InterpRes' inside a binding, assuming
-- that we know how to make an object for variables of the bound type. Useful
-- for writing 'interpOpC' functions.
bindingInterpRes :: (forall ctx'. Member ctx' a -> InterpResH f ctx' a) ->
                    InterpRes f ctx (a -> b) -> InterpRes f (ctx :> a) b
bindingInterpRes mk_var f =
  applyInterpRes (lowerInterpRes1 f) $
  InterpRes $ InExtCtx $ \ctx_ext ->
  mk_var $ ctxExtMember ctx_ext Member_Base

-- | Specialized version of 'bindingInterpRes' for first-order types
binding1InterpRes :: L1Type a -> (forall ctx'. Member ctx' a -> f ctx' a) ->
                     InterpRes f ctx (a -> b) -> InterpRes f (ctx :> a) b
binding1InterpRes l1tp mk_var f =
  bindingInterpRes (InterpRes_base l1tp . mk_var) f

-- | A contextual expression @f@-algebra is like an expression @f@-algebra
-- except that @f@ can depend on the current variable context
class LCtxExprAlgebra (f :: RList * -> * -> *) where
  interpOpC :: Proxy ctx -> Op a -> InterpRes f ctx a

-- | Interpret an 'LExpr' using a contextual @f@-algebra. The 'LExpr' is a
-- closed expression in a given source context. The result is an 'InterpRes'
-- that is relative to a given destination context.
interpExprC :: LCtxExprAlgebra f =>
               MapRList (InterpRes f dest_ctx) ctx ->
               Closed (Mb ctx (LExpr a)) -> InterpRes f dest_ctx a
interpExprC ctx [clNuP| LLambda _ body |] =
  lambdaInterpRes $ \ctx_ext x ->
  interpExprC (mapMapRList (extendInterpRes ctx_ext) ctx :>: x) $
  clApply $(mkClosed [| mbCombine |]) body
interpExprC ctx [clNuP| LAppExpr e |] = interpAppExprC ctx e

-- | Interpret an 'LAppExpr' to another functor @f@ using an @f@-algebra
interpAppExprC :: LCtxExprAlgebra f =>
                  MapRList (InterpRes f dest_ctx) ctx ->
                  Closed (Mb ctx (LAppExpr a)) -> InterpRes f dest_ctx a
interpAppExprC ctx [clNuP| LVar _ n |] =
  case clApply $(mkClosed [| mbNameBoundP |]) n of
    [clP| Left memb |] -> hlistLookup (unClosed memb) ctx
    [clP| Right closed_n |] -> noClosedNames closed_n
interpAppExprC (ctx :: MapRList (InterpRes f dest_ctx) ctx) [clNuP| LOp clmb_op |] =
  let op = mbLift $ unClosed clmb_op in
  interpOpC (Proxy :: Proxy dest_ctx) op
interpAppExprC ctx [clNuP| LApp f arg |] =
  applyInterpRes (interpAppExprC ctx f) (interpExprC ctx arg)


----------------------------------------------------------------------
-- Functions to build contextual f-algebras
----------------------------------------------------------------------

-- | Form the type
--
-- > f (ctx :> a_1_1 :> ... :> a_m1_1) b1 -> ... ->
-- > f (ctx :> a_1_n :> ... :> a_mn_n) bn -> c
--
-- when @a@ has the form
--
-- > (a_1_1 -> ... -> a_m1_1 -> b1) -> ... ->
-- > (a_1_n -> ... -> a_mn_n -> bn) -> c
type family ApplyToArgsInCtx (f :: RList * -> * -> *) (ctx :: RList *) a :: *
type instance ApplyToArgsInCtx f ctx (Literal a) = f ctx (Literal a)
type instance ApplyToArgsInCtx f ctx Ptr = f ctx Ptr
type instance ApplyToArgsInCtx f ctx Prop = f ctx Prop
type instance ApplyToArgsInCtx f ctx (PM a) = f ctx (PM a)
type instance ApplyToArgsInCtx f ctx (a -> b) =
  BindingApplyF f ctx a -> ApplyToArgsInCtx f ctx b

-- | General form for building interpretations of 'Op's. FIXME: this has not
-- been written yet! FIXME: also, the variable-building function here should
-- really be part of the LCtxExprAlgebra class...
buildInterpRes :: LType a ->
                  (forall ctx' a. LType a -> Member ctx' a -> f ctx' a) ->
                  (forall ctx'. CtxExt ctx ctx' -> ApplyToArgsInCtx f ctx' a) ->
                  InterpRes f ctx a
buildInterpRes = error "write buildInterpRes!"

-- | Helper function to build interpretations for a first-order operations
buildFOInterpResH :: L1FunType a ->
                     (forall ctx'. CtxExt ctx ctx' -> ApplyToArgs (f ctx') a) ->
                     InterpRes f ctx a
buildFOInterpResH (L1FunType_base l1tp@(L1Type_lit _)) x =
  InterpRes $ InExtCtx $ InterpRes_base l1tp . x
buildFOInterpResH (L1FunType_base l1tp@L1Type_ptr) x =
  InterpRes $ InExtCtx $ InterpRes_base l1tp . x
buildFOInterpResH (L1FunType_base l1tp@L1Type_prop) x =
  InterpRes $ InExtCtx $ InterpRes_base l1tp . x
buildFOInterpResH (L1FunType_cons l1tp_a tp_b) f =
  lambdaInterpRes $ \ctx_ext x ->
  buildFOInterpResH tp_b $ \ctx_ext' ->
  f (ctxExtAppend ctx_ext ctx_ext') $
  extractInterpResH l1tp_a $ runInterpRes x ctx_ext'

-- | Helper function to build interpretations for a first-order operations
buildFOInterpRes :: L1FunTypeable a =>
                    (forall ctx'. CtxExt ctx ctx' -> ApplyToArgs (f ctx') a) ->
                    InterpRes f ctx a
buildFOInterpRes f = buildFOInterpResH l1funTypeRep f

-- | Helper function to build interpretations for quantifier operations, that
-- have type @ (a -> Prop) -> Prop@
buildQuantiferInterpRes :: L1Type a ->
                           (forall ctx'. Member ctx' a -> f ctx' a) ->
                           (forall ctx'. f (ctx' :> a) Prop -> f ctx' Prop) ->
                           InterpRes f ctx ((a -> Prop) -> Prop)
buildQuantiferInterpRes l1tp mk_var f =
  lambdaInterpRes $ \ctx_ext body_interp ->
  InterpRes $ InExtCtx $ \ctx_ext' ->
  InterpRes_base L1Type_prop $ f $
  extractInterpResH L1Type_prop $
  runInterpRes (binding1InterpRes l1tp mk_var body_interp) $
  CtxExt_cons ctx_ext'

-- | Helper function to build an interpretation for 'Op_bindP'
buildBindInterpRes :: L1Type a -> L1Type b ->
                      (forall ctx'. Member ctx' a -> f ctx' a) ->
                      (forall ctx'. f ctx' (PM a) -> f (ctx' :> a) (PM b) ->
                       f ctx' (PM b)) ->
                      InterpRes f ctx (PM a -> (a -> PM b) -> PM b)
buildBindInterpRes l1tp_a l1tp_b mk_var f =
  lambdaInterpRes $ \ctx_ext1 arg1 ->
  lambdaInterpRes $ \ctx_ext2 arg2 ->
  InterpRes $ InExtCtx $ \ctx_ext3 ->
  InterpRes_pm $
  f (extractInterpResH_pm $
     runInterpRes arg1 $ ctxExtAppend ctx_ext2 ctx_ext3) $
  extractInterpResH_pm $
  runInterpRes (binding1InterpRes l1tp_a mk_var arg2) $
  CtxExt_cons ctx_ext3

-}


----------------------------------------------------------------------
-- * A Monad for Fresh Names
----------------------------------------------------------------------

-- | This type classifies how an existential name in a 'WithNames' is created
data NameDecl a where
  -- | An existential name with no constraints
  NameDecl_exists :: L1FunType a -> NameDecl a
  -- | A let-bound name, with a right-hand side
  NameDecl_let :: L1Type a -> LExpr a -> NameDecl a

-- | Get a list of formulas associated with a 'NameDecl'
nameDeclProps :: NameDecl a -> Name a -> [LProp]
nameDeclProps (NameDecl_exists _) _ = []
nameDeclProps (NameDecl_let l1tp rhs :: NameDecl a) n =
  [mkEq1Tp l1tp (mkVarExprTp (LType_base l1tp) n) rhs]

-- | Apply 'nameDeclProps' to a 'NameDecl' in a name-binding
mbNameDeclProps :: Mb ctx (NameDecl a) -> [Mb (ctx ':> a) LProp]
mbNameDeclProps mb_decl =
  mbList $ mbCombine $ fmap (nu . nameDeclProps) mb_decl

-- | Extract the 'L1FunType' associated with a 'NameDecl'
mbNameDeclFunType :: Mb ctx (NameDecl a) -> L1FunType a
mbNameDeclFunType [nuP| NameDecl_exists ftp |] = mbLift ftp
mbNameDeclFunType [nuP| NameDecl_let l1tp _ |] =
  L1FunType_base $ mbLift l1tp

-- | An object inside 0 or more some existential quantifiers at function type
data ExFOFuns a where
  ExFOFuns :: MapRList L1FunType ctx -> Mb ctx a -> ExFOFuns a

instance Functor ExFOFuns where
  fmap f (ExFOFuns ctx mb_a) = ExFOFuns ctx $ fmap f mb_a

-- Make the NuMatching instance for ExFOFuns
$(mkNuMatching [t| forall a. NuMatching a => ExFOFuns a |])

-- | Turn a 'ExFOFuns' in a binding for first-order function types into an
-- expanded 'ExFOFuns' that quantifies over all the combined function types
mbExFOFuns :: NuMatching a => MapRList L1FunType ctx1 -> Mb ctx1 (ExFOFuns a) ->
              ExFOFuns a
mbExFOFuns ctx1 [nuP| ExFOFuns mb_ctx2 mb_mb_a |] =
  ExFOFuns (appendMapRList ctx1 (mbLift mb_ctx2)) (mbCombine mb_mb_a)

-- | Substitute into 'ExFOFuns' existential quantifier
substExFOFuns :: MapRList LExpr ctx -> Mb ctx (ExFOFuns (LExpr a)) ->
                 ExFOFuns (LExpr a)
substExFOFuns exprs [nuP| ExFOFuns mb_ctx mb_mb_expr |] =
  let ctx = mbLift mb_ctx in
  ExFOFuns ctx $ nuMulti ctx $ \ex_names ->
  substExpr (appendMapRList exprs
             (mapMapRList2 (\ftp n -> mkVarFOFunTp ftp n) ctx ex_names)) $
  mbCombine mb_mb_expr

-- | A monad for computing with fresh bound names
newtype WithNames a =
  WithNames { unWithNames ::
                forall res. NuMatching res => (a -> ExFOFuns res) -> ExFOFuns res }

instance Functor WithNames where
  fmap f m = m >>= return . f

instance Applicative WithNames where
  pure = return
  (<*>) = ap

instance Monad WithNames where
  return x = WithNames $ \k -> k x
  WithNames f >>= g = WithNames $ \k -> f $ \x -> unWithNames (g x) k

instance NuMatching a => RunM WithNames a (ExFOFuns a) where
  runM m = unWithNames m $ \x -> ExFOFuns MNil (emptyMb x)

-- | Type class for monads that allow name-binding
class Monad m => NamesMonad m where
  nuM :: L1FunType a -> m (Name a)

instance NamesMonad WithNames where
  nuM ftp = WithNames $ \k -> mbExFOFuns (MNil :>: ftp) (nu k)

instance NamesMonad m => NamesMonad (StateT s m) where
  nuM ftp = lift $ nuM ftp

instance NamesMonad m => NamesMonad (ChoiceT m) where
  nuM ftp = lift $ nuM ftp

instance NamesMonad m => NamesMonad (ExceptionT exn m) where
  nuM ftp = lift $ nuM ftp

-- | Use 'nuM' to generate multiple fresh names
nuMultiM :: NamesMonad m => MapRList L1FunType ctx -> m (MapRList Name ctx)
nuMultiM MNil = return MNil
nuMultiM (ctx :>: ftp) = (:>:) <$> nuMultiM ctx <*> nuM ftp

-- | Generate a fresh, existentially-bound name
freshExName :: (NamesMonad m, L1FunTypeable a) => m (Name a)
freshExName = nuM l1funTypeRep

-- | Explicitly-typed version of 'freshExName'
freshExNameTp :: NamesMonad m => L1FunType a -> m (Name a)
freshExNameTp = nuM

-- | Eliminate the binders in an 'ExFOFuns' existential quantifier, by
-- generating fresh names for all the names it binds and then substituting
elimExFOFunsM :: NamesMonad m => ExFOFuns (LExpr a) -> m (LExpr a)
elimExFOFunsM (ExFOFuns ctx mb_expr) =
  do ns <- nuMultiM ctx
     return $ substExpr (mapMapRList2 mkVarFOFunTp ctx ns) mb_expr


----------------------------------------------------------------------
-- * A monad for backtracking search with informative failures
----------------------------------------------------------------------

-- | The backtracking monad transformer with an informative failure type
newtype BackT fail (m :: * -> *) a =
  BackT { unBackT ::
            forall r. (a -> (fail -> m r) -> m r) -> (fail -> m r) -> m r }

instance Functor (BackT fail m) where
  fmap f m = m >>= return . f

instance Applicative (BackT fail m) where
  pure = return
  (<*>) = ap

instance Monad (BackT fail m) where
  return x = BackT $ \ks kf -> ks x kf
  (BackT m) >>= f =
    BackT $ \ks kf -> m (\a kf' -> unBackT (f a) ks kf') kf

instance MonadT (BackT fail) where
  lift m = BackT $ \ks kf -> m >>= \a -> ks a kf

instance ExceptionM (BackT fail m) fail where
  raise exn = BackT $ \_ kf -> kf exn

instance Monad m => RunExceptionM (BackT fail m) fail where
  try (BackT m) = lift $ m (\a _ -> return $ Right a) (return . Left)

-- Running a BackT computation yields either a success or a failure
instance RunM m (Either fail a) r => RunM (BackT fail m) a r where
  runM (BackT m) = runM $ m (\a _ -> return $ Right a) (return . Left)

-- | A typeclass for monads that support disjunction; i.e., they have an 'mplus'
-- but instead of 'mzero' they might have 'raise' (which is different from
-- 'mzero' because it takes an exception value)
class Monad m => MonadDisj m where
  mdisj :: m a -> m a -> m a

instance MonadDisj (BackT fail m) where
  mdisj (BackT m1) (BackT m2) =
    BackT $ \ks kf -> m1 ks (\_ -> m2 ks kf)

instance MonadDisj m => MonadDisj (ReaderT r m) where
  mdisj m1 m2 =
    do r <- ask
       lift $ mdisj (runReaderT r m1) (runReaderT r m2)


----------------------------------------------------------------------
-- * The Symbolic Representation of Memory
----------------------------------------------------------------------

-- | The type of a memory array of a literal type @a@, which is represented as a
-- function from pointers plus offsets to literal values of type @a@ stored at
-- that offset from that pointer
type SymMemArrayType a = Ptr -> Literal Word64 -> Literal a

-- | The type of a memory array of pointers, which is represented as a relation
-- from a pointer plus offset to the pointer stored at that offset from the
-- former pointer
type SymMemPtrArrayType = Ptr -> Literal Word64 -> Ptr -> Prop

-- | A 'Name' that represents a symbolic memory of arrays of a literal type @a@
-- plus a proof that @a@ is a literal type
data SymMemName a = SymMemName (LitType a) (Name (SymMemArrayType a))

$(mkNuMatching [t| forall a. SymMemName a |])

-- | The symbolic representation of a 'Memory'
data SymMemory (mm :: [*]) =
  SymMemory
  {
    symMemArrays :: MapList SymMemName mm,
    -- ^ One 'Name' for each storable type
    symMemPtrArray :: Name SymMemPtrArrayType,
    -- ^ A 'Name' for storing the 'Ptr' type
    symMemLengths :: Name (Ptr -> Literal Word64),
    -- ^ A 'Name' for the function containing the lengths of the various arrays
    symMemLastAlloc :: Name Ptr
    -- ^ A 'Name' for the last-allocated pointer value
  }

-- | Helper type for building the 'NuMatching' instance for 'SymMemory'
data SymMemNameList as where
  SymMemNameListNil :: SymMemNameList '[]
  SymMemNameListCons :: SymMemName a -> SymMemNameList as ->
                        SymMemNameList (a ': as)

$(mkNuMatching [t| forall as. SymMemNameList as |])

toSymMemNameList :: MapList SymMemName as -> SymMemNameList as
toSymMemNameList Nil = SymMemNameListNil
toSymMemNameList (Cons a as) = SymMemNameListCons a (toSymMemNameList as)

fromSymMemNameList :: SymMemNameList as -> MapList SymMemName as
fromSymMemNameList SymMemNameListNil = Nil
fromSymMemNameList (SymMemNameListCons a as) = (Cons a (fromSymMemNameList as))

-- We build a NuMatching instance for SymMemory using an isomorphism to tuples
-- since the HobbitLib TH stuff for building them doesn't handle records yet...
instance NuMatching (SymMemory mm) where
  nuMatchingProof =
    isoMbTypeRepr
    (\SymMemory {..} ->
      (toSymMemNameList symMemArrays, symMemPtrArray,
       symMemLengths, symMemLastAlloc))
    (\(fromSymMemNameList -> symMemArrays, symMemPtrArray,
       symMemLengths, symMemLastAlloc) ->
      SymMemory {..})

-- | The 'LType' corresponding to @'SymMemArrayType' a@
symMemArrayType :: LitType a -> LType (SymMemArrayType a)
symMemArrayType ltp =
  LType_fun (LType_base L1Type_ptr) $
  LType_fun (LType_base $ L1Type_lit LitType_bits) $
  LType_base $ L1Type_lit ltp

-- | The 'L1FunType' corresponding to @'SymMemArrayType' a@
symMemArrayFunType :: LitType a -> L1FunType (SymMemArrayType a)
symMemArrayFunType ltp =
  L1FunType_cons L1Type_ptr $ L1FunType_cons (L1Type_lit LitType_bits) $
  L1FunType_base $ L1Type_lit ltp

-- | Extract a typed expression for the 'symMemArrays' field of a 'SymMemory'
symMemArrayName :: SymMemory mm -> ElemPf mm a -> SymMemName a
symMemArrayName mem elem_pf = ml_lookup (symMemArrays mem) elem_pf

-- | Make the proposition that two 'SymMemory's are equal
symMemEquals :: SymMemory mm -> SymMemory mm -> LProp
symMemEquals mem1 mem2 =
  mkAnd $
  memArraysEqual (symMemArrays mem1) (symMemArrays mem2) ++
  [mkNameEq l1funTypeRep (symMemPtrArray mem1) (symMemPtrArray mem2),
   mkNameEq l1funTypeRep (symMemLengths mem1) (symMemLengths mem2),
   mkNameEq l1funTypeRep (symMemLastAlloc mem1) (symMemLastAlloc mem2)]
  where
    memArraysEqual :: MapList SymMemName mm -> MapList SymMemName mm -> [LProp]
    memArraysEqual Nil _ = []
    memArraysEqual (Cons (SymMemName l1tp n1) as1) (ml_first_rest ->
                                                    (SymMemName _ n2, as2)) =
      mkNameEq (symMemArrayFunType l1tp) n1 n2 : memArraysEqual as1 as2

-- | A type context of all the variables needed to build a @'SymMemory' tag@
-- when @mm = LStorables tag@
type family SymMemoryCtx (mm :: [*]) :: RList *
type instance SymMemoryCtx '[] =
  'RNil ':> SymMemPtrArrayType ':> (Ptr -> Literal Word64) ':> Ptr
type instance SymMemoryCtx (a ': mm) =
  SymMemoryCtx mm ':> SymMemArrayType a

-- | Construct a context of types for a 'SymMemory'
symMemoryCtxTypes :: MemoryModel mm =>
                     Proxy mm -> MapRList L1FunType (SymMemoryCtx mm)
symMemoryCtxTypes (_ :: Proxy mm) =
  helper (memoryLitTypes :: MapList LitType mm)
  where
    helper :: MapList LitType mm' -> MapRList L1FunType (SymMemoryCtx mm')
    helper (Cons lit_tp mm) = helper mm :>: symMemArrayFunType lit_tp
    helper Nil = MNil :>: l1funTypeRep :>: l1funTypeRep :>: l1funTypeRep

-- | Construct a context of free variables from a symbolic memory
namesOfSymMemory :: SymMemory mm -> MapRList Name (SymMemoryCtx mm)
namesOfSymMemory (SymMemory { .. }) =
  helper symMemArrays (MNil :>: symMemPtrArray :>:
                       symMemLengths :>: symMemLastAlloc)
  where
    helper :: MapList SymMemName mm' -> MapRList Name (SymMemoryCtx '[]) ->
              MapRList Name (SymMemoryCtx mm')
    helper (Cons (SymMemName _ n) ns) base = helper ns base :>: n
    helper Nil base = base

-- | Construct a context of expressions from the free variables in a symbolic
-- memory returned by 'namesOfSymMemory'
exprsOfSymMemory :: MemoryModel mm => SymMemory mm ->
                    MapRList LExpr (SymMemoryCtx mm)
exprsOfSymMemory (mem :: SymMemory mm) =
  mapMapRList2 mkVarFOFunTp (symMemoryCtxTypes (Proxy :: Proxy mm))
  (namesOfSymMemory mem)

-- | Construct a symbolic memory from a context of free variables
symMemoryOfNames :: MemoryModel mm => MapRList Name (SymMemoryCtx mm) ->
                    SymMemory mm
symMemoryOfNames names =
  let (symMemArrays, symMemPtrArray,
       symMemLengths, symMemLastAlloc) = helper memoryLitTypes names
  in
  SymMemory { symMemArrays, symMemPtrArray, symMemLengths, symMemLastAlloc }
  where
    helper :: MapList LitType mm -> MapRList Name (SymMemoryCtx mm) ->
              (MapList SymMemName mm, Name SymMemPtrArrayType,
               Name (Ptr -> Literal Word64), Name Ptr)
    helper Nil (MNil :>: n1 :>: n2 :>: n3) = (Nil, n1, n2, n3)
    helper (Cons lit_tp lit_tps) (names' :>: n) =
      let (ns, n1, n2, n3) = helper lit_tps names' in
      (Cons (SymMemName lit_tp n) ns, n1, n2, n3)

-- | Build the proposition stating that @vals@ is a valid name for use as a
-- 'symMemPtrArray' field in a 'SymMemory'. This means that (vals ptr ix ptr')
-- should only hold for at most one ptr' value, for any given ptr and ix values.
validPtrArrayName :: Name SymMemPtrArrayType -> LProp
validPtrArrayName vals =
  mkForall $ \p ->
  mkForall $ \i ->
  mkForall $ \p1 ->
  mkForall $ \p2 ->
  mkOr [mkNot (mkVarApp vals p i p1),
        mkNot (mkVarApp vals p i p2),
        mkEq1 p1 p2]

-- | Construct the proposition that a 'SymMemory' is well-formed
validSymMemoryProp :: SymMemory mm -> LProp
validSymMemoryProp mem = validPtrArrayName $ symMemPtrArray mem

-- | Substitute a 'SymMemory' into an expression
substMemory :: MemoryModel mm => SymMemory mm ->
               Mb (SymMemoryCtx mm) (LExpr a) -> LExpr a
substMemory mem mb_expr = substExpr (exprsOfSymMemory mem) mb_expr


----------------------------------------------------------------------
-- * The Logic Predicate Monad
----------------------------------------------------------------------

-- | Typeclass associating type tag @arch@ with information about the
-- architecture that it models, including the types of literal values that can
-- be stored and the type of "exceptions" (i.e., non-local exits) it supports
class (MemoryModel (ArchStorables arch), Eq (ArchException arch),
       NuMatching (ArchException arch)) =>
      Arch arch where
  type ArchStorables arch :: [*]
  type ArchException arch :: *

type ArchMemory arch = SymMemory (ArchStorables arch)
type ArchMemoryCtx arch = SymMemoryCtx (ArchStorables arch)

-- | The type of transition relations on the architecture described by @arch@
-- (FIXME: better documentation)
newtype LogicPM arch a =
  LogicPM { unLogicPM ::
              ExceptionT (ArchException arch)
              (StateT (ArchMemory arch, [LExpr Prop])
               (ChoiceT
                WithNames)) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadPlus, NamesMonad)

-- The run method for LogicPM
instance (NuMatching a, Arch arch, mem ~ ArchMemory arch,
          exn ~ ArchException arch) =>
         RunM (LogicPM arch) a (mem ->
                                ExFOFuns [(Either exn a, (mem, [LProp]))]) where
  runM (LogicPM m) =
    \mem -> runM $ findAll $ runStateT (mem,[]) $ runExceptionT m

instance exn ~ (ArchException arch) => ExceptionM (LogicPM arch) exn where
  raise = LogicPM . raise

instance exn ~ (ArchException arch) => RunExceptionM (LogicPM arch) exn where
  try (LogicPM m) = LogicPM $ try m

-- | Get the current 'SymMemory' in a 'LogicPM' computation
getMem :: LogicPM arch (ArchMemory arch)
getMem = LogicPM (fst <$> get)

-- | Set the current 'SymMemory' in a 'LogicPM' computation
setMem :: ArchMemory arch -> LogicPM arch ()
setMem mem = LogicPM $ sets_ (\(_,props) -> (mem,props))

-- | Prune out the current execution in a 'LogicPM' computation
falsePM :: LogicPM arch ()
falsePM = LogicPM mzero

-- | Assume an 'LProp' along the current execution in a 'LogicPM' computation
assumePM :: LProp -> LogicPM arch ()
assumePM prop = LogicPM $ sets_ $ \(mem, props) -> (mem, prop:props)

-- | Generate a fresh, let-bound name from a definition for it
letBindName :: L1Typeable a => LExpr a -> LogicPM arch (Name a)
letBindName rhs =
  do n <- nuM $ L1FunType_base l1typeRep
     assumePM (mkEq1 (mkVar1 n) rhs)
     return n

-- | Generate a fresh, let-bound name from a definition for it, returning an
-- expression for the generated name
letBind :: L1Typeable a => LExpr a -> LogicPM arch (LExpr a)
letBind rhs = mkVar1 <$> letBindName rhs

-- | Build an assertion in the predicate monad. This builds a disjunctive
-- transition relation, which transitions to an error state (given by the
-- supplied exception) if the assertion does not hold, and otherwise performs a
-- no-op transition.
assertPM :: Arch arch => ArchException arch -> LProp -> LogicPM arch ()
assertPM exn prop =
  orPM (assumePM prop) (assumePM (mkNot prop) >> raisePM exn)

-- | Raise an exception in a 'LogicPM' computation
raisePM :: ArchException arch -> LogicPM arch ()
raisePM = raise

-- | Run the first computation. If it raises the given exception, then run the
-- second computation.
catch :: (Eq exn, RunExceptionM m exn) => exn -> m a -> m a -> m a
catch exn m m_exn =
  do exn_or_res <- try m
     case exn_or_res of
       Left exn' ->
         if exn' == exn then m_exn else raise exn'
       Right res -> return res

-- | Catch an exception in 'LogicPM'
catchPM :: Arch arch => ArchException arch ->
           LogicPM arch a -> LogicPM arch a -> LogicPM arch a
catchPM = catch


----------------------------------------------------------------------
-- * Memory Operations in the Predicate Monad
----------------------------------------------------------------------

-- | Read a literal from memory
readLitPM :: Arch arch => ElemPf (ArchStorables arch) a -> LExpr Ptr ->
             LExpr (Literal Word64) -> LogicPM arch (LExpr (Literal a))
readLitPM elem_pf ptr ix =
  do mem <- getMem
     -- Get the variable vals associating pointers + offsets to stored values
     let SymMemName ltp vals = symMemArrayName mem elem_pf
     -- Return the expression (vals ptr ix)
     return $ mkVarAppTp (symMemArrayType ltp) vals ptr ix

-- | Read a pointer from memory, by generating a fresh 'Ptr' variable and
-- assuming that it is stored at the given location in memory
readPtrPM :: Arch arch => LExpr Ptr -> LExpr (Literal Word64) ->
             LogicPM arch (LExpr Ptr)
readPtrPM ptr ix =
  do mem <- getMem
     -- Get the variable vals associating pointers + offsets to stored values
     let vals = symMemPtrArray mem
     -- Make a fresh name for the returned pointer
     res_nm <- freshExName
     -- Assume that res_nm is stored at ptr+ix, i.e., that (vals ptr ix res_nm)
     assumePM $ mkVarApp vals ptr ix (mkVar1 res_nm)
     -- Return res_nm
     return $ mkVar1 res_nm

-- | Read the length associated with a memory object in memory
readPtrLengthM :: Arch arch => LExpr Ptr ->
                  LogicPM arch (LExpr (Literal Word64))
readPtrLengthM ptr =
  do lengths_nm <- symMemLengths <$> getMem
     return $ mkVarApp lengths_nm ptr

-- | Read the last allocated pointer value
readLastAllocM :: Arch arch => LogicPM arch (LExpr Ptr)
readLastAllocM = mkVar1 <$> symMemLastAlloc <$> getMem

-- | Update a literal value in memory
updateLitPM :: Arch arch => ElemPf (ArchStorables arch) a -> LExpr Ptr ->
               LExpr (Literal Word64) -> LExpr (Literal a) ->
               LogicPM arch ()
updateLitPM elem_pf ptr ix val =
  do mem <- getMem
     -- Get the variable vals associating pointers + offsets to stored values
     let SymMemName ltp vals = symMemArrayName mem elem_pf
     -- Create a fresh name for the updated memory
     vals' <- freshExNameTp (symMemArrayFunType ltp)
     -- Assume that (vals' ptr ix) = val
     assumePM $
       mkEqTp (L1FunType_base $ L1Type_lit ltp)
       (mkVarAppTp (symMemArrayType ltp) vals' ptr ix) val
     -- Assume that (vals' p i) = (vals p i) for p != ptr or i != ix
     assumePM $ mkForall $ \p ->
       mkForall $ \i ->
       mkOr [mkAnd [mkEq1 p ptr, mkEq1 i ix]
            ,
             mkEqTp (L1FunType_base $ L1Type_lit ltp)
             (mkVarAppTp (symMemArrayType ltp) vals p i)
             (mkVarAppTp (symMemArrayType ltp) vals' p i)]
     -- Set vals' as the output memory
     setMem $ mem { symMemArrays =
                      ml_map1 (\_ -> SymMemName ltp vals')
                      (symMemArrays mem) elem_pf }

-- | Update a pointer value in memory
updatePtrPM :: Arch arch => ElemPf (ArchStorables arch) a -> LExpr Ptr ->
               LExpr (Literal Word64) -> LExpr Ptr -> LogicPM arch ()
updatePtrPM elem_pf ptr ix val =
  do mem <- getMem
     let vals = symMemPtrArray mem
     -- Create a fresh name for the updated memory
     vals' <- freshExName
     -- Assume that vals' is a valid set of pointer arrays
     assumePM $ validPtrArrayName vals'
     -- Assume that (vals' ptr ix val) holds
     assumePM $ mkVarApp vals' ptr ix val
     -- Assume that (vals' p i p') = (vals p i p') for p != ptr or i != ix
     assumePM $ mkForall $ \p ->
       mkForall $ \i ->
       mkForall $ \p' ->
       mkOr [mkAnd [mkEq1 p ptr, mkEq1 i ix],
             mkEq1 (mkVarApp vals p i p') (mkVarApp vals' p i p')]
     -- Set mem' as the output memory
     setMem $ mem { symMemPtrArray = vals' }

-- | Copy the memory objects pointed to by a pointer, starting at an offset,
-- continuing for a given length, to a destination pointer + offset. This copies
-- all types of memory objects, including pointer values.
copyPtrPM :: Arch arch => LExpr Ptr -> LExpr (Literal Word64) ->
             LExpr (Literal Word64) -> LExpr Ptr -> LExpr (Literal Word64) ->
             LogicPM arch ()
copyPtrPM src_ptr src_ix len dest_ptr dest_ix =
  do mem <- getMem
     let vals = symMemPtrArray mem
     -- Create a fresh name for the pointer memory
     vals' <- freshExName
     -- Assume that, forall ix < len, we have (vals' dest_ptr (dest_ix + ix) p)
     -- = (vals src_ptr (src_ix + ix) p)
     assumePM $ mkForall $ \ix ->
       mkForall $ \p ->
       mkOr [mkLe len ix,
             mkEq1 (mkVarApp vals' dest_ptr (dest_ix + ix) p)
             (mkVarApp vals src_ptr (src_ix + ix) p)]
     -- Assume that (vals' p ix p') = (vals p ix p') when p != dest_ptr
     assumePM $ mkForall $ \p ->
       mkForall $ \ix ->
       mkForall $ \p' ->
       mkOr [mkEq1 p dest_ptr,
             mkEq1 (mkVarApp vals' p ix p') (mkVarApp vals p ix p')]
     -- Use the helper to make new symbolic memories for the non-pointer types
     arrays' <- copy_helper (symMemArrays mem)
     -- Set the updated memory with the new names
     setMem $ mem { symMemArrays = arrays', symMemPtrArray = vals' }
       where
         copy_helper :: MapList SymMemName mm ->
                        LogicPM tag (MapList SymMemName mm)
         copy_helper Nil = return Nil
         copy_helper (Cons (SymMemName lit_tp vals) arrays) =
           do let l1tp = L1Type_lit lit_tp
              let vals_type = symMemArrayType lit_tp
              -- Create a fresh name for the updated memory
              vals' <- freshExNameTp (symMemArrayFunType lit_tp)
              -- Assume that, for ix < len, (vals' dest_ptr (dest_ix + ix)) =
              -- (vals src_ptr (src_ix + ix))
              assumePM $ mkForall $ \ix ->
                mkOr [mkLe len ix,
                      mkEq1Tp l1tp (mkVarAppTp vals_type vals' dest_ptr ix)
                      (mkVarAppTp vals_type vals src_ptr ix)]
              -- Assume that (vals' p ix) = (vals p ix) when p != dest_ptr
              assumePM $ mkForall $ \p ->
                mkForall $ \ix ->
                mkOr [mkEq1 p dest_ptr,
                      mkEq1Tp l1tp
                      (mkVarAppTp vals_type vals' p ix)
                      (mkVarAppTp vals_type vals p ix)]
              -- Recurse, and combine the result with vals'
              arrays' <- copy_helper arrays
              return (Cons (SymMemName lit_tp vals') arrays')

-- | Allocate a pointer with a given length
allocPtrM :: Arch arch => LExpr (Literal Word64) -> LogicPM arch (LExpr Ptr)
allocPtrM len =
  do mem <- getMem
     let lengths = symMemLengths mem
     let last_alloc = symMemLastAlloc mem
     -- Define a new function for the lengths
     lengths' <- freshExName
     -- Define a new name last_alloc' = (next_ptr last_alloc)
     last_alloc' <- letBindName $ mkOp Op_next_ptr $ mkVar1 last_alloc
     -- Assume that (lengths' last_alloc') = len
     assumePM $ mkEq1 (mkVarApp lengths' $ mkVar1 last_alloc') len
     -- Assume that (mem_lengths' p) = (mem_lengths p) for p != last_alloc
     assumePM $
       mkForall $ \p ->
       mkOr [mkEq1 p (mkVar1 last_alloc'),
             mkEq1 (mkVarApp lengths' p) (mkVarApp lengths p)]
     -- Set the output memory
     setMem $ mem { symMemLengths = lengths', symMemLastAlloc = last_alloc' }
     -- Return the newly-allocated pointer
     return $ mkVar1 last_alloc'


----------------------------------------------------------------------
-- * Disjunction in the Predicate Monad
----------------------------------------------------------------------

-- | Collect all the possible alternative output states of a 'LogicPM'
-- computation, grouping them together by exception or lack thereof
groupByExnPM :: Arch arch => LogicPM arch () ->
                LogicPM arch [(Either (ArchException arch) (),
                               [(SymMemory (ArchStorables arch), [LProp])])]
groupByExnPM (LogicPM pm) =
  LogicPM $ get >>= \s ->
  liftM groupAList $
  lift $ lift $ lift $ findAll $ runStateT s $ runExceptionT pm

-- | Get the current propositions for this computation branch and bind a fresh
-- name equal to the conjunction of these propositions, returning this fresh
-- name after first clearing the input propositions (the motiviation for this
-- being that the caller is going to use the returned newly-let-bound prop)
letBindInputProps :: LogicPM arch LProp
letBindInputProps =
  do (mem, props) <- LogicPM get
     prop <- letBind (mkAnd props)
     LogicPM $ set (mem, [])
     return prop

-- | Join a list of symbolic memories into a single symbolic memory, creating
-- fresh names for the components that differ between the memories.
combineMemories :: [ArchMemory arch] -> LogicPM arch (ArchMemory arch)
combineMemories [] = error "combineMemories: empty list!"
combineMemories (mem:mems) =
  foldM (\mem1 mem2 ->
          do symMemArrays <-
               combineMemArrays (symMemArrays mem1) (symMemArrays mem2)
             symMemPtrArray <-
               combineNames l1funTypeRep (symMemPtrArray mem1) (symMemPtrArray mem2)
             symMemLengths <-
               combineNames l1funTypeRep (symMemLengths mem1) (symMemLengths mem2)
             symMemLastAlloc <-
               combineNames l1funTypeRep (symMemLastAlloc mem1) (symMemLastAlloc mem2)
             return $ SymMemory { symMemArrays, symMemPtrArray,
                                  symMemLengths, symMemLastAlloc }
          ) mem mems
  where
    combineNames :: L1FunType a -> Name a -> Name a -> LogicPM arch (Name a)
    combineNames ftp n1 n2 =
      if n1 == n2 then return n1 else nuM ftp
    combineMemArrays :: MapList SymMemName mm -> MapList SymMemName mm ->
                        LogicPM arch (MapList SymMemName mm)
    combineMemArrays Nil _ = return Nil
    combineMemArrays (Cons (SymMemName lit_tp n1) as1) (ml_first_rest ->
                                                        (SymMemName _ n2, as2)) =
      do n' <- combineNames (symMemArrayFunType lit_tp) n1 n2
         as' <- combineMemArrays as1 as2
         return $ Cons (SymMemName lit_tp n') as'

-- | FIXME: documentation
canonicalizePM :: Arch arch => LogicPM arch () -> LogicPM arch ()
canonicalizePM pm =
  do orig_prop <- letBindInputProps
     res_alist <- groupByExnPM pm
     foldr mplus mzero $
       map (\(exn_or_unit, mems_with_props) ->
            do mem' <- combineMemories $ map fst mems_with_props
               LogicPM $ set (mem',
                              [orig_prop,
                               mkOr $
                               map (\(mem, props) ->
                                     mkAnd (props ++ [symMemEquals mem mem']))
                               mems_with_props
                              ])
               case exn_or_unit of
                 Left exn -> LogicPM $ raise exn
                 Right () -> return ())
       res_alist

-- | Disjoin two 'LogicPM' computations
orPM :: Arch arch => LogicPM arch () -> LogicPM arch () -> LogicPM arch ()
orPM pm1 pm2 = canonicalizePM $ pm1 `mplus` pm2


----------------------------------------------------------------------
-- * SMT Solver Interface
----------------------------------------------------------------------

-- | Build the type lift @'[SMTValue a1, .., SMTValue an]@ from the type list
-- @[a1, .., an]@
type family SMTValues (as :: [*]) :: [*]
type instance SMTValues '[] = '[]
type instance SMTValues (a ': as) = SMTValue a ': SMTValues as

-- | FIXME: documentation
type SMTValueFun a =
  FinFun (SMTValues (ArgTypes a)) (SMTValue (RetType a))

-- | The type of a value returned by SMT solvers for a variable of a given type
type family SMTValue a
type instance SMTValue (Literal a) = Literal a
type instance SMTValue Ptr = Ptr
type instance SMTValue Prop = Bool
type instance SMTValue (a -> b) = SMTValueFun (a -> b)

-- | An optional value for a free variable passed to an SMT solver, where
-- 'Nothing' means that the variable with this value is unconstrained, i.e., any
-- value will work.
newtype MaybeSMTValue a =
  MaybeSMTValue { unMaybeSMTValue :: Maybe (SMTValue a) }

-- | Extract a 'PTr' from a 'MaybeSMTValue' of 'Ptr' type, using 'nullPtr' for
-- an unconstrained value
extractSMTValuePtr :: MaybeSMTValue Ptr -> Ptr
extractSMTValuePtr (MaybeSMTValue (Just p)) = p
extractSMTValuePtr (MaybeSMTValue Nothing) = nullPtr

-- | Extract a 'FinFun' from a 'MaybeSMTValue' of functional type, using the
-- given default return value for an unconstrained value
extractSMTValueFun :: SMTValue (RetType (a -> b)) -> MaybeSMTValue (a -> b) ->
                      SMTValueFun (a -> b)
extractSMTValueFun _ (MaybeSMTValue (Just f)) = f
extractSMTValueFun def (MaybeSMTValue Nothing) = mkFinFun def

-- | The result of calling an SMT solver on some set of input formulas
data SMTResult model
  = SMT_sat model
    -- ^ The formulas were satisfiable; a model of the formulas is also given
  | SMT_unsat
    -- ^ The formulas were unsatisfiable
  | SMT_unknown String
    -- ^ Satisfiability could not be determined; an error message is also given
  deriving Functor

-- | Type class indicating that a type can be used as an SMT solver. Often the
-- @solver@ type is just a dummy type, indicating a function to be called.
class SMTSolver solver where
  -- | Call the solver indicated by the type argument @solver@, passing it a set
  -- of propositions over some set of free variables given by @ctx@. These
  -- variables must have first-order function type. If the formulas are
  -- satisfiable, return a model of them, given by 'SMTValue's of the free
  -- variables, and also return the "satisfiable core" of atomic formulas that
  -- were satisfied by the model. An "atomic" formula here is one that contains
  -- no conjunction or disjunction except inside a quantifier
  smtSolve :: solver -> MapRList L1FunType ctx -> Mb ctx [LProp] ->
              IO (SMTResult (MapRList MaybeSMTValue ctx, Mb ctx [LProp]))
  -- | Get the debugging level of the solver: 0 = no debugging, 1 = print
  -- queries, higher levels = more info
  smtGetDebugLevel :: solver -> Int
  -- | Set the debugging level of the solver: 0 = no debugging, 1 = print
  -- queries, higher levels = more info
  smtSetDebugLevel :: Int -> solver -> solver

-- | FIXME: documentation
ptrArraysOfSymPtrArrays :: MaybeSMTValue SymMemPtrArrayType ->
                           FinFun '[ Ptr, Literal Word64 ] Ptr
ptrArraysOfSymPtrArrays (extractSMTValueFun False -> f) =
  if finfunDef f == True then
    error "ptrArraysOfSymPtrArrays: bad memory returned from SMT solver!"
  else
    foldl' (\arrays_f ((p_from, (i, (p_to, ()))), b) ->
             if b then
               updateFinFun arrays_f (p_from, (i, ())) p_to
             else arrays_f)
    (mkFinFun nullPtr)
    (finfunMap f)

-- | FIXME: documentation
memoryOfSMTValues :: MapList Literal mm ->
                     MapRList MaybeSMTValue (SymMemoryCtx mm) ->
                     Memory mm
memoryOfSMTValues Nil (MNil :>: ptrs_v :>: lengths_v :>: last_alloc_v) =
  Memory { memArrays = Nil,
           memPtrArray = ptrArraysOfSymPtrArrays ptrs_v,
           memLengths = extractSMTValueFun (Literal 0) lengths_v,
           memLastAlloc = extractSMTValuePtr last_alloc_v }
memoryOfSMTValues (Cons def mm) (mm_values :>: vals_v) =
  let mem = memoryOfSMTValues mm mm_values in
  mem { memArrays =
          Cons (LitArrayStore $ extractSMTValueFun def vals_v) (memArrays mem) }


----------------------------------------------------------------------
-- * Testing Reachability using an SMT Solver
----------------------------------------------------------------------

-- FIXME HERE: make ArchStateProp a newtype, rename it to just StateProp, and
-- get rid of all the damned proxies!

-- | A proposition about the current state, i.e., memory. This is represented as
-- an existential proposition inside name-bindings for the current memory.
type StateProp mm = Mb (SymMemoryCtx mm) (ExFOFuns LProp)

type ArchStateProp arch = StateProp (ArchStorables arch)

-- | Build a 'StateProp' from a proposition inside a binding for the variables
-- of a symbolic memory plus possibly some extras
statePropOfMb :: Proxy mm -> MapRList L1FunType extras ->
                 Mb (SymMemoryCtx mm :++: extras) LProp -> StateProp mm
statePropOfMb _ extra_tps mb_prop =
  fmap (ExFOFuns extra_tps) $ mbSeparate extra_tps mb_prop

statePropOfProp :: Arch arch => Proxy arch -> LProp -> ArchStateProp arch
statePropOfProp (_ :: Proxy arch) prop =
  nuMulti (symMemoryCtxTypes (Proxy :: Proxy (ArchStorables arch))) $ \_ ->
  ExFOFuns MNil $ emptyMb prop

trueStateProp :: Arch arch => Proxy arch -> ArchStateProp arch
trueStateProp proxy = statePropOfProp proxy mkTrue

falseStateProp :: Arch arch => Proxy arch -> ArchStateProp arch
falseStateProp proxy = statePropOfProp proxy mkFalse

-- | Substitue a memory into a 'StateProp'
substMemStateProp :: MemoryModel mm => SymMemory mm -> StateProp mm ->
                     ExFOFuns LProp
substMemStateProp mem state_prop =
  substExFOFuns (exprsOfSymMemory mem) state_prop

-- | Map a binary function over the propositions in two 'StateProp's
statePropMap2 :: Proxy mm -> (LProp -> LProp -> LProp) ->
                 StateProp mm -> StateProp mm -> StateProp mm
statePropMap2 _ f sprop1 sprop2 =
  flip nuMultiWithElim (MNil :>: sprop1 :>: sprop2) $ \_ exfos ->
  case exfos of
    (_ :>: Identity (ExFOFuns ctx1 mb_p1) :>: Identity (ExFOFuns ctx2 mb_p2)) ->
      ExFOFuns (appendMapRList ctx1 ctx2) $ mbCombine $
      flip nuMultiWithElim1 mb_p1 $ \_ p1 ->
      flip nuMultiWithElim1 mb_p2 $ \_ p2 ->
      f p1 p2
    _ -> error "statePropMap2: unreachable!"

-- | Construct the conjunction of two state propositions
archStatePropAnd :: Proxy arch -> ArchStateProp arch ->
                   ArchStateProp arch -> ArchStateProp arch
archStatePropAnd (_ :: Proxy arch) =
  statePropMap2 (Proxy :: Proxy (ArchStorables arch)) (\p1 p2 -> mkAnd [p1, p2])

-- | Construct the disjunction of two state propositions
archStatePropOr :: Proxy arch -> ArchStateProp arch ->
                   ArchStateProp arch -> ArchStateProp arch
archStatePropOr (_ :: Proxy arch) =
  statePropMap2 (Proxy :: Proxy (ArchStorables arch)) (\p1 p2 -> mkOr [p1, p2])

-- | Negate a state proposition
archStatePropNot :: Proxy arch -> ArchStateProp arch -> ArchStateProp arch
archStatePropNot _ = fmap (fmap mkNot)

-- | Assume that a 'StateProp' holds in a 'LogicPM'
assumeStateProp :: Arch arch => ArchStateProp arch -> LogicPM arch ()
assumeStateProp state_prop =
  do mem <- getMem
     prop <- elimExFOFunsM $ substMemStateProp mem state_prop
     assumePM prop

-- | Construct the proposition that a 'LogicPM' could transition from the
-- current state to a "success" final state (where it just returns unit instead
-- of throwing an exception), by forming the disjunction of all propositions
-- associated with a success exit
succeedsPM :: Arch arch => LogicPM arch () -> ArchStateProp arch
succeedsPM (pm :: LogicPM arch ()) =
  let mem_tps = symMemoryCtxTypes (Proxy :: Proxy (ArchStorables arch)) in
  nuMulti mem_tps $ \names ->
  fmap (\final_states ->
         mkOr $ mapMaybe (\(ex, (_, props)) ->
                           case ex of
                             Right () -> Just $ mkAnd props
                             Left _ -> Nothing)
         final_states) $
  runM pm (symMemoryOfNames names)

-- | An "exit"
type Exit arch = Either (ArchException arch) ()

-- | A reachability proposition, specifying a 'StateProp' that must hold for
-- each possible exit
type ReachProp arch = [(Exit arch, ArchStateProp arch)]

-- | The reachability proposition stating that a given exit is reachable
exitReachProp :: Arch arch => Proxy arch -> Exit arch -> ReachProp arch
exitReachProp proxy ex = [(ex, trueStateProp proxy)]

-- | Set the state proposition for a given exit in a reachability proposition
updateReachProp :: Arch arch => Proxy arch -> Exit arch -> ArchStateProp arch ->
                   ReachProp arch -> ReachProp arch
updateReachProp _ ex state_prop prop =
  map (\p@(ex', _) -> if ex == ex' then (ex, state_prop) else p) prop

-- | Construct a state proposition for the states from which a reachability
-- proposition is reachable via a 'LogicPM' transition
reachablePM :: Arch arch => LogicPM arch () -> ReachProp arch ->
               ArchStateProp arch
reachablePM pm reach_prop =
  -- Construct the proposition that, if we transition via pm, then we will end
  -- up in a final state that satisfies a proposition given by the clause in
  -- reach_prop given by the exit (an exception or "success" exit) that pm
  -- transitioned to
  succeedsPM
  (try pm >>= \ex ->
    case lookup ex reach_prop of
      Just state_prop -> assumeStateProp state_prop
      Nothing -> falsePM)

-- | Pass a state proposition to an SMT solver, returning a 'Memory' that
-- satisfies that proposition if it is satisfiable along with the satisfiable
-- core of the state proposition (see 'smtSolve')
smtSolveStateProp :: (SMTSolver solver, Arch arch) => solver ->
                     Proxy arch -> ArchStateProp arch ->
                     IO (SMTResult (Memory (ArchStorables arch),
                                    ArchStateProp arch))
smtSolveStateProp solver (_ :: Proxy arch) state_prop =
  case state_prop of
    [nuP| ExFOFuns mb_ctx prop |] ->
      do let proxy = Proxy :: Proxy (ArchStorables arch)
             mem_tps = symMemoryCtxTypes proxy
             (extra_tps, mb_prop) = (mbLift mb_ctx, mbCombine prop)
         res <-
           smtSolve solver (appendMapRList mem_tps extra_tps) $
           fmap (: []) mb_prop
         return $ flip fmap res $ \(model, sat_core) ->
           (
             -- Split model into values for the free vars of the memory and
             -- extra values, take only the former, and build a Memory of them
             memoryOfSMTValues memoryDefaults $ fst $
             splitMapRList Proxy extra_tps model,
             -- Conjoin all the props in the sat_core (inside their bindings)
             -- and then form a state prop of the result
             statePropOfMb proxy extra_tps $ fmap mkAnd sat_core)

-- | Pass the proposition generated by 'reachablePM' to an SMT solver, returning
-- an initial 'Memory' that can transition via the given 'LogicPM' transition to
-- a final state that satisfies the given 'ReachProp'
smtSolvePM :: (SMTSolver solver, Arch arch) => solver ->
              Proxy arch -> LogicPM arch () -> ReachProp arch ->
              IO (SMTResult (Memory (ArchStorables arch), ArchStateProp arch))
smtSolvePM solver proxy pm reach_prop =
  smtSolveStateProp solver proxy (reachablePM pm reach_prop)


----------------------------------------------------------------------
-- * Programs
----------------------------------------------------------------------

newtype BlockLabel = BlockLabel { getBlockLabel :: Int }

-- | The type of structured programs
data Program arch
  = BasicBlock !BlockLabel (LogicPM arch ())
    -- ^ A basic block, which is just a single transition
  | SeqBlock !BlockLabel !(Program arch) !(Program arch)
    -- ^ A block that sequences two sub-programs
  | OrBlock !BlockLabel !(Program arch) !(Program arch)
    -- ^ A disjunctive block, that can execute either sub-program
  | CatchBlock !BlockLabel !(ArchException arch) !(Program arch) !(Program arch)
    -- ^ A block that executes the first sub-program and then, if and only if
    -- that program threw the given exception, the second sub-program
  | LoopBlock !BlockLabel !(Program arch)
    -- ^ A loop block, that can execute the sub-program 0 or more times

-- | A monad for building 'Program's, using a fresh name counter
newtype PgmBuilder a = PgmBuilder { unPgmBuilder :: StateT BlockLabel Id a }
                     deriving (Functor, Monad, Applicative)

instance RunM (StateT BlockLabel Id) a r => RunM PgmBuilder a r where
  runM = runM . unPgmBuilder

-- | Generate a fresh 'BlockLabel' in the 'PgmBuilder' monad
freshLabel :: PgmBuilder BlockLabel
freshLabel = PgmBuilder $ do
  label@(BlockLabel i) <- get
  set $ BlockLabel $ i+1
  return label

-- | Generate a 'Program' from a transition
mkBasicBlock :: LogicPM arch () -> PgmBuilder (Program arch)
mkBasicBlock pm = BasicBlock <$> freshLabel <*> return pm

-- | Sequence two 'Program's
mkSeqBlock :: Program arch -> Program arch -> PgmBuilder (Program arch)
mkSeqBlock (BasicBlock _ pm1) (BasicBlock _ pm2) = mkBasicBlock (pm1 >> pm2)
mkSeqBlock pgm1 pgm2 = SeqBlock <$> freshLabel <*> return pgm1 <*> return pgm2

-- | Disjoin two 'Program's
mkOrBlock :: Arch arch => Program arch -> Program arch ->
             PgmBuilder (Program arch)
mkOrBlock (BasicBlock _ pm1) (BasicBlock _ pm2) = mkBasicBlock (orPM pm1 pm2)
mkOrBlock pgm1 pgm2 = OrBlock <$> freshLabel <*> return pgm1 <*> return pgm2

-- | Make a catch 'Program'
mkCatchBlock :: Arch arch => ArchException arch -> Program arch ->
                Program arch -> PgmBuilder (Program arch)
mkCatchBlock exn (BasicBlock _ pm1) (BasicBlock _ pm2) =
  mkBasicBlock $ catchPM exn pm1 pm2
mkCatchBlock exn pgm1 pgm2 =
  CatchBlock <$> freshLabel <*> return exn <*> return pgm1 <*> return pgm2

-- | Make a loop 'Program'
mkLoopBlock :: Program arch -> PgmBuilder (Program arch)
mkLoopBlock pgm = LoopBlock <$> freshLabel <*> return pgm


----------------------------------------------------------------------
-- * PDR and generating over-approximations
----------------------------------------------------------------------

-- | Generate a state proposition that over-approximates the set of input states
-- such that the given 'Program' could transition to a state that satisfies the
-- given reachability proposition
overReachablePgm :: (SMTSolver solver, Arch arch) => solver ->
                    Program arch -> ReachProp arch -> IO (ArchStateProp arch)
overReachablePgm _solver pgm p = helper Proxy pgm p where
  helper :: Arch arch => Proxy arch -> Program arch -> ReachProp arch ->
            IO (ArchStateProp arch)
  helper _ (BasicBlock _ pm) prop = return $ reachablePM pm prop
  helper proxy (SeqBlock _ pgm1 pgm2) prop =
    do state_prop1 <- helper proxy pgm2 prop
       helper proxy pgm1 $ updateReachProp proxy (Right ()) state_prop1 prop
  helper (proxy :: Proxy arch) (OrBlock _ pgm1 pgm2) prop =
    archStatePropOr proxy <$>
    helper proxy pgm1 prop <*> helper proxy pgm2 prop
  helper proxy (CatchBlock _ exn pgm1 pgm2) prop =
    do state_prop1 <- helper proxy pgm2 prop
       helper proxy pgm1 $ updateReachProp proxy (Left exn) state_prop1 prop
  helper proxy (LoopBlock label _pgm) prop =
    -- Return the weakest over-approximation, the true proposition
    putStrLn "FIXME HERE NOW: PDR not yet implemented!" >>
    return (trueStateProp proxy)


----------------------------------------------------------------------
-- * Generating under-approximations
----------------------------------------------------------------------

type UnderFail arch = (BlockLabel, ArchStateProp arch)

-- | The result type for under-approximation with respect to an architecture,
-- which is either a state proposition for the beginning of a program or a label
-- in the code
type UnderRes arch = Either (UnderFail arch) (ArchStateProp arch)

-- | The monad for computing under-approximations of state propositions. Each
-- computation is relative to an "expansion level" (the reader transformer), and
-- performs backtracking search (the backtracking transformer)
newtype UnderM arch a =
  UnderM { unUnderM :: ReaderT Int (BackT (UnderFail arch) IO) a }
  deriving (Functor, Applicative, Monad, MonadDisj)

instance (a ~ ArchStateProp arch, res ~ UnderRes arch) =>
         RunM (UnderM arch) a (Int -> IO res) where
  runM m i = runM (unUnderM m) i

instance BaseM (UnderM arch) IO where
  inBase = UnderM . lift . lift

instance exn ~ UnderFail arch => ExceptionM (UnderM arch) exn where
  raise = UnderM . raise

-- | Generate a stream of under-approximations from the satisfiable cores of a
-- state prop, where we generate the next sat core by negating the previous ones
--
-- FIXME: exclude the negated formulas from the Sat cores
satCoreStream :: (SMTSolver solver, Arch arch) =>
                 solver -> Proxy arch -> BlockLabel -> ArchStateProp arch ->
                 UnderM arch (ArchStateProp arch)
satCoreStream solver proxy label start_sprop =
  do res1 <- inBase $ smtSolveStateProp solver proxy start_sprop
     case res1 of
       SMT_sat (_, sat_core1) ->
         -- There is at least one Sat core, so start generating sat cores
         helper solver proxy start_sprop sat_core1
       SMT_unsat ->
         -- There is no Sat core for this label, so raise an error
         raise (label, start_sprop)
       SMT_unknown str -> error $ "SMT error:" ++ str
       where
         -- The helper takes in a state prop and a Sat core of that state prop,
         -- and it returns first the Sat core and then, if there are any more
         -- possible Sat cores (formed by conjoining the negation of the current
         -- Sat core with the initial state prop) it recurses
         helper :: (SMTSolver solver, Arch arch) =>
                   solver -> Proxy arch -> ArchStateProp arch ->
                   ArchStateProp arch -> UnderM arch (ArchStateProp arch)
         helper solver proxy sprop sat_core =
           do let next_sprop =
                    archStatePropAnd proxy sprop (archStatePropNot proxy sat_core)
              res <- inBase $ smtSolveStateProp solver proxy next_sprop
              case res of
                SMT_sat (_, next_sat_core) ->
                  -- There is at least one more Sat core, so keep going
                  return sat_core
                  `mdisj` helper solver proxy next_sprop next_sat_core
                SMT_unsat ->
                  -- We are at our last Sat core, so just return it
                  return sat_core
                SMT_unknown str -> error $ "SMT error:" ++ str

-- | Perform backtracking search to try to find a satisfiable
-- under-approximation of the set of input states such that the given 'Program'
-- could transition to a state that satisfies the given reachability
-- proposition. If this could not be done because we got stuck, return the label
-- of the earliest state that we could reach, along with its under-approximation
underReachablePgm :: (SMTSolver solver, Arch arch) =>
                     solver -> Program arch -> ReachProp arch ->
                     Int -> IO (UnderRes arch)
underReachablePgm solver pgm rprop = runM (helper solver Proxy pgm rprop) where

  helper :: (SMTSolver solver, Arch arch) =>
            solver -> Proxy arch -> Program arch -> ReachProp arch ->
            UnderM arch (ArchStateProp arch)
  helper solver proxy (BasicBlock label pm) rprop =
    satCoreStream solver proxy label $ reachablePM pm rprop
  helper solver proxy (SeqBlock _ pgm1 pgm2) rprop =
    -- Either pgm1 reaches an initial state of pgm2 that leads to rprop, OR pgm1
    -- reaches a non-success exit
    --
    -- FIXME HERE: need a fair interleaving here
    (do sprop1 <- helper solver proxy pgm2 rprop
        helper solver proxy pgm1 [(Right (), sprop1)])
    `mdisj`
    helper solver proxy pgm1 (updateReachProp proxy (Right ())
                              (falseStateProp proxy) rprop)
  helper solver proxy (OrBlock _ pgm1 pgm2) rprop =
    -- Either pgm1 or pgm2 reaches rprop
    --
    -- FIXME HERE: need a fair interleaving here
    helper solver proxy pgm1 rprop
    `mdisj`
    helper solver proxy pgm2 rprop
  helper solver proxy (CatchBlock _ exn pgm1 pgm2) rprop =
    -- Either pgm1 reaches an exit other than exn in rprop, or it reaches an
    -- initial state of pgm2 which then leads to rprop
    --
    -- FIXME HERE: need a fair interleaving here
    helper solver proxy pgm1 (updateReachProp proxy (Left exn)
                              (falseStateProp proxy) rprop)
    `mdisj`
    (do sprop1 <- helper solver proxy pgm2 rprop
        helper solver proxy pgm1 [(Left exn, sprop1)])
  helper solver proxy (LoopBlock label pgm) rprop =
    -- Either the loop transitions 0 times, or it transitions 0 or more times
    -- successfully (without any exceptions) to a state from which it
    -- transitions one more time to rprop. NOTE: this does *not* need fair
    -- interleaving.
    let go 0 sprop = return sprop
        go n sprop =
          return sprop
          `mdisj`
          helper solver proxy pgm [(Right (), sprop)] >>= go (n-1) in
    return (case lookup (Right ()) rprop of
               Just sprop -> sprop
               Nothing -> falseStateProp proxy)
    `mdisj`
    (do sprop <- helper solver proxy pgm rprop
        max_iters <- UnderM ask
        go max_iters sprop)


-- | Test if a program can reach a set of "bad" states given by a reachability
-- proposition. If so, return an initial 'Memory' that leads to a bad state, and
-- if not, return 'Nothing'. If we do not know, return the earliest point in a
-- program that we could show leads to bad state.
reachablePgm :: (SMTSolver solver, Arch arch) => Int -> solver ->
                Program arch -> ReachProp arch ->
                IO (Maybe (Either (UnderFail arch)
                           (Memory (ArchStorables arch))))
reachablePgm iters solver (pgm :: Program arch) rprop =
  do let proxy = Proxy :: Proxy arch
     over_sprop <- overReachablePgm solver pgm rprop
     over_smt_res <- smtSolveStateProp solver proxy over_sprop
     case over_smt_res of
       SMT_unsat ->
         -- The over-approximation is unsatisfiable, so the bad states are
         -- unreachable!
         return Nothing
       SMT_unknown str -> error $ "SMT error:" ++ str
       SMT_sat _ ->
         runM (underReachablePgm solver pgm rprop iters) >>= \under_res ->
         case under_res of
           Right under_sprop ->
             do under_smt_res <- smtSolveStateProp solver proxy under_sprop
                case under_smt_res of
                  SMT_sat (init_mem, _) -> return $ Just $ Right init_mem
                  SMT_unknown str -> error $ "SMT error:" ++ str
                  _ ->
                    error "underReachablePgm returned an unsatisfiable formula!"
           Left under_fail -> return $ Just $ Left under_fail
