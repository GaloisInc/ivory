{-# LANGUAGE GADTs, KindSignatures, EmptyDataDecls, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds, PartialTypeSignatures #-}

module Ivory.ModelCheck.Logic where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Proxy
import Data.Typeable
import Data.Int
import Data.Word
import Data.List
-- import qualified Data.Bits as Bits
import Numeric.Natural
import Data.Functor.Identity

import Data.Binding.Hobbits
import Data.Type.RList

import           Ivory.Language.Syntax.Type
import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.Pretty


----------------------------------------------------------------------
-- Utilities for type lists
----------------------------------------------------------------------

-- | A heterogeneous list containing one element of type @f a@ for each type @a@
-- in the given type list.
data MapList f (l :: [*]) where
  Nil :: MapList f '[]
  Cons :: f a -> MapList f l -> MapList f (a ': l)

-- | Get the first element of a 'MapList'
ml_first :: MapList f (a ': l) -> f a
ml_first (Cons x _) = x

-- | Get the second element of a 'MapList'
ml_second :: MapList f (a ': b ': l) -> f b
ml_second (Cons _ (Cons y _)) = y

-- | Get the third element of a 'MapList'
ml_third :: MapList f (a ': b ': c ': l) -> f c
ml_third (Cons _ (Cons _ (Cons z _))) = z

-- | Map a function over a 'MapList'
ml_map :: (forall x. f x -> g x) -> MapList f l -> MapList g l
ml_map f Nil = Nil
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

instance Elem (a ': l) a where elemPf = Elem_base
instance Elem l b => Elem (a ': l) b where elemPf = Elem_cons elemPf

-- | Look up an element of a 'MapList' using an 'ElemPf'
ml_lookup :: MapList f l -> ElemPf l a -> f a
ml_lookup (Cons x _) Elem_base = x
ml_lookup (Cons _ l) (Elem_cons pf) = ml_lookup l pf

-- | Apply a function to a specific element of a 'MapList' given by an 'ElemPf'
ml_map1 :: (f a -> f a) -> MapList f l -> ElemPf l a -> MapList f l
ml_map1 f (Cons x l) Elem_base = Cons (f x) l
ml_map1 f (Cons x l) (Elem_cons pf) = Cons x $ ml_map1 f l pf


----------------------------------------------------------------------
-- Types used in our logic
----------------------------------------------------------------------

-- | Container type for literals
newtype Literal a = Literal { unLiteral :: a }

-- | Dummy type for propositions
data Prop deriving Typeable

-- | Dummy type for predicate monads, i.e., set of program transitions
data PM (a :: *) deriving Typeable

-- | Untyped pointers = 'Natural's
newtype Ptr = Ptr { unPtr :: Natural } deriving (Typeable, Eq, Ord)


----------------------------------------------------------------------
-- The types of our logic
----------------------------------------------------------------------

-- | A GADT for the types allowed in our logic
data LType (a :: *) where
  LType_Literal :: Typeable a => LType (Literal a)
  LType_Prop :: LType Prop
  LType_Fun :: LType a -> LType b -> LType (a -> b)
  LType_PM :: LType a -> LType (PM a)

-- | Typeclass for 'LType'
class LTypeable a where
  ltypeRep :: LType a

instance Typeable a => LTypeable (Literal a) where
  ltypeRep = LType_Literal
instance LTypeable Prop where ltypeRep = LType_Prop
instance (LTypeable a, LTypeable b) => LTypeable (a -> b) where
  ltypeRep = LType_Fun ltypeRep ltypeRep
instance LTypeable a => LTypeable (PM a) where ltypeRep = LType_PM ltypeRep

-- Build a NuMatching instance for LType, needed for Liftable
$(mkNuMatching [t| forall a. LType a |])

-- Liftable instance, to lift LTypes out of binding contexts
instance Liftable (LType a) where
  mbLift [nuP| LType_Literal |] = LType_Literal
  mbLift [nuP| LType_Prop |] = LType_Prop
  mbLift [nuP| LType_Fun t1 t2 |] = LType_Fun (mbLift t1) (mbLift t2)
  mbLift [nuP| LType_PM t |] = LType_PM (mbLift t)

-- | Test if two 'LType's are equal
ltypeEq :: LType a -> LType b -> Maybe (a :~: b)
ltypeEq LType_Literal LType_Literal = eqT
ltypeEq LType_Literal _ = Nothing
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
-- The types of functions over first-order types
----------------------------------------------------------------------

-- | Types for functions over first-order types
data LitFunType a where
  LitFunType_base :: Typeable a => LitFunType (Literal a)
  LitFunType_cons :: Typeable a => LitFunType b -> LitFunType (Literal a -> b)

-- | Typeclass for 'LitFunType'
class LitFunTypeable a where
  litFunTypeRep :: LitFunType a

instance Typeable a => LitFunTypeable (Literal a) where
  litFunTypeRep = LitFunType_base
instance (Typeable a, LitFunTypeable b) => LitFunTypeable (Literal a -> b) where
  litFunTypeRep = LitFunType_cons litFunTypeRep

-- Build a NuMatching instance for LitFunType, needed for Liftable
$(mkNuMatching [t| forall a. LitFunType a |])

-- Liftable instance, to lift LitFunTypes out of binding contexts
instance Liftable (LitFunType a) where
  mbLift [nuP| LitFunType_base |] = LitFunType_base
  mbLift [nuP| LitFunType_cons t |] = LitFunType_cons (mbLift t)

-- | Convert a 'LitFunType' to an 'LType'
fun2typeRep :: LitFunType a -> LType a
fun2typeRep LitFunType_base = LType_Literal
fun2typeRep (LitFunType_cons t) = LType_Fun LType_Literal (fun2typeRep t)


----------------------------------------------------------------------
-- Finite functinos
----------------------------------------------------------------------

-- | A finite function, i.e., a function that has the same value for all but
-- finitely many inputs
data FinFun a b = FinFun { finfunDef :: b, finfunMap :: [(a,b)] }

-- | Make a finite function with a given default value
mkFinFun :: b -> FinFun a b
mkFinFun def = FinFun { finfunDef = def, finfunMap = [] }

-- | Apply a 'FinFun'
applyFinFun :: Eq a => FinFun a b -> a -> b
applyFinFun f arg =
  case lookup arg (finfunMap f) of
    Just ret -> ret
    Nothing -> finfunDef f

-- | Update a 'FinFun'
updateFinFun :: Eq a => FinFun a b -> a -> b -> FinFun a b
updateFinFun f arg newval =
  FinFun { finfunDef = finfunDef f,
           finfunMap =
             (arg,newval):(deleteBy
                           (\p1 p2 -> fst p1 == fst p2)
                           (arg,newval)
                           (finfunMap f))}


----------------------------------------------------------------------
-- The memory model of our logic
----------------------------------------------------------------------

-- | Type class stating that @mm@ is a list of the types that are considered
-- storable, and also a list of default values for each of these types.
class MemoryModel (mm :: [*]) where
  memoryDefaults :: MapList Identity mm

-- | An array store represents a set of (infinite) arrays of some given
-- type. These are represented as 'FinFun's that map 'Ptr' pointer values,
-- combined with 'Word64' indices, to the given type.
type ArrayStore a = FinFun (Ptr, Word64) a

-- | Build a default, empty 'ArrayStore'
mkArrayStore :: a -> ArrayStore a
mkArrayStore = mkFinFun

-- | A memory is a collection of 'ArrayStore's, one for each type in the memory
-- model, which is given as a list of types that can be stored. Memories also
-- associate a length with each 'Ptr', and also track that last allocated 'Ptr'
-- value, where any 'Ptr' greater than the last allocated one is not considered
-- allcated, i.e., is an invalid pointer.
data Memory mm =
  Memory { memArrays :: MapList (FinFun (Ptr, Word64)) mm,
           memLengths :: FinFun Ptr Word64,
           memLastAlloc :: Ptr }

-- | Build a default, empty 'Memory'
mkMemory :: MemoryModel mm => Memory mm
mkMemory =
  Memory { memArrays = ml_map (\(Identity x) -> mkFinFun x) memoryDefaults,
           memLengths = mkFinFun 0,
           memLastAlloc = Ptr 0 }

-- | The read operations for 'Memory's
data ReadOp mm args ret where
  -- | Read the nth element of an array, which can be any 'StorableType'
  ReadOp_array :: ElemPf mm a -> ReadOp mm '[ Ptr, Word64 ] a
  -- | Get the length of an array as a 'Word64'
  ReadOp_length :: ReadOp mm '[ Ptr ] Word64
  -- | Get the last-allocated pointer
  ReadOp_last_alloc :: ReadOp mm '[] Ptr

-- | The update operations for 'Memory's
data UpdateOp mm args where
  -- | Update the nth element of an array
  UpdateOp_array :: ElemPf mm a -> UpdateOp mm '[ Ptr, Word64, a ]
  -- | Allocate a new array with a given length
  UpdateOp_alloc :: ElemPf mm a -> UpdateOp mm '[ Word64 ]

-- | Perform a read operation on a 'Memory'
readMemory :: ReadOp mm args ret -> Memory mm -> MapList Literal args -> ret
readMemory (ReadOp_array elem_pf) mem args =
  applyFinFun
    (ml_lookup (memArrays mem) elem_pf)
    (unLiteral (ml_first args),
     unLiteral (ml_second args))
readMemory ReadOp_length mem args =
  applyFinFun (memLengths mem) (unLiteral $ ml_first args)
readMemory ReadOp_last_alloc mem args = memLastAlloc mem

-- | Perform an update operation on a 'Memory'
updateMemory :: UpdateOp mm args -> Memory mm -> MapList Literal args ->
                Memory mm
updateMemory (UpdateOp_array elem_pf) mem args =
  let (ptr,ix,newval) = (unLiteral (ml_first args),
                         unLiteral (ml_second args),
                         unLiteral (ml_third args)) in
  mem
  { memArrays =
      ml_map1
        (\ff -> updateFinFun ff (ptr,ix) newval)
        (memArrays mem)
        elem_pf
  }
updateMemory (UpdateOp_alloc elem_pf) mem args =
  let new_last_alloc = Ptr (unPtr (memLastAlloc mem) + 1) in
  let new_len = unLiteral $ ml_first args in
  mem
  {
    memLengths = updateFinFun (memLengths mem) new_last_alloc new_len,
    memLastAlloc = new_last_alloc
  }


----------------------------------------------------------------------
-- The built-in operations of our logic
----------------------------------------------------------------------

-- | Type class associating meta-data with @tag@ that is needed by our logic
class MemoryModel (Storables tag) => LExprTag tag where
  type Storables tag :: [*]

-- | Type family to add a list of types as arguments to a function return type
type family AddLitArrows (args :: [*]) (ret :: *) :: *
type instance AddLitArrows '[] ret = ret
type instance AddLitArrows (a ': args) ret = Literal a -> AddLitArrows args ret

-- | The unary arithmetic operations
data ArithOp1 = Op1_Abs | Op1_Signum | Op1_Neg | Op1_Complement

-- | The binary arithmetic operations
data ArithOp2
  = Op2_Add | Op2_Sub | Op2_Mult | Op2_Div | Op2_Mod
  | Op2_BitAnd | Op2_BitOr | Op2_BitXor

-- | The arithmetic comparison operations
data ArithCmp
  = OpCmp_EQ -- ^ Equality
  | OpCmp_LT -- ^ Less than
  | OpCmp_LE -- ^ Less than or equal to
    

-- | The operations / function symbols of our logic
data Op tag a where
  -- | Literals that are lifted from Haskell
  Op_Literal :: (Liftable a, Typeable a) => a -> Op tag (Literal a)

  -- * First-order operations on data

  -- | Unary arithmetic
  Op_arith1 :: Typeable a => ArithOp1 -> Op tag (Literal a -> Literal a)
  -- | Binary arithmetic
  Op_arith2 :: Typeable a => ArithOp2 ->
               Op tag (Literal a -> Literal a -> Literal a)
  -- | Coercion between types
  Op_coerce :: (Typeable a, Typeable b) => Op tag (Literal a -> Literal b)
  -- | Comparison operations
  Op_cmp :: Typeable a => ArithCmp ->
            Op tag (Literal a -> Literal a -> Literal Bool)

  -- * Propositional operations
  -- | Logical and
  Op_and :: Op tag (Prop -> Prop -> Prop)
  -- | Logical or
  Op_or :: Op tag (Prop -> Prop -> Prop)
  -- | Logical negation
  Op_not :: Op tag (Prop -> Prop)
  -- | Lift a 'Bool' to a 'Prop'
  Op_istrue  :: Op tag (Literal Bool -> Prop)

  -- | Let-bindings are only allowed at the top level, i.e., in propositions
  Op_Let :: Typeable a => Op tag ((Literal a -> Prop) -> Prop)
  -- | Let-bind the result of reading from a 'Memory'
  Op_LetRead :: ReadOp (Storables tag) args ret ->
                Op tag (AddLitArrows args
                        (Memory (Storables tag) -> (ret -> Prop) -> Prop))

  -- | Memory read operations
  Op_readP :: ReadOp (Storables tag) args ret ->
              Op tag (AddLitArrows args (PM (Literal ret)))
  -- | Memory update operations
  Op_updateP :: UpdateOp (Storables tag) args ->
                Op tag (AddLitArrows args (PM (Literal ())))
  -- | Assertions about the current 'Memory'
  Op_assertP :: Op tag ((Literal (Memory (Storables tag)) -> Prop) ->
                        PM (Literal ()))
  -- | Disjunctions
  Op_orP :: Op tag (PM (Literal ()) -> PM (Literal ()) -> PM (Literal ()))

-- Build a NuMatching instances for Op and friends
$(mkNuMatching [t| ArithOp1 |])
$(mkNuMatching [t| ArithOp2 |])
$(mkNuMatching [t| ArithCmp |])
$(mkNuMatching [t| forall mm args ret. ReadOp mm args ret |])
$(mkNuMatching [t| forall mm args. UpdateOp mm args |])
$(mkNuMatching [t| forall tag a. Op tag a |])

-- Build Liftable instances for Op and friends
instance Liftable ArithOp1 where
  mbLift [nuP| Op1_Abs |] = Op1_Abs
  mbLift [nuP| Op1_Signum |] = Op1_Signum
  mbLift [nuP| Op1_Neg |] = Op1_Neg
  mbLift [nuP| Op1_Complement |] = Op1_Complement
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
  mbLift [nuP| ReadOp_length |] = ReadOp_length
  mbLift [nuP| ReadOp_last_alloc |] = ReadOp_last_alloc
instance Liftable (UpdateOp mm args) where
  mbLift [nuP| UpdateOp_array elem_pf |] = UpdateOp_array $ mbLift elem_pf
  mbLift [nuP| UpdateOp_alloc elem_pf |] = UpdateOp_alloc $ mbLift elem_pf
instance Liftable (Op tag a) where
  mbLift [nuP| Op_Literal x |] = Op_Literal $ mbLift x
  mbLift [nuP| Op_arith1 aop |] = Op_arith1 $ mbLift aop
  mbLift [nuP| Op_arith2 aop |] = Op_arith2 $ mbLift aop
  mbLift [nuP| Op_coerce |] = Op_coerce
  mbLift [nuP| Op_cmp acmp |] = Op_cmp $ mbLift acmp
  mbLift [nuP| Op_and |] = Op_and
  mbLift [nuP| Op_or |] = Op_or
  mbLift [nuP| Op_not |] = Op_not
  mbLift [nuP| Op_istrue |] = Op_istrue
  mbLift [nuP| Op_Let |] = Op_Let
  mbLift [nuP| Op_LetRead read_op |] = Op_LetRead $ mbLift read_op
  mbLift [nuP| Op_readP read_op |] = Op_readP $ mbLift read_op
  mbLift [nuP| Op_updateP update_op |] = Op_updateP $ mbLift update_op
  mbLift [nuP| Op_assertP |] = Op_assertP
  mbLift [nuP| Op_orP |] = Op_orP


----------------------------------------------------------------------
-- The expressions of our logic
----------------------------------------------------------------------

-- | The expressions of our logic as a GADT. This is essentially the typed
-- lambda-calculus with function symbols. All expressions are in beta-normal
-- form, which is enforced by restricting the expressions to only the
-- lambda-abstractions and the applications of variables and function symbols.
data LExpr tag a where
  LLambda :: LType a -> Binding a (LExpr tag b) -> LExpr tag (a -> b)
  LAppExpr :: LAppExpr tag a -> LExpr tag a

-- | Expressions that are applications of variables or function symbols.
data LAppExpr tag a where
  LVar :: Name a -> LAppExpr tag a
  LOp :: Op tag a -> LAppExpr tag a
  LApp :: LAppExpr tag (a -> b) -> LExpr tag a -> LAppExpr tag b

$(mkNuMatching [t| forall tag a. LExpr tag a |])
$(mkNuMatching [t| forall tag a. LAppExpr tag a |])


----------------------------------------------------------------------
-- Building expressions
----------------------------------------------------------------------

-- | Helper function for building lambda-abstractions
mkLambda :: LTypeable a => (LExpr tag a -> LExpr tag b) -> LExpr tag (a -> b)
mkLambda f = LLambda ltypeRep $ nu $ \x -> f (LAppExpr (LVar x))

-- | Apply type function @f@ to the input and outputs of a function type, i.e.,
-- replace @a1 -> ... -> an -> b@ with @f a1 -> ... -> f an -> f b@.
type family ApplyToArgs (f :: * -> *) a :: *
type instance ApplyToArgs f (Literal a) = f (Literal a)
type instance ApplyToArgs f Prop = f Prop
type instance ApplyToArgs f (PM a) = f (PM a)
type instance ApplyToArgs f (a -> b) = f a -> ApplyToArgs f b

-- | Build an eta-expanded term-building function
etaBuild :: LType a -> LAppExpr tag a -> ApplyToArgs (LExpr tag) a
etaBuild LType_Literal e = LAppExpr e
etaBuild LType_Prop e = LAppExpr e
etaBuild (LType_PM _) e = LAppExpr e
etaBuild (LType_Fun _ tp2) e = \x -> etaBuild tp2 (LApp e x)

-- | Helper function for building literal expressions
mkLiteral :: (Typeable a, Liftable a) => a -> LExpr tag (Literal a)
mkLiteral a = etaBuild LType_Literal $ LOp $ Op_Literal a

-- | Helper function for building expression functions from 'Op's
mkOp :: LTypeable a => Op tag a -> ApplyToArgs (LExpr tag) a
mkOp op = etaBuild ltypeRep (LOp op)

-- Num instance allows us to use arithmetic operations to build expressions.
instance (Typeable a, Liftable a, Num a) => Num (LExpr tag (Literal a)) where
  (+) = mkOp (Op_arith2 Op2_Add)
  (-) = mkOp (Op_arith2 Op2_Sub)
  (*) = mkOp (Op_arith2 Op2_Mult)
  abs = mkOp (Op_arith1 Op1_Abs)
  signum = mkOp (Op_arith1 Op1_Signum)
  fromInteger i = mkLiteral (fromInteger i)


----------------------------------------------------------------------
-- Interpreting expressions
----------------------------------------------------------------------

-- | Type class stating that @f@ commutes with the arrow type constructor
class CommutesWithArrow f where
  interpApply :: f (a -> b) -> f a -> f b
  interpLambda :: (f a -> f b) -> f (a -> b)

-- | An expression @f@-algebra shows how to convert any 'Op' of type @a@ to an
-- element of type @f a@. It also requires that @f@ commutes with arrow.
class CommutesWithArrow f => LExprAlgebra tag (f :: * -> *) where
  interpOp :: Op tag a -> f a

-- | Interpret an 'LExpr' to another functor @f@ using an @f@-algebra
interpExpr :: LExprAlgebra tag f =>
              MapRList f ctx -> Closed (Mb ctx (LExpr tag a)) -> f a
interpExpr ctx [clNuP| LLambda _ body |] =
  interpLambda $ \x ->
  interpExpr (ctx :>: x) (clApply $(mkClosed [| mbCombine |]) body)
interpExpr ctx [clNuP| LAppExpr e |] = interpAppExpr ctx e

-- | Interpret an 'LAppExpr' to another functor @f@ using an @f@-algebra
interpAppExpr :: LExprAlgebra tag f =>
                 MapRList f ctx -> Closed (Mb ctx (LAppExpr tag a)) -> f a
interpAppExpr ctx [clNuP| LVar n |] =
  case clApply $(mkClosed [| mbNameBoundP |]) n of
    [clP| Left memb |] -> hlistLookup (unClosed memb) ctx
    [clP| Right closed_n |] -> noClosedNames closed_n
interpAppExpr ctx [clNuP| LOp op |] = interpOp (mbLift $ unClosed op)
interpAppExpr ctx [clNuP| LApp f arg |] =
  interpApply (interpAppExpr ctx f) (interpExpr ctx arg)
