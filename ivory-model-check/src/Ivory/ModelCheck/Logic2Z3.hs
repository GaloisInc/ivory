{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances,
    TypeOperators, DataKinds, EmptyCase, NamedFieldPuns,
    TemplateHaskell, QuasiQuotes, ViewPatterns, RankNTypes #-}

module Ivory.ModelCheck.Logic2Z3 where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Bits
import Data.Word
import Data.List
import Data.Typeable
import Data.Bifunctor

import MonadLib
import MonadLib.Monads
--import MonadLib (StateM(..),StateT(..),runStateT,
--                 ReaderM(..),RunReaderM(..),ReaderT,runReaderT,MonadT(..))
-- import Control.Monad.IO.Class (MonadIO(..))

--import Z3.Monad
import qualified Z3.Monad as Z3
import qualified Z3.Opts as Z3Opts

import Data.Binding.Hobbits
import Data.Type.RList

import Ivory.ModelCheck.Logic


----------------------------------------------------------------------
-- Helper definitions
----------------------------------------------------------------------

-- | FIXME: documentation
joinAListsM :: (Eq key, Monad m) => (val -> val -> m val) ->
              [(key, val)] -> [(key, val)] -> m [(key, val)]
joinAListsM _ [] alist2 = return alist2
joinAListsM join_f ((k, v1) : alist1) alist2 =
  case lookup k alist2 of
    Just v2 ->
      do v_ret <- join_f v1 v2
         alist_ret <-
           joinAListsM join_f alist1 (filter (\(k', _) -> k /= k') alist2)
         return $ (k, v_ret) : alist_ret

groupAList :: Eq key => [(key, val)] -> [(key, [val])]
groupAList = foldr insertHelper [] where
  insertHelper (k,v) ((k',vs):ksvs) =
    if k == k' then ((k',v:vs):ksvs) else (k',vs):(insertHelper (k,v) ksvs)
  insertHelper (k,v) [] = [(k,[v])]


----------------------------------------------------------------------
-- Z3 types
----------------------------------------------------------------------

-- FIXME HERE NOW: remove Z3Type1, as it is equivalent to L1Type
-- | First-order types, i.e., types that are represented in Z3 with 'Z3.AST's
data Z3Type1 a where
  Z3Type_lit :: LitType a -> Z3Type1 (Literal a)
  Z3Type_ptr :: Z3Type1 Ptr
  Z3Type_prop :: Z3Type1 Prop

-- | Typeclass version of 'Z3Type1'
class Z3Type1able a where
  z3type1Rep :: Z3Type1 a

instance LitTypeable a => Z3Type1able (Literal a) where
  z3type1Rep = Z3Type_lit litTypeRep
instance Z3Type1able Ptr where  z3type1Rep = Z3Type_ptr
instance Z3Type1able Prop where  z3type1Rep = Z3Type_prop

-- | Higher-order Z3 types. Note that the use of 'Z3Type1' in the domains of
-- function types technically means these are second-order types.
data Z3Type a where
  Z3Type_base :: Z3Type1 a -> Z3Type a
  Z3Type_fun :: Z3Type1 a -> Z3Type b -> Z3Type (a -> b)

-- | Typeclass version of 'Z3Type'
class Z3Typeable a where
  z3typeRep :: Z3Type a

instance Z3Type1able a => Z3Typeable a where
  z3typeRep = Z3Type_base z3type1Rep
instance (Z3Type1able a, Z3Typeable b) => Z3Typeable (a -> b) where
  z3typeRep = Z3Type_fun z3type1Rep z3typeRep

-- | Convert a @'Z3Type1' a@ to an @'L1Type' a@
z3type1_to_l1type :: Z3Type1 a -> L1Type a
z3type1_to_l1type (Z3Type_lit ltp) = L1Type_lit ltp
z3type1_to_l1type Z3Type_ptr = L1Type_ptr
z3type1_to_l1type Z3Type_prop = L1Type_prop

-- | Convert a @'Z3Type1' a@ to an @'L1Type' a@
l1type_to_z3type1 :: L1Type a -> Z3Type1 a
l1type_to_z3type1 (L1Type_lit ltp) = Z3Type_lit ltp
l1type_to_z3type1 L1Type_ptr = Z3Type_ptr
l1type_to_z3type1 L1Type_prop = Z3Type_prop

-- | Return a base name for each type, to be used for variables
z3type1_base_name :: Z3Type1 a -> String
z3type1_base_name (Z3Type_lit LitType_unit) = "u"
z3type1_base_name (Z3Type_lit LitType_bool) = "b"
z3type1_base_name (Z3Type_lit LitType_int) = "i"
z3type1_base_name (Z3Type_lit LitType_bits) = "bv"
z3type1_base_name Z3Type_prop = "phi"
z3type1_base_name Z3Type_ptr = "ptr"

-- | Proof that there is no element of @'Z3Type1' (a -> b)@
no_functional_z3type1 :: Z3Type1 (a -> b) -> c
no_functional_z3type1 z3tp1 =
  case z3tp1 of { }

-- | Proof that there is no element of @'Z3Type1' ('PM' a)@
no_pm_z3type1 :: Z3Type1 (PM a) -> c
no_pm_z3type1 z3tp1 = case z3tp1 of { }


----------------------------------------------------------------------
-- Typed Z3 expressions
----------------------------------------------------------------------

-- | FIXME: documentation
data Z3Expr a where
  -- * First-order expressions
  Z3Expr_var :: Z3Type1 a -> Name a -> Z3Expr a
  Z3Expr_lit :: LitType a -> a -> Z3Expr (Literal a)
  Z3Expr_prop :: Bool -> Z3Expr Prop
  Z3Expr_ast :: Z3Type1 a -> Z3.AST -> Z3Expr a

  -- * Function applications

  -- | Apply a multi-arity Z3 operation to expressions of the same type
  Z3Expr_multi :: Z3Type1 a -> Z3Type1 b ->
                  ([Z3.AST] -> Z3.Z3 Z3.AST) ->
                  [Z3Expr a] -> Z3Expr b

  -- * Functional expressions
  -- | A Z3 operation applied to 0 or more arguments
  Z3Expr_op :: Z3AppliedOp a -> Z3Expr a

  -- | Lift an arbitrary Haskell function
  Z3Expr_fun :: (Z3Expr a -> Z3Expr b) -> Z3Expr (a -> b)

  -- * Propositional connectives
  Z3Expr_forall :: Z3Type1 a -> Binding a (Z3Expr Prop) -> Z3Expr Prop
  Z3Expr_exists :: Z3Type1 a -> Binding a (Z3Expr Prop) -> Z3Expr Prop
  Z3Expr_and :: [Z3Expr Prop] -> Z3Expr Prop
  Z3Expr_or :: [Z3Expr Prop] -> Z3Expr Prop
  Z3Expr_true :: Z3Expr Prop
  Z3Expr_false :: Z3Expr Prop

-- | FIXME: documentation
data Z3AppliedOp a where
  -- | Lift a 'Z3.FuncDecl'
  Z3Expr_fdecl :: Z3Type a -> Z3.FuncDecl -> Z3AppliedOp a
  -- | Lift a unary operation from Z3
  Z3Expr_op1 :: Z3Type1 a -> Z3Type1 b -> (Z3.AST -> Z3.Z3 Z3.AST) ->
                Z3AppliedOp (a -> b)
  -- | Lift a binary operation from Z3
  Z3Expr_op2 :: Z3Type1 a -> Z3Type1 b -> Z3Type1 c ->
                (Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST) ->
                Z3AppliedOp (a -> b -> c)
  -- | Apply an applied op to an expression
  Z3Expr_app :: Z3AppliedOp (a -> b) -> Z3Expr a -> Z3AppliedOp b

-- | Convenient short-hand: a Z3 proposition is a 'Z3Expr' of type 'Prop'
type Z3Prop = Z3Expr Prop

-- | Apply a 'Z3Expr' to another one
(@@) :: Z3Expr (a -> b) -> Z3Expr a -> Z3Expr b
(Z3Expr_var tp1 _) @@ _ = no_functional_z3type1 tp1
(Z3Expr_ast tp1 _) @@ _ = no_functional_z3type1 tp1
(Z3Expr_multi _ tp1 _ _) @@ _ = no_functional_z3type1 tp1
(Z3Expr_op f) @@ arg = Z3Expr_op $ Z3Expr_app f arg
(Z3Expr_fun f) @@ arg = f arg

-- | Build a typed 'Z3Expr' from a 'Z3.FuncDecl'
z3fdecl :: Z3Type a -> Z3.FuncDecl -> Z3Expr a
z3fdecl z3tp fdecl = Z3Expr_op $ Z3Expr_fdecl z3tp fdecl

-- Use the Z3 operations to build a Num instance for any Z3Expr type
instance (Num a, LitTypeable a) => Num (Z3Expr (Literal a)) where
  e1 + e2 = Z3Expr_multi z3type1Rep z3type1Rep Z3.mkAdd [e1,e2]
  e1 - e2 = Z3Expr_multi z3type1Rep z3type1Rep Z3.mkSub [e1,e2]
  e1 * e2 = Z3Expr_multi z3type1Rep z3type1Rep Z3.mkMul [e1,e2]
  negate e = Z3Expr_op (Z3Expr_op1 z3type1Rep z3type1Rep Z3.mkUnaryMinus) @@ e
  abs e = error "Z3 absolute value function not (yet?) supported"
  signum e = error "Z3 signum function not (yet?) supported"
  fromInteger i = Z3Expr_lit litTypeRep $ fromInteger i

-- | Increment a pointer expression (used to bump the free pointer)
z3ptr_incr :: Z3Expr (Ptr -> Ptr)
z3ptr_incr = Z3Expr_op $ Z3Expr_op1 z3type1Rep z3type1Rep $ \ast ->
  Z3.mkInteger 1 >>= \i -> Z3.mkAdd [ast, i]

-- | Build a universal quantifier
z3forall :: Z3Type1 a -> (Z3Expr a -> Z3Prop) -> Z3Prop
z3forall tp1 body_f = Z3Expr_forall tp1 $ nu $ \n -> body_f $ Z3Expr_var tp1 n

-- | Build an existential quantifier
z3exists :: Z3Type1 a -> (Z3Expr a -> Z3Prop) -> Z3Prop
z3exists tp1 body_f = Z3Expr_exists tp1 $ nu $ \n -> body_f $ Z3Expr_var tp1 n

-- | Build a proposition that two expressions are equal
z3equals :: Z3Type a -> Z3Expr a -> Z3Expr a -> Z3Prop
z3equals (Z3Type_base tp1) e1 e2 =
  (Z3Expr_op $ Z3Expr_op2 tp1 tp1 Z3Type_prop Z3.mkEq) @@ e1 @@ e2
z3equals (Z3Type_fun in_tp1 tp) f1 f2 =
  z3forall in_tp1 (\x -> z3equals tp (f1 @@ x) (f2 @@ x))

-- | Build a 'Z3Prop' from a 'Z3.AST'
z3ast_prop :: Z3.AST -> Z3Prop
z3ast_prop ast = Z3Expr_ast Z3Type_prop ast

-- | Smart constructor for disjunctions (FIXME: check this is right...)
z3or :: [Z3Prop] -> Z3Prop
z3or props =
  case flatten props of
    [] -> Z3Expr_false
    [prop] -> prop
    props' | any (\prop -> case prop of
                     Z3Expr_true -> True
                     _ -> False) props' -> Z3Expr_true
    props' -> Z3Expr_or props'
  where
    flatten = concatMap (\p -> case p of
                            Z3Expr_or props -> flatten props
                            Z3Expr_false -> []
                            _ -> [p])

-- | Smart constructor for conjunction (FIXME: make this actually smart!)
z3and :: [Z3Prop] -> Z3Prop
z3and props = Z3Expr_and props

-- | Smart construct for negation: puts a proposition in negation normal form
z3not :: Z3Prop -> Z3Prop
z3not (Z3Expr_forall z3tp1 body) =
  Z3Expr_exists z3tp1 $ fmap z3not body
z3not (Z3Expr_exists z3tp1 body) =
  Z3Expr_forall z3tp1 $ fmap z3not body
z3not (Z3Expr_and props) = Z3Expr_or $ map z3not props
z3not (Z3Expr_or props) = Z3Expr_and $ map z3not props
z3not Z3Expr_true = Z3Expr_false
z3not Z3Expr_false = Z3Expr_true
z3not e =
  Z3Expr_op $
  Z3Expr_app (Z3Expr_op1 Z3Type_prop Z3Type_prop Z3.mkNot) e


----------------------------------------------------------------------
-- The Z3 representation of memory
----------------------------------------------------------------------

-- | The Z3 representation of memory arrays of a given type
data Z3MemFun a = Z3MemFun (LitType a) Z3.FuncDecl

-- | The representation of a 'Memory' in Z3
data Z3Memory mm =
  Z3Memory
  {
    z3memArrays :: MapList Z3MemFun mm,
    -- ^ One Z3 function for each storable type
    z3memPtrArray :: Z3.FuncDecl,
    -- ^ Z3 function for storing the 'Ptr' type
    z3memLengths :: Z3.FuncDecl,
    -- ^ Z3 function containing the lengths of the various arrays
    z3memLastAlloc :: Z3.AST
    -- ^ Z3 expression containing the last-allocated pointer value
  }

-- | The type of a function used in the 'z3memArrays' field
type Z3MemArrayType a = Ptr -> Literal Word64 -> Literal a

-- | The 'Z3Type' of a function used in the 'z3memArrays' field
z3memArrayType :: LitType a -> Z3Type (Z3MemArrayType a)
z3memArrayType ltp =
  Z3Type_fun Z3Type_ptr $ Z3Type_fun (Z3Type_lit LitType_bits) $
  Z3Type_base $ Z3Type_lit ltp

-- | Extract a typed expression for the 'z3memArrays' field of a 'Z3Memory'
z3memArrayExpr :: ElemPf mm a -> Z3Memory mm -> Z3Expr (Z3MemArrayType a)
z3memArrayExpr elem_pf mem =
  case ml_lookup (z3memArrays mem) elem_pf of
    Z3MemFun ltp fdecl -> z3fdecl (z3memArrayType ltp) fdecl

-- | The type of a function used in the 'z3memPtrArray' field
type Z3MemPtrArrayType = Ptr -> Literal Word64 -> Ptr -> Prop

-- | The 'Z3Type' of a function used in the 'z3memPtrArray' field
z3memPtrArrayType :: Z3Type Z3MemPtrArrayType
z3memPtrArrayType =
  Z3Type_fun Z3Type_ptr $ Z3Type_fun (Z3Type_lit LitType_bits) $
  Z3Type_fun Z3Type_ptr $ Z3Type_base Z3Type_prop

-- | Extract a typed expression for the 'z3memPtrArray' field of a 'Z3Memory'
z3memPtrArrayExpr :: Z3Memory mm -> Z3Expr Z3MemPtrArrayType
z3memPtrArrayExpr mem = z3fdecl z3memPtrArrayType $ z3memPtrArray mem

-- | The type of a function used in the 'z3memLengths' field
type Z3MemLengthsType = Ptr -> Literal Word64

-- | The 'Z3Type' of a function used in the 'z3memLengths' field
z3memLengthsType :: Z3Type Z3MemLengthsType
z3memLengthsType =
  Z3Type_fun Z3Type_ptr $ Z3Type_base $ Z3Type_lit LitType_bits

-- | Extract a typed expression for the 'z3memLengths' field of a 'Z3Memory'
z3memLengthsExpr :: Z3Memory mm -> Z3Expr Z3MemLengthsType
z3memLengthsExpr mem = z3fdecl z3memLengthsType $ z3memLengths mem

-- | The type of the 'z3memLastAlloc' field of a 'Z3Memory'
type Z3MemLastAllocType = Ptr

-- | The 'Z3Type1' of the 'z3memLastAlloc' field of a 'Z3Memory'
z3memLastAllocType :: Z3Type1 Z3MemLastAllocType
z3memLastAllocType = Z3Type_ptr

-- | Extract a typed expression for the 'z3memLastAlloc' field of a 'Z3Memory'
z3memLastAllocExpr :: Z3Memory mm -> Z3Expr Z3MemLastAllocType
z3memLastAllocExpr mem = Z3Expr_ast Z3Type_ptr $ z3memLastAlloc mem

-- | Make the proposition that two 'Z3Memory's are equal
z3memEquals :: Z3Memory mm -> Z3Memory mm -> Z3Prop
z3memEquals mem1 mem2 =
  z3and $
  memArraysEqual (z3memArrays mem1) (z3memArrays mem2) ++
  [eq_helper z3fdecl z3memPtrArrayType (z3memPtrArray mem1) (z3memPtrArray mem2),
   eq_helper z3fdecl z3memLengthsType (z3memLengths mem1) (z3memLengths mem2),
   eq_helper (\_ ast -> Z3Expr_ast z3memLastAllocType ast)
   (Z3Type_base z3memLastAllocType) (z3memLastAlloc mem1) (z3memLastAlloc mem2)]
  where
    memArraysEqual :: MapList Z3MemFun mm -> MapList Z3MemFun mm -> [Z3Prop]
    memArraysEqual Nil Nil = []
    memArraysEqual (Cons (Z3MemFun ltp f1) as1) (Cons (Z3MemFun _ f2) as2) =
      eq_helper z3fdecl (z3memArrayType ltp) f1 f2 : memArraysEqual as1 as2
    eq_helper :: Eq ast => (Z3Type a -> ast -> Z3Expr a) ->
                 Z3Type a -> ast -> ast -> Z3Prop
    eq_helper expr_f tp ast1 ast2 =
      if ast1 == ast2 then Z3Expr_true
      else z3equals tp (expr_f tp ast1) (expr_f tp ast2)


----------------------------------------------------------------------
-- A monad for fresh Z3 constants and function declarations
----------------------------------------------------------------------

-- | FIXME: documentation
data Z3Decl a where
  -- | A constant, of type @b@, with no definition
  Z3Decl_const :: Z3Type1 b -> Z3Decl Z3.AST
  -- | A defined constant of type @b@
  Z3Decl_defn :: Z3Type1 b -> Z3Expr b -> Z3Decl Z3.AST
  -- | A defined propositional constant
  Z3Decl_prop :: Z3Prop -> Z3Decl Z3.AST
  -- | A function constant, of type @b@, with no definition
  Z3Decl_fdecl :: Z3Type b -> Z3Decl Z3.FuncDecl

data WithZ3Decls a where
  WithNoZ3Decls :: a -> WithZ3Decls a
  WithZ3Decl :: Z3Decl b -> (b -> WithZ3Decls a) -> WithZ3Decls a

instance Functor WithZ3Decls where
  fmap f (WithNoZ3Decls a) = WithNoZ3Decls (f a)
  fmap f (WithZ3Decl decl body_f) = WithZ3Decl decl (\x -> fmap f $ body_f x)

instance Applicative WithZ3Decls where

instance Monad WithZ3Decls where
  return x = WithNoZ3Decls x
  (WithNoZ3Decls x) >>= f = f x
  (WithZ3Decl decl body_f) >>= f = WithZ3Decl decl (\x -> (body_f x) >>= f)

class Monad m => Z3DeclsMonad m where
  freshZ3Decl :: Z3Decl a -> m a

instance Z3DeclsMonad WithZ3Decls where
  freshZ3Decl decl = WithZ3Decl decl WithNoZ3Decls

instance Z3DeclsMonad m => Z3DeclsMonad (StateT s m) where
  freshZ3Decl decl = lift $ freshZ3Decl decl

instance Z3DeclsMonad m => Z3DeclsMonad (ChoiceT m) where
  freshZ3Decl decl = lift $ freshZ3Decl decl

instance Z3DeclsMonad m => Z3DeclsMonad (ExceptionT exn m) where
  freshZ3Decl decl = lift $ freshZ3Decl decl


----------------------------------------------------------------------
-- The Z3 predicate monad
----------------------------------------------------------------------

-- | FIXME: documentation
type Z3PM exn mm =
  ExceptionT exn
  (StateT (Z3Memory mm, [Z3Prop])
   (ChoiceT
    WithZ3Decls))

-- | Get the current 'Z3Memory' in a 'Z3PM' computation
getMem :: Z3PM exn mm (Z3Memory mm)
getMem = liftM fst get

-- | Set the current 'Z3Memory' in a 'Z3PM' computation
setMem :: Z3Memory mm -> Z3PM exn mm ()
setMem mem = sets_ (\(_,props) -> (mem,props))

-- | Assume a 'Z3Prop' in the current 'Z3PM' computation
assumePM :: Z3Prop -> Z3PM exn mm ()
assumePM prop = sets_ $ \(mem, props) -> (mem, prop:props)

-- | Assume that @f@ is a valid function for use as a 'z3memPtrArray' field in a
-- 'Z3Memory'. This means that (fdecl ptr ix ptr') should only hold for at most
-- one ptr' value, for any given ptr and ix values.
assumePtrArrayFDecl :: Z3Expr Z3MemPtrArrayType -> Z3PM exn mm ()
assumePtrArrayFDecl f =
  assumePM $ z3forall Z3Type_ptr $ \p ->
  z3forall (Z3Type_lit LitType_bits) $ \i ->
  z3forall Z3Type_ptr $ \p1 ->
  z3forall Z3Type_ptr $ \p2 ->
  z3or [z3not (f @@ p @@ i @@ p1),
        z3not (f @@ p @@ i @@ p2),
        z3equals z3typeRep p1 p2]

-- | Perform a read operation in the current 'Z3PM' computation
readPM :: ReadOp mm args ret -> MapList Z3Expr args -> Z3PM exn mm (Z3Expr ret)
readPM (ReadOp_array elem_pf) (Cons ptr (Cons ix Nil)) =
  do mem <- getMem
     return $ z3memArrayExpr elem_pf mem @@ ptr @@ ix
readPM ReadOp_ptr_array (Cons ptr (Cons ix Nil)) =
  do mem <- getMem
     ptr_ast <- freshZ3Decl $ Z3Decl_const Z3Type_ptr
     let ptr_ret = Z3Expr_ast Z3Type_ptr ptr_ast
     assumePM $ z3memPtrArrayExpr mem @@ ptr @@ ix @@ ptr_ret
     return ptr_ret
readPM ReadOp_length (Cons ptr Nil) =
  do mem <- getMem
     return $ z3memLengthsExpr mem @@ ptr
readPM ReadOp_last_alloc _ =
  do mem <- getMem
     return $ z3memLastAllocExpr mem

-- | Perform an update operation in the current 'Z3PM' computation
updatePM :: UpdateOp mm args -> MapList Z3Expr args -> Z3PM exn mm ()
updatePM (UpdateOp_array elem_pf) (Cons ptr (Cons ix (Cons v Nil))) =
  do mem <- getMem
     -- Look up the proper fdecl with its output type
     let Z3MemFun ltp fdecl = ml_lookup (z3memArrays mem) elem_pf
     -- Create a fresh fdecl' for the updated memory
     fdecl' <- freshZ3Decl (Z3Decl_fdecl $ z3memArrayType ltp)
     -- Build the updated memory itself
     let mem' = mem { z3memArrays =
                        ml_map1 (\_ -> Z3MemFun ltp fdecl')
                        (z3memArrays mem) elem_pf }
     -- Assert that (fdecl' ptr ix) = v
     assumePM $
       z3equals (Z3Type_base $ Z3Type_lit ltp)
       (z3memArrayExpr elem_pf mem' @@ ptr @@ ix) v
     -- Assert that (fdecl' p i) = (fdecl p i) for p != ptr or i != ix
     assumePM $ z3forall Z3Type_ptr $ \p ->
       z3forall (Z3Type_lit LitType_bits) $ \i ->
       z3or [z3and [z3equals z3typeRep p ptr, z3equals z3typeRep i ix]
            ,
             z3equals (Z3Type_base $ Z3Type_lit ltp)
             (z3memArrayExpr elem_pf mem @@ p @@ i)
             (z3memArrayExpr elem_pf mem' @@ p @@ i)]
     -- Set mem' as the output memory
     setMem mem'
updatePM UpdateOp_ptr_array (Cons ptr (Cons ix (Cons v Nil))) =
  do mem <- getMem
     fdecl' <- freshZ3Decl (Z3Decl_fdecl z3memPtrArrayType)
     let mem' = mem { z3memPtrArray = fdecl' }
     -- Assert that fdecl' is a valid set of pointer arrays
     assumePtrArrayFDecl $ z3memPtrArrayExpr mem'
     -- Assert that (fdecl' ptr ix v) holds
     assumePM $ z3memPtrArrayExpr mem' @@ ptr @@ ix @@ v
     -- Assert that (fdecl' p i p') = (fdecl p i p') for p != ptr or i != ix
     assumePM $ z3forall Z3Type_ptr $ \p ->
       z3forall (Z3Type_lit LitType_bits) $ \i ->
       z3forall Z3Type_ptr $ \p' ->
       z3or [z3and [z3equals z3typeRep p ptr, z3equals z3typeRep i ix]
            ,
             z3equals (Z3Type_base Z3Type_prop)
             (z3memPtrArrayExpr mem @@ p @@ i @@ p')
             (z3memPtrArrayExpr mem' @@ p @@ i @@ p')]
     -- Set mem' as the output memory
     setMem mem'
updatePM (UpdateOp_alloc _) (Cons len Nil) =
  do mem <- getMem
     -- Define a new function for the lengths
     fdecl' <- freshZ3Decl $ Z3Decl_fdecl z3memLengthsType
     -- Define a new variable for last_alloc = old_last_alloc + 1
     ast' <- freshZ3Decl $
       Z3Decl_defn z3memLastAllocType $ z3ptr_incr @@ z3memLastAllocExpr mem
     -- Build the output memory
     let mem' = mem { z3memLengths = fdecl', z3memLastAlloc = ast' }
     -- Assert that (mem_lengths' last_alloc) = len
     assumePM $ z3equals z3typeRep
       (z3memLengthsExpr mem' @@ z3memLastAllocExpr mem')
       len
     -- Assert that (mem_lengths' p) = (mem_lengths p) for p != last_alloc
     assumePM $
       z3forall Z3Type_ptr $ \p ->
       z3or [z3equals z3typeRep p (z3memLastAllocExpr mem')
            ,
             z3equals z3typeRep (z3memLengthsExpr mem' @@ p)
             (z3memLengthsExpr mem @@ p)]
     -- Set mem' as the output memory
     setMem mem'

-- | Raise an exception in a 'Z3PM' computation
raisePM :: exn -> Z3PM exn mm ()
raisePM exn = raise exn

-- | Run the first computation. If it raises the given exception, then run the
-- second computation.
catchPM :: Eq exn => exn -> Z3PM exn mm () -> Z3PM exn mm () -> Z3PM exn mm ()
catchPM exn m m_exn =
  handle m $ \exn' ->
  if exn' == exn then m_exn else raisePM exn'


----------------------------------------------------------------------
-- The orP operation in the predicate monad
----------------------------------------------------------------------

-- | Collect all the possible alternative output states of a 'Z3PM' computation,
-- grouping them together by exception or lack thereof
collectResultsPM :: Eq exn => Z3PM exn mm () ->
                    Z3PM exn mm [(Either exn (), [(Z3Memory mm, [Z3Prop])])]
collectResultsPM pm =
  get >>= \(mem,_) ->
  liftM groupAList $
  lift $ lift $ lift $ findAll $ runStateT (mem, []) $ runExceptionT pm

-- | Get the current propositions for this computation branch and bind a fresh
-- Z3 constant equal to the conjunction of these propositions, returning this
-- fresh Z3 constant after first settin it as the output proposition.
getPropsAsConstant :: Z3PM exn mm Z3Prop
getPropsAsConstant =
  do (mem, props) <- get
     prop_ast <- freshZ3Decl (Z3Decl_prop $ Z3Expr_and props)
     let combined_prop = z3ast_prop prop_ast
     set (mem, [combined_prop])
     return combined_prop

-- | FIXME: documentation
freshMemoryFrom :: [Z3Memory mm] -> Z3PM exn mm (Z3Memory mm)
freshMemoryFrom [] = error "freshMemoryFrom: empty list!"
freshMemoryFrom (mem:mems) =
  foldM (\mem1 mem2 ->
          do z3memArrays <-
               combineMemArrays (z3memArrays mem1) (z3memArrays mem2)
             z3memPtrArray <-
               combineDecls (Z3Decl_fdecl z3memPtrArrayType)
               (z3memPtrArray mem1) (z3memPtrArray mem2)
             z3memLengths <-
               combineDecls (Z3Decl_fdecl z3memLengthsType)
               (z3memLengths mem1) (z3memLengths mem2)
             z3memLastAlloc <-
               combineDecls (Z3Decl_const z3memLastAllocType)
               (z3memLastAlloc mem1) (z3memLastAlloc mem2)
             return $ Z3Memory { z3memArrays, z3memPtrArray,
                                 z3memLengths, z3memLastAlloc }
          ) mem mems
  where
    combineDecls :: Eq a => Z3Decl a -> a -> a -> Z3PM exn mm a
    combineDecls decl a1 a2 =
      if a1 == a2 then return a1 else freshZ3Decl decl
    combineMemArrays :: MapList Z3MemFun mm -> MapList Z3MemFun mm ->
                        Z3PM exn mm' (MapList Z3MemFun mm)
    combineMemArrays Nil Nil = return Nil
    combineMemArrays (Cons (Z3MemFun ltp f1) as1) (Cons (Z3MemFun _ f2) as2) =
      if f1 == f2 then
        combineMemArrays as1 as2 >>= return . Cons (Z3MemFun ltp f1)
      else
        do f' <- freshZ3Decl (Z3Decl_fdecl $ z3memArrayType ltp)
           as' <- combineMemArrays as1 as2
           return $ Cons (Z3MemFun ltp f') as'

-- | FIXME: documentation
canonicalizePM :: Eq exn => Z3PM exn mm () -> Z3PM exn mm ()
canonicalizePM pm =
  do orig_prop <- getPropsAsConstant
     res_alist <- collectResultsPM pm
     foldr mplus mzero $
       map (\(either, mems_with_props) ->
            do mem' <- freshMemoryFrom $ map fst mems_with_props
               set (mem',
                    [orig_prop,
                     z3or $
                     map (\(mem, props) ->
                           z3and (props ++ [z3memEquals mem mem']))
                     mems_with_props
                    ])
               case either of
                 Left exn -> raise exn
                 Right () -> return ())
       res_alist

-- | Disjoin two 'Z3PM' computations
orPM :: Eq exn => Z3PM exn mm () -> Z3PM exn mm () -> Z3PM exn mm ()
orPM pm1 pm2 = canonicalizePM $ pm1 `mplus` pm2


----------------------------------------------------------------------
-- Converting our reachability logic to Z3Exprs
----------------------------------------------------------------------

-- | The type of reachability logic expressions that have been converted to Z3
-- expressions. This type mostly handles the conversion of types in the former
-- logic to those of the latter.
data Logic2Z3 tag a where
  Logic2Z3_lit :: Z3Expr (Literal a) -> Logic2Z3 tag (Literal a)
  Logic2Z3_prop :: Z3Expr Prop -> Logic2Z3 tag Prop
  Logic2Z3_ptr :: Z3Expr Ptr -> Logic2Z3 tag Ptr
  Logic2Z3_fun :: (Logic2Z3 tag a -> Logic2Z3 tag b) -> Logic2Z3 tag (a -> b)
  Logic2Z3_pm :: Z3PM (Maybe (LException tag)) (LStorables tag) (Z3Expr a) ->
                 Logic2Z3 tag (PM a)
  Logic2Z3_pm_unit :: Z3PM (Maybe (LException tag)) (LStorables tag) () ->
                      Logic2Z3 tag (PM (Literal ()))

-- | Build a 'Logic2Z3' from a 'Z3Expr' of base type
logic2Z3_of_expr :: Z3Type1 a -> Z3Expr a -> Logic2Z3 tag a
logic2Z3_of_expr (Z3Type_lit lit_tp) e = Logic2Z3_lit e
logic2Z3_of_expr Z3Type_ptr e = Logic2Z3_ptr e
logic2Z3_of_expr Z3Type_prop e = Logic2Z3_prop e

-- | Extract a 'Z3Expr' from a 'Logic2Z3' of base type
expr_of_logic2Z3 :: Z3Type1 a -> Logic2Z3 tag a -> Z3Expr a
expr_of_logic2Z3 _ (Logic2Z3_lit e) = e
expr_of_logic2Z3 _ (Logic2Z3_ptr e) = e
expr_of_logic2Z3 _ (Logic2Z3_prop e) = e
expr_of_logic2Z3 z3tp1 (Logic2Z3_fun _) = no_functional_z3type1 z3tp1
expr_of_logic2Z3 z3tp1 (Logic2Z3_pm _) = no_pm_z3type1 z3tp1
expr_of_logic2Z3 z3tp1 (Logic2Z3_pm_unit _) = no_pm_z3type1 z3tp1

-- | Extract a 'Z3PM' from a @'Logic2Z3' (PM a)@
pm_of_logic2z3 :: Logic2Z3 tag (PM a) ->
                  Z3PM (Maybe (LException tag)) (LStorables tag) (Z3Expr a)
pm_of_logic2z3 (Logic2Z3_pm m) = m
pm_of_logic2z3 (Logic2Z3_pm_unit m) = m >> return (Z3Expr_lit LitType_unit ())

-- | Extract a unit 'Z3PM' from a @'Logic2Z3' (PM ())@
unit_pm_of_logic2z3 :: Logic2Z3 tag (PM (Literal ())) ->
                       Z3PM (Maybe (LException tag)) (LStorables tag) ()
unit_pm_of_logic2z3 (Logic2Z3_pm_unit m) = m
unit_pm_of_logic2z3 (Logic2Z3_pm m) = m >>= \_ -> return ()

-- | Build a Logic2Z3 from a unary Z3 operation
logic2Z3_op1 :: LitType a -> Z3Type1 b -> (Z3.AST -> Z3.Z3 Z3.AST) ->
                Logic2Z3 tag (Literal a -> b)
logic2Z3_op1 lit_tp z3tp1 e_f =
  Logic2Z3_fun $ \(Logic2Z3_lit e_arg) ->
  logic2Z3_of_expr z3tp1 $
  (Z3Expr_op $ Z3Expr_op1 (Z3Type_lit lit_tp) z3tp1 e_f) @@ e_arg

-- | Build a Logic2Z3 from a binary Z3 operation
logic2Z3_op2 :: LitType a1 -> LitType a2 -> Z3Type1 b ->
                (Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST) ->
                Logic2Z3 tag (Literal a1 -> Literal a2 -> b)
logic2Z3_op2 lit_tp1 lit_tp2 z3tp1 e_f =
  Logic2Z3_fun $ \(Logic2Z3_lit e_arg1) ->
  Logic2Z3_fun $ \(Logic2Z3_lit e_arg2) ->
  logic2Z3_of_expr z3tp1 $
  (Z3Expr_op $
   Z3Expr_op2 (Z3Type_lit lit_tp1) (Z3Type_lit lit_tp2) z3tp1 e_f)
  @@ e_arg1 @@ e_arg2

-- CommutesWithArrow instance for Logic2Z3
instance CommutesWithArrow (Logic2Z3 tag) where
  interpApply (Logic2Z3_fun f) = f
  interpLambda = Logic2Z3_fun

-- LExprAlgebra instance for Logic2Z3
instance LExprAlgebra tag (Logic2Z3 tag) where
  interpOp (Op_Literal ltp x) = Logic2Z3_lit $ Z3Expr_lit ltp x
  interpOp (Op_arith1 ltp aop) =
    logic2Z3_op1 ltp (Z3Type_lit ltp) $ aop1_fun aop
    where
      aop1_fun Op1_Abs = error "Z3 absolute value function not (yet?) supported"
      aop1_fun Op1_Signum = error "Z3 signum function not (yet?) supported"
      aop1_fun Op1_Neg = Z3.mkUnaryMinus
      aop1_fun Op1_Complement = Z3.mkBvnot
  interpOp (Op_arith2 ltp aop) =
    logic2Z3_op2 ltp ltp (Z3Type_lit ltp) $ aop2_fun ltp aop
    where
      aop2_fun _ Op2_Add = \x y -> Z3.mkAdd [x,y]
      aop2_fun _ Op2_Sub = \x y -> Z3.mkSub [x,y]
      aop2_fun _ Op2_Mult = \x y -> Z3.mkMul [x,y]
      aop2_fun (LitType_bits :: LitType a) Op2_Div
        | isSigned (0 :: a) = Z3.mkBvudiv
      aop2_fun (LitType_bits :: LitType a) Op2_Div = Z3.mkBvsdiv
      aop2_fun _ Op2_Div = Z3.mkDiv
      aop2_fun (LitType_bits :: LitType a) Op2_Mod
        | isSigned (0 :: a) =
            error "FIXME: how to translate signed mod into Z3?"
      aop2_fun (LitType_bits :: LitType a) Op2_Mod = Z3.mkBvsmod
      aop2_fun _ Op2_Mod = Z3.mkMod
      aop2_fun (LitType_bits :: LitType a) Op2_Rem
        | isSigned (0 :: a) = Z3.mkBvurem
      aop2_fun (LitType_bits :: LitType a) Op2_Rem = Z3.mkBvsrem
      aop2_fun _ Op2_Rem = Z3.mkRem
      aop2_fun _ Op2_BitAnd = Z3.mkBvand
      aop2_fun _ Op2_BitOr = Z3.mkBvor
      aop2_fun _ Op2_BitXor = Z3.mkBvxor
  interpOp (Op_coerce ltp_from ltp_to) =
    logic2Z3_op1 ltp_from (Z3Type_lit ltp_to) $ coerce_fun ltp_from ltp_to
    where
      coerce_fun :: LitType f -> LitType t -> Z3.AST -> Z3.Z3 Z3.AST
      coerce_fun (LitType_bits :: LitType f) (LitType_bits :: LitType t)
        | finiteBitSize (0 :: t) == finiteBitSize (0 :: f) =
          return
      coerce_fun (LitType_bits :: LitType f) (LitType_bits :: LitType t)
        | isSigned (0 :: f) && finiteBitSize (0 :: t) > finiteBitSize (0 :: f) =
          Z3.mkSignExt $ finiteBitSize (0 :: t) - finiteBitSize (0 :: f)
      coerce_fun (LitType_bits :: LitType f) (LitType_bits :: LitType t)
        | finiteBitSize (0 :: t) > finiteBitSize (0 :: f) =
          Z3.mkZeroExt $ finiteBitSize (0 :: t) - finiteBitSize (0 :: f)
      coerce_fun (LitType_bits :: LitType f) (LitType_bits :: LitType t) =
        Z3.mkExtract (finiteBitSize (0 :: t) - 1) 0
      coerce_fun LitType_int (LitType_bits :: LitType t) =
        Z3.mkInt2bv (finiteBitSize (0 :: t))
      coerce_fun (LitType_bits :: LitType f) LitType_int =
        \ast -> Z3.mkBv2int ast (isSigned (0 :: f))
      coerce_fun LitType_int LitType_int = return
      coerce_fun LitType_bool LitType_bool = return
      coerce_fun LitType_unit LitType_unit = return
      coerce_fun _ _ = error "Z3 Coercion not supported!"
  interpOp (Op_cmp ltp acmp) =
    logic2Z3_op2 ltp ltp (Z3Type_lit LitType_bool) $ cmp_fun acmp
    where
      cmp_fun :: ArithCmp -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST
      cmp_fun OpCmp_EQ = Z3.mkEq
      cmp_fun OpCmp_LT = Z3.mkLt
      cmp_fun OpCmp_LE = Z3.mkLe
  interpOp Op_and =
    Logic2Z3_fun $ \(Logic2Z3_prop p1) ->
    Logic2Z3_fun $ \(Logic2Z3_prop p2) -> Logic2Z3_prop $ z3and [p1, p2]
  interpOp Op_or =
    Logic2Z3_fun $ \(Logic2Z3_prop p1) ->
    Logic2Z3_fun $ \(Logic2Z3_prop p2) -> Logic2Z3_prop $ z3or [p1, p2]
  interpOp Op_not =
    Logic2Z3_fun $ \(Logic2Z3_prop p) -> Logic2Z3_prop $ z3not p
  interpOp Op_istrue =
    logic2Z3_op1 LitType_bool Z3Type_prop return
  interpOp (Op_forall l1tp) =
    Logic2Z3_fun $ \(Logic2Z3_fun body_f) ->
    Logic2Z3_prop $ z3forall (l1type_to_z3type1 l1tp) $ \x ->
    case body_f (logic2Z3_of_expr (l1type_to_z3type1 l1tp) x) of
      Logic2Z3_prop p -> p
  interpOp (Op_exists l1tp) =
    Logic2Z3_fun $ \(Logic2Z3_fun body_f) ->
    Logic2Z3_prop $ z3exists (l1type_to_z3type1 l1tp) $ \x ->
    case body_f (logic2Z3_of_expr (l1type_to_z3type1 l1tp) x) of
      Logic2Z3_prop p -> p
  interpOp (Op_returnP l1tp) =
    Logic2Z3_fun $ \x ->
    Logic2Z3_pm $ return $ expr_of_logic2Z3 (l1type_to_z3type1 l1tp) x
  interpOp (Op_bindP l1tp_a l1tp_b) =
    Logic2Z3_fun $ \m ->
    Logic2Z3_fun $ \(Logic2Z3_fun f) ->
    Logic2Z3_pm (pm_of_logic2z3 m >>= \x ->
                  pm_of_logic2z3 (f $ logic2Z3_of_expr (l1type_to_z3type1 l1tp_a) x))
  interpOp (Op_readP rop@(ReadOp_array elem_pf)) =
    Logic2Z3_fun $ \(Logic2Z3_ptr ptr) -> Logic2Z3_fun $ \(Logic2Z3_lit ix) ->
    Logic2Z3_pm $ readPM rop (Cons ptr (Cons ix Nil))
  interpOp (Op_readP rop@ReadOp_ptr_array) =
    Logic2Z3_fun $ \(Logic2Z3_ptr ptr) -> Logic2Z3_fun $ \(Logic2Z3_lit ix) ->
    Logic2Z3_pm $ readPM rop (Cons ptr (Cons ix Nil))
  interpOp (Op_readP rop@ReadOp_length) =
    Logic2Z3_fun $ \(Logic2Z3_ptr ptr) ->
    Logic2Z3_pm $ readPM rop (Cons ptr Nil)
  interpOp (Op_readP rop@ReadOp_last_alloc) =
    Logic2Z3_pm $ readPM rop Nil
  interpOp (Op_updateP uop@(UpdateOp_array elem_pf)) =
    Logic2Z3_fun $ \(Logic2Z3_ptr ptr) -> Logic2Z3_fun $ \(Logic2Z3_lit ix) ->
    Logic2Z3_fun $ \(Logic2Z3_lit v) ->
    Logic2Z3_pm_unit $ updatePM uop (Cons ptr (Cons ix (Cons v Nil)))
  interpOp (Op_updateP uop@UpdateOp_ptr_array) =
    Logic2Z3_fun $ \(Logic2Z3_ptr ptr) -> Logic2Z3_fun $ \(Logic2Z3_lit ix) ->
    Logic2Z3_fun $ \(Logic2Z3_ptr v) ->
    Logic2Z3_pm_unit $ updatePM uop (Cons ptr (Cons ix (Cons v Nil)))
  interpOp (Op_updateP uop@(UpdateOp_alloc _)) =
    Logic2Z3_fun $ \(Logic2Z3_lit len) ->
    Logic2Z3_pm_unit $ updatePM uop (Cons len Nil)
  interpOp (Op_raiseP maybe_exn) =
    Logic2Z3_pm_unit $ raisePM maybe_exn
  interpOp (Op_catchP exn) =
    Logic2Z3_fun $ \m1 -> Logic2Z3_fun $ \m2 ->
    Logic2Z3_pm_unit $
    catchPM (Just exn) (unit_pm_of_logic2z3 m1) (unit_pm_of_logic2z3 m2)
  interpOp Op_assumeP =
    Logic2Z3_fun $ \(Logic2Z3_prop p) -> Logic2Z3_pm_unit $ assumePM p
  interpOp Op_orP =
    Logic2Z3_fun $ \m1 -> Logic2Z3_fun $ \m2 ->
    Logic2Z3_pm_unit $
    orPM (unit_pm_of_logic2z3 m1) (unit_pm_of_logic2z3 m2)


----------------------------------------------------------------------
-- Z3 variable contexts
----------------------------------------------------------------------

-- | The type of Z3 contexts, that say how to translate free variables into
-- Z3. Specifically, a Z3 context of type @'Z3Ctx' ctx@ indicates, for each
-- variable type listed in @ctx@, whether it is an existential or universal
-- variables. Existential variables are associated with Z3 constant symbols,
-- while universal variables are referred to be deBruijn index, i.e., by their
-- positions in the Z3 context. Z3 contexts can also contain "ghost" universal
-- variables, which are extra universal varibles that have been added to the
-- context whose types are not listed in @ctx@. These come about from lowering
-- Z3 expressions into extended contexts that contain more universal variables
-- than the context in which they were created.
data Z3Ctx ctx where
  Z3Ctx_EVar :: Z3Ctx ctx -> Z3Type1 a -> Z3.AST -> Z3Ctx (ctx :> a)
  -- ^ Add an existential variable to the context, which is associated with a Z3
  -- constant symbol, given as a 'Z3.AST'
  Z3Ctx_UVar :: Z3Ctx ctx -> Z3Type1 a -> Z3Ctx (ctx :> a)
  -- ^ Add a universal variable to the context
  Z3Ctx_GhostUVar :: Z3Ctx ctx -> Z3Ctx ctx
  -- ^ Add a universal variable to the context
  Z3Ctx_nil :: Z3Ctx RNil
  -- ^ The empty Z3 context

-- | Look up a variable in a 'Z3Ctx', returning either a 'Z3.AST', for an
-- existential variable, or a deBruijn index for a universal variable
z3ctx_lookup :: Z3Ctx ctx -> Member ctx a ->
                (Z3Type1 a, Either Z3.AST Integer)
z3ctx_lookup (Z3Ctx_EVar _ z3tp1 ast) Member_Base = (z3tp1, Left ast)
z3ctx_lookup (Z3Ctx_UVar _ z3tp1) Member_Base = (z3tp1, Right 0)
z3ctx_lookup (Z3Ctx_EVar ctx _ _) (Member_Step memb) = z3ctx_lookup ctx memb
z3ctx_lookup (Z3Ctx_UVar ctx _) (Member_Step memb) =
  bimap id (bimap id (+ 1)) $ z3ctx_lookup ctx memb
z3ctx_lookup (Z3Ctx_GhostUVar ctx) memb =
  bimap id (bimap id (+ 1)) $ z3ctx_lookup ctx memb

-- | Lift a 'Z3Ctx' out of the most recent binding. If this most recent binding
-- is a universal variable, preserve it as a "ghost" universal variable, so that
-- the deBruijn indices of the other bound variables do not change.
z3ctx_lift :: Z3Ctx (ctx :> a) -> Z3Ctx ctx
z3ctx_lift (Z3Ctx_EVar ctx _ _) = ctx
z3ctx_lift (Z3Ctx_UVar ctx _) = Z3Ctx_GhostUVar ctx
z3ctx_lift (Z3Ctx_GhostUVar ctx) = Z3Ctx_GhostUVar $ z3ctx_lift ctx


----------------------------------------------------------------------
-- Monad for interacting with Z3
----------------------------------------------------------------------

-- | State information used in constructing Z3 terms
data Z3Info =
  Z3Info
  {
    -- | Index used to create fresh constants and function symbols
    fresh_id :: Integer,
    -- | Sort of the unit type in Z3
    unit_sort :: Z3.Sort,
    -- | Constructor for the unit type
    unit_ctor :: Z3.FuncDecl,
    -- | Sort of the Boolean type in Z3 (also used in Z3 for propositions)
    bool_sort :: Z3.Sort,
    -- | Sort of the 'Integer' type in Z3
    integer_sort :: Z3.Sort,
    -- | Association list of unsigned bit-vector types from their lengths
    unsigned_bv_sorts :: [(Int, Z3.Sort)],
    -- | Association list of signed bit-vector types from their lengths
    signed_bv_sorts :: [(Int, Z3.Sort)]
  }

-- | Monad built on top of 'Z3' for building Z3 expressions relative to a given
-- variable context. The monad uses the state information in a 'Z3Info', and
-- reads a 'Z3Ctx' for the current variable context.
newtype Z3m a =
  Z3m { runZ3m :: StateT Z3Info Z3.Z3 a }
  deriving (Functor,Applicative,Monad)

instance StateM Z3m Z3Info where
  get = Z3m get
  set x = Z3m $ set x

instance BaseM Z3m Z3.Z3 where
  inBase m = Z3m $ lift m

instance RunM Z3.Z3 a (Maybe Z3.Logic -> Z3Opts.Opts -> IO a) where
  runM m = \logic opts -> Z3.evalZ3With logic opts m

instance RunM Z3m a (Z3Info -> Maybe Z3.Logic -> Z3Opts.Opts ->
                     IO (a, Z3Info)) where
  runM m = runM $ runZ3m m


----------------------------------------------------------------------
-- Converting logical types to Z3 types and sorts
----------------------------------------------------------------------

-- | Get the 'Z3.Sort' for a 'Z3Type1'
z3type1_to_sort :: Z3Type1 a -> Z3m Z3.Sort
z3type1_to_sort (Z3Type_lit LitType_unit) = liftM unit_sort get
z3type1_to_sort (Z3Type_lit LitType_bool) = liftM bool_sort get
z3type1_to_sort (Z3Type_lit LitType_int) = liftM integer_sort get
z3type1_to_sort (Z3Type_lit (LitType_bits :: LitType a)) =
  do z3info <- get
     let num_bits = finiteBitSize (zeroBits :: a)
     let (sorts, set_sorts) =
           if isSigned (zeroBits :: a) then
             (signed_bv_sorts z3info, \x -> z3info { signed_bv_sorts = x })
           else
             (unsigned_bv_sorts z3info, \x -> z3info { unsigned_bv_sorts = x })
     case lookup num_bits sorts of
       Just sort -> return sort
       Nothing -> do sort <- inBase $ Z3.mkBvSort num_bits
                     set $ set_sorts $ (num_bits,sort):sorts
                     return sort
z3type1_to_sort Z3Type_prop = liftM bool_sort get
z3type1_to_sort Z3Type_ptr = liftM integer_sort get

-- * Predefined 'Z3.Sort's for useful types
z3_sort_ptr = z3type1_to_sort Z3Type_ptr
z3_sort_int = z3type1_to_sort (Z3Type_lit LitType_int)

-- | Get the input and output 'Z3.Sort's for a 'Z3Type'
z3type_to_fun_sorts :: Z3Type a -> Z3m ([Z3.Sort],Z3.Sort)
z3type_to_fun_sorts (Z3Type_base z3tp1) =
  liftM (\sort -> ([], sort)) $ z3type1_to_sort z3tp1
z3type_to_fun_sorts (Z3Type_fun in_z3tp1 out_z3tp) =
  liftM2 (\sort (sorts,out_sort) -> (sort:sorts, out_sort))
  (z3type1_to_sort in_z3tp1) (z3type_to_fun_sorts out_z3tp)


-- FIXME HERE: translate from the Z3 logic into Z3 itself


----------------------------------------------------------------------
-- Context manipulation in the Z3m monad
----------------------------------------------------------------------

{-

-- | Make a fresh Z3 symbol
mkFreshSym :: String -> Z3m tag ctx Z3.Symbol
mkFreshSym str =
  do z3info <- get
     sym <- inBase $ Z3.mkStringSymbol $ str ++ show (fresh_id z3info)
     set $ z3info { fresh_id = fresh_id z3info + 1 }
     return sym

-- | Make a fresh Z3 constant
mkFreshConst :: String -> Z3Type1 a -> Z3m tag ctx Z3.AST
mkFreshConst base_name z3tp1 =
  do sort <- z3type1_to_sort z3tp1
     sym <- mkFreshSym base_name
     inBase $ Z3.mkConst sym sort

-- | Make a fresh Z3 constant
mkFreshFDecl :: String -> Z3Type a -> Z3m tag ctx Z3.FuncDecl
mkFreshFDecl base_name z3tp =
  do (in_sorts, out_sort) <- z3type_to_fun_sorts z3tp
     sym <- mkFreshSym base_name
     inBase $ Z3.mkFuncDecl sym in_sorts out_sort

-- | Run a 'Z3m' computation in a context extended with a universal variable
withUVar :: Z3Type1 a -> Z3m tag (ctx :> a) b -> Z3m tag ctx b
withUVar z3tp1 m =
  do ctx <- ask
     localCtx (Z3Ctx_UVar ctx z3tp1) m

-- | Run a 'Z3m' computation in a context extended with an existential variable
withEVar :: Z3Type1 a -> Z3m tag (ctx :> a) b -> Z3m tag ctx b
withEVar z3tp1 m =
  do ctx <- ask
     ast <- mkFreshConst (z3type1_base_name z3tp1) z3tp1
     localCtx (Z3Ctx_EVar ctx z3tp1 ast) m

-- | Lower a 'Z3m' computation into an extended context
lowerZ3m :: Z3m tag ctx b -> Z3m tag (ctx :> a) b
lowerZ3m m =
  do ctx <- ask
     localCtx (z3ctx_lift ctx) m

-}

----------------------------------------------------------------------
-- Expression-building operations
----------------------------------------------------------------------

{-

-- | Extract a function from a functional 'Z3Expr'
z3appM :: Z3Expr tag ctx (a -> b) -> Z3Expr tag ctx a ->
          Z3m tag ctx (Z3Expr tag ctx b)
z3appM (Z3Expr_fun f) = f

-- | Apply a 'Z3ExprM'
(@@) :: Z3ExprM tag ctx (a -> b) -> Z3ExprM tag ctx a ->
        Z3ExprM tag ctx b
(Z3ExprM fm) @@ (Z3ExprM argm) =
  Z3ExprM $ do f <- fm
               arg <- argm
               z3appM f arg

-- | Build a Z3 expression for a free variable inside the 'Z3m' monad
z3var :: Member ctx a -> Z3ExprM tag ctx a
z3var memb =
  Z3ExprM $ do ctx <- ask
               case z3ctx_lookup ctx memb of
                 (z3tp1, Left ast) -> return (Z3Expr_ast z3tp1 ast)
                 (z3tp1, Right i) ->
                   do z3sort <- z3type1_to_sort z3tp1
                      ast <- inBase $ Z3.mkBound (fromInteger i) z3sort
                      return (Z3Expr_ast z3tp1 ast)

-- | Convert a 'Z3Expr' to a 'Z3.AST'
z3expr2ast :: Z3Type1 a -> Z3Expr tag ctx a -> Z3m tag ctx Z3.AST
z3expr2ast _ (Z3Expr_lit LitType_unit ()) =
  do z3info <- get
     inBase $ Z3.mkApp (unit_ctor z3info) []
z3expr2ast _ (Z3Expr_lit LitType_bool bool) =
  inBase $ Z3.mkBool bool
z3expr2ast _ (Z3Expr_lit LitType_int i) =
  do z3info <- get
     inBase $ Z3.mkNumeral (show i) (integer_sort z3info)
z3expr2ast _ (Z3Expr_lit lit_tp@LitType_bits i) =
  do sort <- z3type1_to_sort (Z3Type_lit lit_tp)
     inBase $ Z3.mkNumeral (show $ toInteger i) sort
z3expr2ast _ (Z3Expr_prop bool) = inBase $ Z3.mkBool bool
z3expr2ast _ (Z3Expr_ast _ ast) = return ast
z3expr2ast z3tp1 (Z3Expr_fun _) = no_functional_z3type1 z3tp1
z3expr2ast z3tp1 (Z3Expr_pm _ _) = no_pm_z3type1 z3tp1

-- | Lower a Z3 expression of base type into an extended context
lowerZ3Expr :: Z3Type1 b -> Z3Expr tag ctx b -> Z3Expr tag (ctx :> a) b
lowerZ3Expr _ (Z3Expr_lit lt_tp x) = Z3Expr_lit lt_tp x
lowerZ3Expr _ (Z3Expr_prop b) = Z3Expr_prop b
lowerZ3Expr _ (Z3Expr_ast z3tp1 ast) = Z3Expr_ast z3tp1 ast
lowerZ3Expr z3tp1 (Z3Expr_fun _) = no_functional_z3type1 z3tp1
lowerZ3Expr z3tp1 (Z3Expr_pm _ _) = no_pm_z3type1 z3tp1

-- | Lower a 'Z3ExprM' of base type into an extended context
z3lower :: Z3Type1 b -> Z3ExprM tag ctx b -> Z3ExprM tag (ctx :> a) b
z3lower z3tp1 (Z3ExprM m) =
  Z3ExprM $ liftM (lowerZ3Expr z3tp1) $ lowerZ3m m

-- | Build a Z3 functional expression from a 'Z3.FuncDecl'
z3expr_fdecl :: Z3Type a -> Z3.FuncDecl -> Z3ExprM tag ctx a
z3expr_fdecl _ _ = error "FIXME: write z3expr_fdecl!"

-- | Build a Z3 unary functional expression
z3fun1 :: Z3Type1 a -> Z3Type1 b -> (Z3.AST -> Z3.Z3 Z3.AST) ->
          Z3ExprM tag ctx (a -> b)
z3fun1 z3tp1_a z3tp1_b f =
  Z3ExprM $ return $ Z3Expr_fun $ \expr ->
  do ast_arg <- z3expr2ast z3tp1_a expr
     ast_ret <- inBase $ f ast_arg
     return $ Z3Expr_ast z3tp1_b ast_ret
-- FIXME: enhance functions to work on statically-known values

-- | Build a Z3 binary functional expression
z3fun2 :: Z3Type1 a -> Z3Type1 b -> Z3Type1 c ->
          (Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST) ->
          Z3ExprM tag ctx (a -> b -> c)
z3fun2 z3tp1_a z3tp1_b z3tp1_c f =
  Z3ExprM $ return $ Z3Expr_fun $ \expr1 ->
  return $ Z3Expr_fun $ \expr2 ->
  do ast_arg1 <- z3expr2ast z3tp1_a expr1
     ast_arg2 <- z3expr2ast z3tp1_b expr2
     ast_ret <- inBase $ f ast_arg1 ast_arg2
     return $ Z3Expr_ast z3tp1_c ast_ret
-- FIXME: enhance functions to work on statically-known values


-- * Propositional connectives in 'Z3ExprM'

z3true = Z3ExprM $ return $ Z3Expr_prop True
z3false = Z3ExprM $ return $ Z3Expr_prop False
z3and = z3fun2 Z3Type_prop Z3Type_prop Z3Type_prop $ \x y -> Z3.mkAnd [x,y]
z3or = z3fun2 Z3Type_prop Z3Type_prop Z3Type_prop $ \x y -> Z3.mkOr [x,y]

z3and_exprs :: [Z3Expr tag ctx Prop] -> Z3ExprM tag ctx Prop
z3and_exprs props =
  Z3ExprM $ do asts <- mapM (z3expr2ast Z3Type_prop) props
               ast_ret <- inBase $ Z3.mkAnd asts
               return $ Z3Expr_ast Z3Type_prop ast_ret

z3forall :: Z3Type1 a ->
            (Z3ExprM tag (ctx :> a) a -> Z3ExprM tag (ctx :> a) Prop) ->
            Z3ExprM tag ctx Prop
z3forall z3tp1 f =
  Z3ExprM $
  do body_ast <-
       withUVar z3tp1 $
       ((runZ3ExprM $ f $ z3var Member_Base) >>=
        z3expr2ast Z3Type_prop)
     patt <- inBase $ Z3.mkPattern []
     sym <- mkFreshSym (z3type1_base_name z3tp1)
     sort <- z3type1_to_sort z3tp1
     ast <- inBase $ Z3.mkForall [patt] [sym] [sort] body_ast
     return $ Z3Expr_ast Z3Type_prop ast

z3equals :: Z3Type a -> Z3ExprM tag ctx (a -> a -> Prop)
z3equals = error "FIXME: write z3equals!"

----------------------------------------------------------------------
-- Translating Ops
----------------------------------------------------------------------


-- | Helper type that bundles 'Z3m' together with 'Z3Expr'
newtype Z3InterpRes tag ctx a =
  Z3InterpRes { runZ3InterpRes :: Z3m tag ctx (Z3Expr tag ctx a) }

-- | FIXME: documentation
mkZ3InterpRes1 :: Z3Type1 a -> Z3Type1 b -> (Z3.AST -> Z3.Z3 Z3.AST) ->
                  InterpRes (Z3InterpRes tag) ctx (a -> b)
mkZ3InterpRes1 z3tp_a z3tp_b f =
  buildFOInterpResH (L1FunType_cons (z3type1_to_l1type z3tp1_a)
                     (z3type1_to_l1type z3tp1_b)) $
  \_ (Z3InterpRes m1) ->
  Z3InterpRes $
  do 

mkZ3InterpRes2 :: Z3Type1 a -> Z3Type1 b -> Z3Type1 c ->
                  (Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST) ->
                  InterpRes (Z3InterpRes tag) ctx (a -> b -> c)
mkZ3InterpRes2 z3tp1_a z3tp1_b z3tp1_c f =
  buildFOInterpResH

-- Instance of LCtxExprAlgebra, to translate Ops to Z3
instance LExprTag tag => LCtxExprAlgebra tag (Z3InterpRes tag) where

  -- Literals
  interpOpC _ (Op_Literal ltp x) =
    buildFOInterpResH (L1FunType_base $ L1Type_lit ltp) $ \_ ->
    Z3InterpRes $ z3literal ltp x

  -- Arithmetic operations
  interpOpC _ (Op_arith1 ltp Op1_Abs) =
    error "FIXME: absolute value function not (yet) supported"
  interpOpC _ (Op_arith1 ltp Op1_Signum) =
    error "FIXME: signum function not (yet) supported"
  interpOpC _ (Op_arith1 ltp Op1_Neg) =
    error "FIXME: negation function not (yet) supported"
  interpOpC _ (Op_arith1 ltp Op1_Complement) =
    error "FIXME: complementation function not (yet) supported"
  interpOpC _ (Op_arith2 ltp aop) = error "interpOpC"
  interpOpC _ (Op_coerce ltp_from ltp_to) = error "interpOpC"
  interpOpC _ (Op_cmp ltp acmp) = error "interpOpC"

  -- Propositional operations
  interpOpC _ Op_and = error "interpOpC"
  interpOpC _ Op_or = error "interpOpC"
  interpOpC _ Op_not = error "interpOpC"
  interpOpC _ Op_istrue = error "interpOpC"
  interpOpC _ (Op_forall l1tp) = error "interpOpC"
  interpOpC _ (Op_exists l1tp) = error "interpOpC"
  interpOpC _ (Op_let l1tp) = error "interpOpC"

  -- Predicate monad operations
  interpOpC _ (Op_returnP l1tp) = error "interpOpC"
  interpOpC _ (Op_bindP l1tp_a l1tp_b) = error "interpOpC"
  interpOpC _ Op_errorP = error "interpOpC"
  interpOpC _ (Op_readP read_op) = error "interpOpC"
  interpOpC _ (Op_updateP update_op) = error "interpOpC"
  interpOpC _ Op_assumeP = error "interpOpC"
  interpOpC _ Op_orP = error "interpOpC"
-}
