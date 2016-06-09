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
-- Z3 variable contexts
----------------------------------------------------------------------

-- FIXME: Z3Ctx's should enforce an exists-forall form, i.e., the inner-most
-- variables should be universals and the outermost variables should be
-- existentials

-- | The type of Z3 contexts, that say how to translate free variables into
-- Z3. Specifically, a Z3 context of type @'Z3Ctx' ctx@ indicates, for each
-- variable type listed in @ctx@, whether it is an existential or universal
-- variables. Existential variables are associated with Z3 constant symbols,
-- while universal variables are referred to be deBruijn index, i.e., by their
-- positions in the Z3 context.
data Z3Ctx ctx where
  Z3Ctx_EVar :: Z3Ctx ctx -> L1Type a -> Z3.AST -> Z3Ctx (ctx :> a)
  -- ^ Add an existential variable to the context, which is associated with a Z3
  -- constant symbol, given as a 'Z3.AST'
  Z3Ctx_FVar :: Z3Ctx ctx -> L1FunType (a -> b) -> Z3.FuncDecl ->
                Z3Ctx (ctx :> (a -> b))
  -- ^ Add an existential function variable to the context, which is associated
  -- with a Z3 function symbol, i.e., with a 'Z3.FuncDecl'
  Z3Ctx_UVar :: Z3Ctx ctx -> L1Type a -> Z3Ctx (ctx :> a)
  -- ^ Add a universal variable to the context
  Z3Ctx_nil :: Z3Ctx RNil
  -- ^ The empty Z3 context

-- | The result type of 'z3ctx_lookup'
data Z3CtxLookupRes a
  = Z3CtxLookupRes_EVar (L1Type a) Z3.AST
  | Z3CtxLookupRes_FVar (L1FunType a) Z3.FuncDecl
  | Z3CtxLookupRes_UVar (L1Type a) Integer

-- | Look up a variable in a 'Z3Ctx'
z3ctx_lookup :: Z3Ctx ctx -> Member ctx a -> Z3CtxLookupRes a
z3ctx_lookup ctx memb = z3ctx_lookupH 0 ctx memb
  where
    z3ctx_lookupH :: Integer -> Z3Ctx ctx -> Member ctx a -> Z3CtxLookupRes a
    z3ctx_lookupH _ (Z3Ctx_EVar _ l1tp ast) Member_Base =
      Z3CtxLookupRes_EVar l1tp ast
    z3ctx_lookupH _ (Z3Ctx_FVar _ l1tp fdecl) Member_Base =
      Z3CtxLookupRes_FVar l1tp fdecl
    z3ctx_lookupH i (Z3Ctx_UVar _ l1tp) Member_Base =
      Z3CtxLookupRes_UVar l1tp i
    z3ctx_lookupH i (Z3Ctx_EVar ctx _ _) (Member_Step memb) =
      z3ctx_lookupH i ctx memb
    z3ctx_lookupH i (Z3Ctx_FVar ctx _ _) (Member_Step memb) =
      z3ctx_lookupH i ctx memb
    z3ctx_lookupH i (Z3Ctx_UVar ctx _) (Member_Step memb) =
      z3ctx_lookupH (i+1) ctx memb


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

-- | Build a 'Z3Info'
mkZ3Info :: Z3.Z3 Z3Info
mkZ3Info =
  do unit_sym <- Z3.mkStringSymbol "unit"
     (unit_sort, unit_ctor, _) <- Z3.mkTupleSort unit_sym []
     bool_sort <- Z3.mkBoolSort
     integer_sort <- Z3.mkIntSort
     return Z3Info { fresh_id = 1, unit_sort, unit_ctor,
                     bool_sort, integer_sort,
                     unsigned_bv_sorts = [], signed_bv_sorts = [] }

-- | Monad built on top of 'Z3' for building Z3 expressions relative to a given
-- variable context. The monad uses the state information in a 'Z3Info', reads a
-- 'Z3Ctx' for the current variable context, and also outputs a set of
-- assertions, given as type 'Z3.AST', to pass to the current solver.
newtype Z3m ctx a =
  Z3m { runZ3m :: ReaderT (Z3Ctx ctx)
                  (StateT Z3Info (WriterT [Z3.AST] Z3.Z3)) a }
  deriving (Functor,Applicative,Monad)

instance ReaderM (Z3m ctx) (Z3Ctx ctx) where
  ask = Z3m $ ask

instance RunReaderM (Z3m ctx) (Z3Ctx ctx) where
  local ctx (Z3m m) = Z3m $ local ctx m

localCtx :: Z3Ctx ctx -> Z3m ctx a -> Z3m ctx' a
localCtx ctx (Z3m m) =
  Z3m $ lift $ runReaderT ctx m

instance StateM (Z3m ctx) Z3Info where
  get = Z3m get
  set x = Z3m $ set x

instance WriterM (Z3m ctx) [Z3.AST] where
  put props = Z3m $ put props

instance RunWriterM (Z3m ctx) [Z3.AST] where
  collect (Z3m m) = Z3m $ collect m

instance BaseM (Z3m ctx) Z3.Z3 where
  inBase m = Z3m $ lift $ lift $ lift m

instance RunM Z3.Z3 a (Z3.Z3 a) where
  runM m = m

instance RunM (Z3m ctx) a (Z3Ctx ctx -> Z3Info ->
                           Z3.Z3 ((a, Z3Info), [Z3.AST])) where
  runM m = runM $ runZ3m m

{-
instance RunM Z3.Z3 a (Maybe Z3.Logic -> Z3Opts.Opts -> IO a) where
  runM m = \logic opts -> Z3.evalZ3With logic opts m

instance RunM (Z3m ctx) a (Z3Ctx ctx -> Z3Info ->
                           Maybe Z3.Logic -> Z3Opts.Opts ->
                           IO ((a, Z3Info), [Z3.AST])) where
  runM m = runM $ runZ3m m
-}

----------------------------------------------------------------------
-- Converting logical types to Z3 sorts
----------------------------------------------------------------------

-- | Get the 'Z3.Sort' for a 'L1Type'
l1type_to_sort :: L1Type a -> Z3m ctx Z3.Sort
l1type_to_sort (L1Type_lit LitType_unit) = liftM unit_sort get
l1type_to_sort (L1Type_lit LitType_bool) = liftM bool_sort get
l1type_to_sort (L1Type_lit LitType_int) = liftM integer_sort get
l1type_to_sort (L1Type_lit (LitType_bits :: LitType a)) =
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
l1type_to_sort L1Type_prop = liftM bool_sort get
l1type_to_sort L1Type_ptr = liftM integer_sort get

-- | Get the input and output 'Z3.Sort's for a first-order function type
l1funType_to_sorts :: L1FunType a -> Z3m ctx ([Z3.Sort],Z3.Sort)
l1funType_to_sorts (L1FunType_base l1tp) =
  liftM (\sort -> ([], sort)) $ l1type_to_sort l1tp
l1funType_to_sorts (L1FunType_cons l1tp ftp) =
  liftM2 (\sort (sorts,out_sort) -> (sort:sorts, out_sort))
  (l1type_to_sort l1tp) (l1funType_to_sorts ftp)

-- | Return a base name for each type, to be used for variables
l1type_base_name :: L1Type a -> String
l1type_base_name (L1Type_lit LitType_unit) = "u"
l1type_base_name (L1Type_lit LitType_bool) = "b"
l1type_base_name (L1Type_lit LitType_int) = "i"
l1type_base_name (L1Type_lit LitType_bits) = "bv"
l1type_base_name L1Type_prop = "phi"
l1type_base_name L1Type_ptr = "ptr"


----------------------------------------------------------------------
-- Monadic operations on Z3 contexts
----------------------------------------------------------------------

-- | Build a fresh 'Z3.Symbol' with base name @str@
mkFreshSymbol :: String -> Z3m ctx Z3.Symbol
mkFreshSymbol basename =
  do z3info <- get
     let i = fresh_id z3info
     set $ z3info { fresh_id = i+1 }
     inBase $ Z3.mkStringSymbol (basename ++ show i)

-- | Make a fresh constant as a 'Z3.AST' from a first-order type
mkFreshConstant :: L1Type a -> Z3m ctx Z3.AST
mkFreshConstant l1tp =
  do sym <- mkFreshSymbol (l1type_base_name l1tp)
     sort <- l1type_to_sort l1tp
     inBase $ Z3.mkConst sym sort

-- | Make a fresh function declaration as a 'Z3.FuncDecl' from a first-order
-- function type
mkFreshFuncDecl :: L1FunType a -> Z3m ctx Z3.FuncDecl
mkFreshFuncDecl ftp =
  do sym <- mkFreshSymbol ("f_" ++ l1type_base_name (funType_ret_type ftp))
     (arg_sorts, ret_sort) <- l1funType_to_sorts ftp
     inBase $ Z3.mkFuncDecl sym arg_sorts ret_sort

-- | Run a 'Z3m' computation in the context extended with a universal variable
withUVar :: L1Type a -> Z3m (ctx :> a) b -> Z3m ctx b
withUVar l1tp m =
  do ctx <- ask
     localCtx (Z3Ctx_UVar ctx l1tp) m

-- | Run a 'Z3m' computation in the context extended with an existential
-- variable, modeled with a fresh Z3 constant. NOTE: this is not allowed inside
-- the context of a universal, as this would require a fresh Z3 constant for
-- each of the (potentially infinitely many) possible instantiations for this
-- universal variable. Return both the computation result and the fresh Z3
-- constant.
withEVar :: L1Type a -> Z3m (ctx :> a) b -> Z3m ctx (Z3.AST, b)
withEVar l1tp m =
  do ctx <- ask
     errorIfUniversal ctx
     const_ast <- mkFreshConstant l1tp
     ret <- localCtx (Z3Ctx_EVar ctx l1tp const_ast) m
     return (const_ast, ret)
       where
         errorIfUniversal :: Z3Ctx ctx' -> Z3m ctx ()
         errorIfUniversal (Z3Ctx_UVar _ _) =
           error "withEVar: attempt to build a Z3 exists formula inside a forall!"
         errorIfUniversal (Z3Ctx_EVar ctx _ _) =
           errorIfUniversal ctx
         errorIfUniversal (Z3Ctx_FVar ctx _ _) =
           errorIfUniversal ctx
         errorIfUniversal Z3Ctx_nil = return ()

-- | Build an existential 'Z3Ctx' from a list of first-order types
mkZ3Ctx :: MapRList L1FunType ctx -> Z3m c (Z3Ctx ctx)
mkZ3Ctx (tps :>: L1FunType_base l1tp) =
  liftM3 Z3Ctx_EVar (mkZ3Ctx tps) (return l1tp) (mkFreshConstant l1tp)
mkZ3Ctx (tps :>: ftp@(L1FunType_cons _ _)) =
  liftM3 Z3Ctx_FVar (mkZ3Ctx tps) (return ftp) (mkFreshFuncDecl ftp)
mkZ3Ctx MNil = return Z3Ctx_nil


----------------------------------------------------------------------
-- Converting from our reachability logic into Z3
----------------------------------------------------------------------

-- | Helper type for a 'Z3.AST' inside a 'Z3m' computation. Note that the type
-- argument is ignored.
newtype Z3m_AST ctx a = Z3m_AST { unZ3m_AST :: Z3m ctx Z3.AST }

-- | Helper function for extracting a 'Z3.AST' from a typed argument passed to
-- 'interpOpB' or 'interpVarB' in a 'BindingApply'
extractL1ArgAST :: L1Type a -> BindingApply Z3m_AST ctx a -> Z3m ctx Z3.AST
extractL1ArgAST l1tp arg =
  unZ3m_AST $ elimL1BindingApplyF l1tp $ unBindingApply arg

-- | Helper function for extracting a list of 'Z3.AST's from a list of typed
-- arguments passed to 'interpOpB' or 'interpVarB'
extractArgASTs :: LTypeArgs a args ret ->
                  MapList (BindingApply Z3m_AST ctx) args ->
                  Z3m ctx [Z3.AST]
extractArgASTs (LTypeArgs_base _) Nil = return []
extractArgASTs (LTypeArgs_pm _) Nil = return []
extractArgASTs (LTypeArgs_fun (LType_base l1tp) tp_args) (Cons arg args) =
  do ast <- extractL1ArgAST l1tp arg
     asts <- extractArgASTs tp_args args
     return (ast : asts)
extractArgASTs (LTypeArgs_fun _ tp_args) (Cons arg args) =
  error "extractArgASTs: converting a higher-order variable to Z3!"

-- | Helper function for extracting a 'Z3.AST' from an argument of type
-- @'Literal' a@ passed to 'interpOpB' or 'interpVarB'
extractLitArgAST :: BindingApply Z3m_AST ctx (Literal a) -> Z3m ctx Z3.AST
extractLitArgAST arg = unZ3m_AST $ unBindingApply arg

-- | Helper function for extracting a 'Z3.AST' from an argument of type 'Ptr'
-- passed to 'interpOpB' or 'interpVarB'
extractPtrArgAST :: BindingApply Z3m_AST ctx Ptr -> Z3m ctx Z3.AST
extractPtrArgAST arg = unZ3m_AST $ unBindingApply arg

-- | Helper function for extracting a 'Z3.AST' from an argument of type 'Prop'
-- passed to 'interpOpB' or 'interpVarB'
extractPropArgAST :: BindingApply Z3m_AST ctx Prop -> Z3m ctx Z3.AST
extractPropArgAST arg = unZ3m_AST $ unBindingApply arg

-- | Apply an 'ArithOp1' to a 'Z3.AST'
z3ArithOp1 :: ArithOp1 -> Z3.AST -> Z3.Z3 Z3.AST
z3ArithOp1 Op1_Abs = error "Z3 absolute value function not (yet?) supported"
z3ArithOp1 Op1_Signum = error "Z3 signum function not (yet?) supported"
z3ArithOp1 Op1_Neg = Z3.mkUnaryMinus
z3ArithOp1 Op1_Complement = Z3.mkBvnot

-- | Apply an 'ArithOp2' on a given 'LitType' to a 'Z3.AST'
z3ArithOp2 :: LitType a -> ArithOp2 -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST
z3ArithOp2 _ Op2_Add = \x y -> Z3.mkAdd [x,y]
z3ArithOp2 _ Op2_Sub = \x y -> Z3.mkSub [x,y]
z3ArithOp2 _ Op2_Mult = \x y -> Z3.mkMul [x,y]
z3ArithOp2 (LitType_bits :: LitType a) Op2_Div
  | isSigned (0 :: a) = Z3.mkBvudiv
z3ArithOp2 (LitType_bits :: LitType a) Op2_Div = Z3.mkBvsdiv
z3ArithOp2 _ Op2_Div = Z3.mkDiv
z3ArithOp2 (LitType_bits :: LitType a) Op2_Mod
  | isSigned (0 :: a) =
      error "FIXME: how to translate signed mod into Z3?"
z3ArithOp2 (LitType_bits :: LitType a) Op2_Mod = Z3.mkBvsmod
z3ArithOp2 _ Op2_Mod = Z3.mkMod
z3ArithOp2 (LitType_bits :: LitType a) Op2_Rem
  | isSigned (0 :: a) = Z3.mkBvurem
z3ArithOp2 (LitType_bits :: LitType a) Op2_Rem = Z3.mkBvsrem
z3ArithOp2 _ Op2_Rem = Z3.mkRem
z3ArithOp2 _ Op2_BitAnd = Z3.mkBvand
z3ArithOp2 _ Op2_BitOr = Z3.mkBvor
z3ArithOp2 _ Op2_BitXor = Z3.mkBvxor

-- | Apply a coercion function from one 'LitType' to another to a 'Z3.AST'
z3Coerce :: LitType f -> LitType t -> Z3.AST -> Z3.Z3 Z3.AST
z3Coerce (LitType_bits :: LitType f) (LitType_bits :: LitType t)
  | finiteBitSize (0 :: t) == finiteBitSize (0 :: f) =
    return
z3Coerce (LitType_bits :: LitType f) (LitType_bits :: LitType t)
  | isSigned (0 :: f) && finiteBitSize (0 :: t) > finiteBitSize (0 :: f) =
    Z3.mkSignExt $ finiteBitSize (0 :: t) - finiteBitSize (0 :: f)
z3Coerce (LitType_bits :: LitType f) (LitType_bits :: LitType t)
  | finiteBitSize (0 :: t) > finiteBitSize (0 :: f) =
    Z3.mkZeroExt $ finiteBitSize (0 :: t) - finiteBitSize (0 :: f)
z3Coerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) =
  Z3.mkExtract (finiteBitSize (0 :: t) - 1) 0
z3Coerce LitType_int (LitType_bits :: LitType t) =
  Z3.mkInt2bv (finiteBitSize (0 :: t))
z3Coerce (LitType_bits :: LitType f) LitType_int =
  \ast -> Z3.mkBv2int ast (isSigned (0 :: f))
z3Coerce LitType_int LitType_int = return
z3Coerce LitType_bool LitType_bool = return
z3Coerce LitType_unit LitType_unit = return
z3Coerce _ _ = error "Z3 Coercion not supported!"

-- | Apply an 'ArithCmp' to two 'Z3.AST's
z3ArithCmp :: ArithCmp -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST
z3ArithCmp OpCmp_EQ = Z3.mkEq
z3ArithCmp OpCmp_LT = Z3.mkLt
z3ArithCmp OpCmp_LE = Z3.mkLe

-- Instance for converting LExprs to Z3 ASTs
instance LBindingExprAlgebra tag Z3m_AST where

  -- Interpret a variable, given as a Member in the current context, into Z3
  interpVarB _ tp_args memb args =
    Z3m_AST $
    do z3ctx <- ask
       arg_asts <- extractArgASTs tp_args args
       case z3ctx_lookup z3ctx memb of
         Z3CtxLookupRes_EVar _ ast ->
           -- NOTE: in this case, args must be empty, because the type of the
           -- variable must be an L1Type, but we need not "prove" that here, and
           -- we simply ignore args
           return ast
         Z3CtxLookupRes_FVar _ fdecl ->
           inBase $ Z3.mkApp fdecl arg_asts
         Z3CtxLookupRes_UVar l1tp i ->
           -- NOTE: as with the EVar case, arg_asts must be empty here
           do sort <- l1type_to_sort l1tp
              inBase $ Z3.mkBound (fromInteger i) sort

  -- Interpret literals into Z3
  interpOpB (Op_Literal LitType_unit ()) _ =
    Z3m_AST $ do f <- liftM unit_ctor get
                 inBase $ Z3.mkApp f []
  interpOpB (Op_Literal LitType_bool b) _ =
    Z3m_AST $ inBase $ Z3.mkBool b
  interpOpB (Op_Literal LitType_int i) _ =
    Z3m_AST $ inBase $ Z3.mkInteger i
  interpOpB (Op_Literal lit_tp@LitType_bits bv) _ =
    Z3m_AST $ do sort <- l1type_to_sort (L1Type_lit lit_tp)
                 inBase $ Z3.mkIntegral bv sort

  -- Interpret arithmetic operations on Booleans
  interpOpB (Op_arith1 LitType_bool Op1_Neg) (Cons arg1 _) =
    -- Negation on Booleans --> not
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 inBase $ Z3.mkNot ast1
  interpOpB (Op_arith1 LitType_bool aop) (Cons arg1 _) =
    error "In converting to Z3: Unsupported unary operation on Booleans"
  interpOpB (Op_arith2 LitType_bool Op2_Add) (Cons arg1 (Cons arg2 _)) =
    -- Addition on Booleans --> or
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractLitArgAST arg2
                 inBase $ Z3.mkOr [ast1, ast2]
  interpOpB (Op_arith2 LitType_bool Op2_Mult) (Cons arg1 (Cons arg2 _)) =
    -- Multiplication on Booleans --> and
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractLitArgAST arg2
                 inBase $ Z3.mkAnd [ast1, ast2]
  interpOpB (Op_arith2 LitType_bool aop) _ =
    error "In converting to Z3: Unsupported binary operation on Booleans"

  -- Interpret the arithmetic operations into Z3
  interpOpB (Op_arith1 _ aop) (Cons arg1 _) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 inBase $ z3ArithOp1 aop ast1
  interpOpB (Op_arith2 lit_tp aop) (Cons arg1 (Cons arg2 _)) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractLitArgAST arg2
                 inBase $ z3ArithOp2 lit_tp aop ast1 ast2
  interpOpB (Op_coerce lit_tp_from lit_tp_to) (Cons arg1 _) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 inBase $ z3Coerce lit_tp_from lit_tp_from ast1
  interpOpB (Op_cmp ltp acmp) (Cons arg1 (Cons arg2 _)) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractLitArgAST arg2
                 inBase $ z3ArithCmp acmp ast1 ast2
  interpOpB (Op_cond l1tp) (Cons arg1 (Cons arg2 (Cons arg3 _))) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractL1ArgAST l1tp arg2
                 ast3 <- extractL1ArgAST l1tp arg3
                 inBase $ Z3.mkIte ast1 ast2 ast3
  interpOpB Op_null_ptr _ = Z3m_AST $ inBase $ Z3.mkInteger 0
  interpOpB (Op_global_var i) _ =
    if i < 0 then
      Z3m_AST $ inBase $ Z3.mkInteger i
    else error "In converting to Z3: non-negative global variable constant!"
  interpOpB Op_next_ptr (Cons arg1 _) =
    Z3m_AST $ do ast1 <- extractPtrArgAST arg1
                 ast_one <- inBase $ Z3.mkInteger 1
                 inBase $ Z3.mkAdd [ast1, ast_one]
  interpOpB Op_ptr_eq (Cons arg1 (Cons arg2 _)) =
    Z3m_AST $ do ast1 <- extractPtrArgAST arg1
                 ast2 <- extractPtrArgAST arg2
                 inBase $ Z3.mkEq ast1 ast2

  -- Interpret the first-order propositional operations into Z3
  interpOpB Op_and (Cons arg1 (Cons arg2 _)) =
    Z3m_AST $ do ast1 <- extractPropArgAST arg1
                 ast2 <- extractPropArgAST arg2
                 inBase $ Z3.mkAnd [ast1, ast2]
  interpOpB Op_or (Cons arg1 (Cons arg2 _)) =
    Z3m_AST $ do ast1 <- extractPropArgAST arg1
                 ast2 <- extractPropArgAST arg2
                 inBase $ Z3.mkOr [ast1, ast2]
  interpOpB Op_not (Cons arg1 _) =
    Z3m_AST $ do ast1 <- extractPropArgAST arg1
                 inBase $ Z3.mkNot ast1
  interpOpB (Op_eq l1tp) (Cons arg1 (Cons arg2 _)) =
    Z3m_AST $ do ast1 <- extractL1ArgAST l1tp arg1
                 ast2 <- extractL1ArgAST l1tp arg2
                 inBase $ Z3.mkEq ast1 ast2
  interpOpB Op_istrue (Cons arg1 _) =
    -- NOTE: Op_istrue is the identity when converted to Z3
    Z3m_AST $ extractLitArgAST arg1

  -- Interpret the quantifiers into Z3
  interpOpB (Op_forall l1tp) (Cons (BindingApply (Z3m_AST body_m)) _) =
    Z3m_AST $ do body_ast <- withUVar l1tp body_m
                 sym <- mkFreshSymbol (l1type_base_name l1tp)
                 sort <- l1type_to_sort l1tp
                 inBase $ Z3.mkForall [] [sym] [sort] body_ast
  interpOpB (Op_exists l1tp) (Cons (BindingApply (Z3m_AST body_m)) _) =
    Z3m_AST $ liftM snd $ withEVar l1tp body_m
  interpOpB (Op_let l1tp) (Cons rhs (Cons (BindingApply (Z3m_AST body_m)) _)) =
    Z3m_AST $ do rhs_ast <- extractL1ArgAST l1tp rhs
                 (const_ast, ret) <- withEVar l1tp body_m
                 equality_ast <- inBase $ Z3.mkEq const_ast rhs_ast
                 put [equality_ast]
                 return ret

  -- Signal an error if we try to convert any predicate monad operations to Z3
  interpOpB (Op_returnP _) _ = error "Cannot convert returnP to Z3"
  interpOpB (Op_bindP l1tp_a l1tp_b) _ =
    error "Cannot convert bindP to Z3!"
  interpOpB (Op_readP _) _ =
    error "Cannot convert readP to Z3!"
  interpOpB (Op_updateP _) _ =
    error "Cannot convert updateP to Z3!"
  interpOpB (Op_raiseP maybe_exn) _ =
    error "Cannot convert raiseP to Z3!"
  interpOpB (Op_catchP exn) _ =
    error "Cannot convert catchP to Z3!"
  interpOpB Op_assumeP _ =
    error "Cannot convert assumeP to Z3!"
  interpOpB (Op_existsP _) _ =
    error "Cannot convert existsP to Z3!"
  interpOpB Op_orP _ =
    error "Cannot convert orP to Z3!"

-- | Top-level call to convert an 'LProp' into a Z3 expression
lprop_to_z3ast :: Closed (LProp tag) -> Z3m RNil Z3.AST
lprop_to_z3ast prop =
  unZ3m_AST $ interpExprB Proxy prop

-- | Top-level call to convert an 'LProp'-in-binding into a Z3 expression
mb_lprop_to_z3ast :: Closed (Mb ctx (LProp tag)) -> Z3m ctx Z3.AST
mb_lprop_to_z3ast prop =
  unZ3m_AST $ interpMbExprB Proxy prop


----------------------------------------------------------------------
-- Top-level interface
----------------------------------------------------------------------

-- | Convert a 'Z3.AST' that is known to be a value into an 'SMTValue'
z3ast_to_value :: L1Type a -> Z3.AST -> Z3.Z3 (SMTValue a)
z3ast_to_value (L1Type_lit lit_tp@LitType_unit) _ =
  return $ Literal ()
z3ast_to_value (L1Type_lit lit_tp@LitType_bool) ast =
  liftM Literal $ Z3.getBool ast
z3ast_to_value (L1Type_lit lit_tp@LitType_int) ast =
  liftM Literal $ Z3.getInt ast
z3ast_to_value (L1Type_lit lit_tp@(LitType_bits :: LitType bv)) ast =
  liftM (Literal . fromInteger) $ Z3.getBv ast (isSigned (zeroBits :: bv))
z3ast_to_value L1Type_ptr ast =
  liftM Ptr $ Z3.getInt ast
z3ast_to_value L1Type_prop ast =
  Z3.getBool ast

-- | Get the value of a 'Z3.AST' from a Z3 model
evalAST :: Z3.Model -> L1Type a -> Z3.AST -> Z3.Z3 (MaybeSMTValue a)
evalAST z3model l1tp ast =
  do maybe_ast <- Z3.modelEval z3model ast False
     case maybe_ast of
       Nothing -> return $ MaybeSMTValue Nothing
       Just ast -> liftM (MaybeSMTValue . Just) $ z3ast_to_value l1tp ast

-- | Helper for 'evalFuncEntry'
evalFunEntryArgsFrom :: Z3.FuncEntry -> Int -> MapList L1Type args ->
                        Z3.Z3 (TupleType (SMTValues args))
evalFunEntryArgsFrom fentry i Nil = return ()
evalFunEntryArgsFrom fentry i (Cons l1tp tps) =
  do arg_ast <- Z3.funcEntryGetArg fentry i
     arg_val <- z3ast_to_value l1tp arg_ast
     rest_vals <- evalFunEntryArgsFrom fentry (i+1) tps
     return (arg_val, rest_vals)

-- | Get the value of a 'Z3.FuncEntry' as a pair of 'SMTValue's for the input
-- and output values given by the 'Z3.FuncEntry'
evalFuncEntry :: L1FunType a -> Z3.FuncEntry ->
                 Z3.Z3 (TupleType (SMTValues (ArgTypes a)),
                        SMTValue (RetType a))
evalFuncEntry ftp fentry =
  do arg_vals <- evalFunEntryArgsFrom fentry 0 (funType_arg_types ftp)
     ret_ast <- Z3.funcEntryGetValue fentry
     ret_val <- z3ast_to_value (funType_ret_type ftp) ret_ast
     return (arg_vals, ret_val)

-- | Get the value of a 'Z3.FuncInterp' as an 'SMTValue'
evalFuncInterp :: L1FunType a -> Z3.FuncInterp -> Z3.Z3 (SMTValueFun a)
evalFuncInterp ftp finterp =
  do num_entries <- Z3.funcInterpGetNumEntries finterp
     func_entries <- mapM (Z3.funcInterpGetEntry finterp) [0 .. num_entries - 1]
     entry_val_map <- mapM (evalFuncEntry ftp) func_entries
     default_ast <- Z3.funcInterpGetElse finterp
     default_val <- z3ast_to_value (funType_ret_type ftp) default_ast
     return $ FinFun { finfunDef = default_val, finfunMap = entry_val_map }

-- | Get the value of a 'Z3.FuncDecl' from a Z3 model
evalFuncDecl :: Z3.Model -> L1FunType (a -> b) -> Z3.FuncDecl ->
                Z3.Z3 (MaybeSMTValue (a -> b))
evalFuncDecl z3model ftp fdecl =
  do maybe_finterp <- Z3.getFuncInterp z3model fdecl
     case maybe_finterp of
       Nothing -> return $ MaybeSMTValue Nothing
       Just finterp ->
         liftM (MaybeSMTValue . Just) $ evalFuncInterp ftp finterp

-- | Get the values of all the existential values in a 'Z3Ctx' from a Z3 model
evalZ3Ctx :: Z3.Model -> Z3Ctx ctx -> Z3.Z3 (MapRList MaybeSMTValue ctx)
evalZ3Ctx z3model Z3Ctx_nil = return MNil
evalZ3Ctx z3model (Z3Ctx_EVar ctx l1tp ast) =
  liftM2 (:>:) (evalZ3Ctx z3model ctx) (evalAST z3model l1tp ast)
evalZ3Ctx z3model (Z3Ctx_FVar ctx ftp fdecl) =
  liftM2 (:>:) (evalZ3Ctx z3model ctx) (evalFuncDecl z3model ftp fdecl)
evalZ3Ctx z3model (Z3Ctx_UVar _ _) =
  error "evalZ3Ctx: formula has a top-level universal variable!"

-- | The options we will pass to Z3 by default
defaultZ3Options :: Z3Opts.Opts
defaultZ3Options = Z3Opts.stdOpts

-- | The logic we use in Z3, where 'Nothing' is the Z3 default logic
defaultZ3Logic :: Maybe Z3.Logic
defaultZ3Logic = Nothing

-- | Run a 'Z3m' computation, using the default Z3 options and logic, a freshly
-- created 'Z3Info', and an existential 'Z3Ctx'
evalZ3m :: MapRList L1FunType ctx -> Z3m ctx a -> IO a
evalZ3m tps m =
  Z3.evalZ3With defaultZ3Logic defaultZ3Options $
  do z3info <- mkZ3Info
     ((ret, _), _) <-
       runM (mkZ3Ctx tps >>= \ctx -> localCtx ctx m) Z3Ctx_nil z3info
     return ret

-- | Dummy type to indicate the Z3 solver in the 'SMTSolver' class
data Z3Solver = Z3Solver

instance SMTSolver Z3Solver where
  smtSolve _ const_tps props =
    evalZ3m const_tps $
    do (converted_asts, collected_asts) <-
         collect $ mapM mb_lprop_to_z3ast props
       result <-
         inBase $ Z3.solverCheckAssumptions $ collected_asts ++ converted_asts
       case result of
         Z3.Sat ->
           do ctx <- ask
              z3model <- inBase Z3.solverGetModel
              vals <- inBase $ evalZ3Ctx z3model ctx
              return (SMT_sat vals)
         Z3.Unsat -> return SMT_unsat
         Z3.Undef -> inBase $ liftM SMT_unknown Z3.solverGetReasonUnknown
