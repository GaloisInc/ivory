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
  Z3Ctx_FVar :: Z3Ctx ctx -> L1FunType a -> Z3.FuncDecl -> Z3Ctx (ctx :> a)
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
    unit_ctor :: Z3.AST,
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
newtype Z3m ctx a =
  Z3m { runZ3m :: ReaderT (Z3Ctx ctx) (StateT Z3Info Z3.Z3) a }
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

instance BaseM (Z3m ctx) Z3.Z3 where
  inBase m = Z3m $ lift $ lift m

instance RunM Z3.Z3 a (Maybe Z3.Logic -> Z3Opts.Opts -> IO a) where
  runM m = \logic opts -> Z3.evalZ3With logic opts m

instance RunM (Z3m ctx) a (Z3Ctx ctx -> Z3Info ->
                           Maybe Z3.Logic -> Z3Opts.Opts ->
                           IO (a, Z3Info)) where
  runM m = runM $ runZ3m m


----------------------------------------------------------------------
-- Converting from our reachability logic into Z3
----------------------------------------------------------------------

-- | Helper type for a 'Z3.AST' inside a 'Z3m' computation. Note that the type
-- argument is ignored.
newtype Z3m_AST ctx a = Z3m_AST { unZ3m_AST :: Z3m ctx Z3.AST }

-- Instance for converting LExprs to Z3 ASTs
--instance LBindingExprAlgebra tag Z3m_AST where
{-
  interpOpOrVarB :: PMOrBaseType a -> OpOrVar tag ctx (AddArrows args a) ->
                    MapList (BindingApply f ctx) args ->
                    f ctx a
-}

{-

-- | Extract a computation of a 'Z3.AST' from a 'Z3Res' of base type
z3res_to_ast :: L1Type a -> Z3Res a -> Z3m Z3.AST
z3res_to_ast _ (Z3Res_base _ m) = m
z3res_to_ast l1tp (Z3Res_fun _ _) = no_functional_l1type l1tp

-- | Convert a 'Z3Expr' to Z3
z3expr_to_z3 :: Z3Ctx ctx -> Mb ctx (Z3Expr a) -> Z3Res a
z3expr_to_z3 ctx [nuP| Z3Expr_var l1tp n |] =
  case mbNameBoundP n of
    Left memb -> z3ctx_lookup ctx memb
    Right n -> error "z3expr_to_z3: unbound name!"
z3expr_to_z3 ctx [nuP| Z3Expr_lit LitType_unit () |] =
  Z3Res_base l1typeRep $
  do z3info <- get
     return $ unit_ctor z3info
z3expr_to_z3 ctx [nuP| Z3Expr_lit LitType_bool b |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkBool $ mbLift b
z3expr_to_z3 ctx [nuP| Z3Expr_lit LitType_int i |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkInteger $ mbLift i
z3expr_to_z3 ctx [nuP| Z3Expr_lit lit_tp@LitType_bits x |] =
  Z3Res_base l1typeRep $
  do sort <- l1type_to_sort (L1Type_lit lit_tp)
     inBase $ Z3.mkIntegral sort $ mbLift x
z3expr_to_z3 ctx [nuP| Z3Expr_prop b |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkBool $ mbLift b
z3expr_to_z3 ctx [nuP| Z3Expr_ast l1tp ast |] =
  Z3Res_base l1tp $ return ast
z3expr_to_z3 ctx [nuP| Z3Expr_multi _ l1tp_out f args |] =
  Z3Res_base l1tp_out $ inBase $ f args
z3expr_to_z3 ctx [nuP| Z3Expr_op op |] = z3op_to_z3 ctx op
z3expr_to_z3 ctx [nuP| Z3Expr_forall l1tp body |] =
  error "FIXME HERE NOW"
z3expr_to_z3 ctx [nuP| Z3Expr_exists l1tp body |] =
  error "FIXME HERE NOW"
z3expr_to_z3 ctx [nuP| Z3Expr_and es |] =
  Z3Res_base L1Type_prop $
  do asts <- mapM (z3res_to_ast L1Type_prop . z3expr_to_z3 ctx) es
     inBase $ Z3.mkAnd asts
z3expr_to_z3 ctx [nuP| Z3Expr_or es |] =
  Z3Res_base L1Type_prop $
  do asts <- mapM (\e -> case z3expr_to_z3 ctx e of
                      Z3Res_base _ m -> m) es
     inBase $ Z3.mkOr asts
z3expr_to_z3 ctx [nuP| Z3Expr_true |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkTrue
z3expr_to_z3 ctx [nuP| Z3Expr_false |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkFalse

z3op_to_z3 :: Z3Ctx ctx -> Z3AppliedOp a -> Z3Res a
z3op_to_z3 ctx [nuP| Z3Expr_fdecl z3tp fdecl |] =
  error "FIXME HERE NOW"
z3op_to_z3 ctx [nuP| Z3Expr_op1 l1tp_a l1tp_b f |] =
  Z3Res_fun l1tp_a $ \res1 ->
  Z3Res_base l1tp_b $
  do ast1 <- z3res_to_ast l1tp_a res1
     inBase $ f ast1
z3op_to_z3 ctx [nuP| Z3Expr_op2 l1tp_a l1tp_b l1tp_c f |] =
  Z3Res_fun l1tp_a $ \res1 ->
  Z3Res_fun l1tp_b $ \res2 ->
  Z3Res_base l1tp_c $
  do ast1 <- z3res_to_ast l1tp_a res1
     ast2 <- z3res_to_ast l1tp_b res2
     inBase $ f ast1 ast2
z3op_to_z3 ctx [nuP| Z3Expr_app f arg |] =
  case z3op_to_z3 ctx f of
    Z3Res_fun g -> g $ z3expr_to_z3 arg
-}





-- FIXME HERE NOW: old stuff below!


{-

----------------------------------------------------------------------
-- Z3 types
----------------------------------------------------------------------

-- | Higher-order Z3 types. Note that the use of 'L1Type' in the domains of
-- function types technically means these are second-order types.
data Z3Type a where
  Z3Type_base :: L1Type a -> Z3Type a
  Z3Type_fun :: L1Type a -> Z3Type b -> Z3Type (a -> b)

-- | Typeclass version of 'Z3Type'
class Z3Typeable a where
  z3typeRep :: Z3Type a

instance L1Typeable a => Z3Typeable a where
  z3typeRep = Z3Type_base l1typeRep
instance (L1Typeable a, Z3Typeable b) => Z3Typeable (a -> b) where
  z3typeRep = Z3Type_fun l1typeRep z3typeRep

-- | Return a base name for each type, to be used for variables
l1type_base_name :: L1Type a -> String
l1type_base_name (L1Type_lit LitType_unit) = "u"
l1type_base_name (L1Type_lit LitType_bool) = "b"
l1type_base_name (L1Type_lit LitType_int) = "i"
l1type_base_name (L1Type_lit LitType_bits) = "bv"
l1type_base_name L1Type_prop = "phi"
l1type_base_name L1Type_ptr = "ptr"


FIXME HERE NOW: interpret LExprs directly into (Z3m Z3.AST)


----------------------------------------------------------------------
-- Typed Z3 expressions
----------------------------------------------------------------------

-- | FIXME: documentation
data Z3Op 

-- | FIXME: documentation
data Z3Expr a where
  -- * First-order expressions
  Z3Expr_lit :: Liftable a => LitType a -> a -> Z3Expr (Literal a)
  Z3Expr_prop :: Bool -> Z3Expr Prop

  -- * Function applications

  -- * Functional expressions
  -- | A Z3 operation applied to 0 or more arguments
  Z3Expr_app_expr :: Z3AppliedExpr a -> Z3Expr a

  -- * Propositional connectives
  Z3Expr_forall :: L1Type a -> Binding a (Z3Expr Prop) -> Z3Expr Prop
  Z3Expr_exists :: L1Type a -> Binding a (Z3Expr Prop) -> Z3Expr Prop
  Z3Expr_and :: [Z3Expr Prop] -> Z3Expr Prop
  Z3Expr_or :: [Z3Expr Prop] -> Z3Expr Prop
  Z3Expr_true :: Z3Expr Prop
  Z3Expr_false :: Z3Expr Prop

-- | FIXME: documentation
data Z3AppliedExpr a where
  -- | A variable
  Z3Expr_var :: Name a -> Z3Expr a
  -- | Apply an applied op to an expression
  Z3Expr_app :: Z3AppliedExpr (a -> b) -> Z3Expr a -> Z3AppliedExpr b

-- | Convenient short-hand: a Z3 proposition is a 'Z3Expr' of type 'Prop'
type Z3Prop = Z3Expr Prop

-- Make the NuMatching instances for Z3Expr
$(mkNuMatching [t| forall l a. Z3Expr a |])
$(mkNuMatching [t| forall l a. Z3AppliedExpr a |])

-- | Apply a 'Z3Expr' to another one
(@@) :: Z3Expr (a -> b) -> Z3Expr a -> Z3Expr b
(Z3Expr_var tp1 _) @@ _ = no_functional_l1type tp1
(Z3Expr_ast tp1 _) @@ _ = no_functional_l1type tp1
(Z3Expr_multi _ tp1 _ _) @@ _ = no_functional_l1type tp1
(Z3Expr_op f) @@ arg = Z3Expr_op $ Z3Expr_app f arg

-- | Build a typed 'Z3Expr' from a 'Z3.FuncDecl'
z3fdecl :: Z3Type a -> Z3.FuncDecl -> Z3Expr a
z3fdecl z3tp fdecl = Z3Expr_op $ Z3Expr_fdecl z3tp fdecl

-- Use the Z3 operations to build a Num instance for any Z3Expr type
instance (Num a, LitTypeable a) => Num (Z3Expr (Literal a)) where
  e1 + e2 = Z3Expr_multi l1typeRep l1typeRep Z3.mkAdd [e1,e2]
  e1 - e2 = Z3Expr_multi l1typeRep l1typeRep Z3.mkSub [e1,e2]
  e1 * e2 = Z3Expr_multi l1typeRep l1typeRep Z3.mkMul [e1,e2]
  negate e = Z3Expr_op (Z3Expr_op1 l1typeRep l1typeRep Z3.mkUnaryMinus) @@ e
  abs e = error "Z3 absolute value function not (yet?) supported"
  signum e = error "Z3 signum function not (yet?) supported"
  fromInteger i = Z3Expr_lit litTypeRep $ fromInteger i

-- | Increment a pointer expression (used to bump the free pointer)
z3ptr_incr :: Z3Expr (Ptr -> Ptr)
z3ptr_incr = Z3Expr_op $ Z3Expr_op1 l1typeRep l1typeRep $ \ast ->
  Z3.mkInteger 1 >>= \i -> Z3.mkAdd [ast, i]

-- | Build a universal quantifier
z3forall :: L1Type a -> (Z3Expr a -> Z3Prop) -> Z3Prop
z3forall tp1 body_f = Z3Expr_forall tp1 $ nu $ \n -> body_f $ Z3Expr_var tp1 n

-- | Build an existential quantifier
z3exists :: L1Type a -> (Z3Expr a -> Z3Prop) -> Z3Prop
z3exists tp1 body_f = Z3Expr_exists tp1 $ nu $ \n -> body_f $ Z3Expr_var tp1 n

-- | Build a proposition that two expressions are equal
z3equals :: Z3Type a -> Z3Expr a -> Z3Expr a -> Z3Prop
z3equals (Z3Type_base tp1) e1 e2 =
  (Z3Expr_op $ Z3Expr_op2 tp1 tp1 L1Type_prop Z3.mkEq) @@ e1 @@ e2
z3equals (Z3Type_fun in_tp1 tp) f1 f2 =
  z3forall in_tp1 (\x -> z3equals tp (f1 @@ x) (f2 @@ x))

-- | Build a 'Z3Prop' from a 'Z3.AST'
z3ast_prop :: Z3.AST -> Z3Prop
z3ast_prop ast = Z3Expr_ast L1Type_prop ast

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
  Z3Expr_app (Z3Expr_op1 L1Type_prop L1Type_prop Z3.mkNot) e


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
logic2Z3_of_expr :: L1Type a -> Z3Expr a -> Logic2Z3 tag a
logic2Z3_of_expr (L1Type_lit lit_tp) e = Logic2Z3_lit e
logic2Z3_of_expr L1Type_ptr e = Logic2Z3_ptr e
logic2Z3_of_expr L1Type_prop e = Logic2Z3_prop e

-- | Extract a 'Z3Expr' from a 'Logic2Z3' of base type
expr_of_logic2Z3 :: L1Type a -> Logic2Z3 tag a -> Z3Expr a
expr_of_logic2Z3 _ (Logic2Z3_lit e) = e
expr_of_logic2Z3 _ (Logic2Z3_ptr e) = e
expr_of_logic2Z3 _ (Logic2Z3_prop e) = e
expr_of_logic2Z3 z3tp1 (Logic2Z3_fun _) = no_functional_l1type z3tp1
expr_of_logic2Z3 z3tp1 (Logic2Z3_pm _) = no_pm_l1type z3tp1
expr_of_logic2Z3 z3tp1 (Logic2Z3_pm_unit _) = no_pm_l1type z3tp1

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
logic2Z3_op1 :: LitType a -> L1Type b -> (Z3.AST -> Z3.Z3 Z3.AST) ->
                Logic2Z3 tag (Literal a -> b)
logic2Z3_op1 lit_tp z3tp1 e_f =
  Logic2Z3_fun $ \(Logic2Z3_lit e_arg) ->
  logic2Z3_of_expr z3tp1 $
  (Z3Expr_op $ Z3Expr_op1 (L1Type_lit lit_tp) z3tp1 e_f) @@ e_arg

-- | Build a Logic2Z3 from a binary Z3 operation
logic2Z3_op2 :: LitType a1 -> LitType a2 -> L1Type b ->
                (Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST) ->
                Logic2Z3 tag (Literal a1 -> Literal a2 -> b)
logic2Z3_op2 lit_tp1 lit_tp2 z3tp1 e_f =
  Logic2Z3_fun $ \(Logic2Z3_lit e_arg1) ->
  Logic2Z3_fun $ \(Logic2Z3_lit e_arg2) ->
  logic2Z3_of_expr z3tp1 $
  (Z3Expr_op $
   Z3Expr_op2 (L1Type_lit lit_tp1) (L1Type_lit lit_tp2) z3tp1 e_f)
  @@ e_arg1 @@ e_arg2

-- CommutesWithArrow instance for Logic2Z3
instance CommutesWithArrow (Logic2Z3 tag) where
  interpApply (Logic2Z3_fun f) = f
  interpLambda = Logic2Z3_fun

-- LExprAlgebra instance for Logic2Z3
instance LExprAlgebra tag (Logic2Z3 tag) where
  interpOp (Op_Literal ltp x) = Logic2Z3_lit $ Z3Expr_lit ltp x
  interpOp (Op_arith1 ltp aop) =
    logic2Z3_op1 ltp (L1Type_lit ltp) $ aop1_fun aop
    where
      aop1_fun Op1_Abs = error "Z3 absolute value function not (yet?) supported"
      aop1_fun Op1_Signum = error "Z3 signum function not (yet?) supported"
      aop1_fun Op1_Neg = Z3.mkUnaryMinus
      aop1_fun Op1_Complement = Z3.mkBvnot
  interpOp (Op_arith2 ltp aop) =
    logic2Z3_op2 ltp ltp (L1Type_lit ltp) $ aop2_fun ltp aop
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
    logic2Z3_op1 ltp_from (L1Type_lit ltp_to) $ coerce_fun ltp_from ltp_to
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
    logic2Z3_op2 ltp ltp (L1Type_lit LitType_bool) $ cmp_fun acmp
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
    logic2Z3_op1 LitType_bool L1Type_prop return
  interpOp (Op_forall l1tp) =
    Logic2Z3_fun $ \(Logic2Z3_fun body_f) ->
    Logic2Z3_prop $ z3forall l1tp $ \x ->
    case body_f (logic2Z3_of_expr l1tp x) of
      Logic2Z3_prop p -> p
  interpOp (Op_exists l1tp) =
    Logic2Z3_fun $ \(Logic2Z3_fun body_f) ->
    Logic2Z3_prop $ z3exists l1tp $ \x ->
    case body_f (logic2Z3_of_expr l1tp x) of
      Logic2Z3_prop p -> p
  interpOp (Op_returnP l1tp) =
    Logic2Z3_fun $ \x ->
    Logic2Z3_pm $ return $ expr_of_logic2Z3 l1tp x
  interpOp (Op_bindP l1tp_a l1tp_b) =
    Logic2Z3_fun $ \m ->
    Logic2Z3_fun $ \(Logic2Z3_fun f) ->
    Logic2Z3_pm (pm_of_logic2z3 m >>= \x ->
                  pm_of_logic2z3 (f $ logic2Z3_of_expr l1tp_a x))
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
-- Converting logical types to Z3 sorts
----------------------------------------------------------------------

-- | Get the 'Z3.Sort' for a 'L1Type'
l1type_to_sort :: L1Type a -> Z3m Z3.Sort
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

-- * Predefined 'Z3.Sort's for useful types
z3_sort_ptr = l1type_to_sort L1Type_ptr
z3_sort_int = l1type_to_sort (L1Type_lit LitType_int)

-- | Get the input and output 'Z3.Sort's for a 'Z3Type'
z3type_to_fun_sorts :: Z3Type a -> Z3m ([Z3.Sort],Z3.Sort)
z3type_to_fun_sorts (Z3Type_base z3tp1) =
  liftM (\sort -> ([], sort)) $ l1type_to_sort z3tp1
z3type_to_fun_sorts (Z3Type_fun in_z3tp1 out_z3tp) =
  liftM2 (\sort (sorts,out_sort) -> (sort:sorts, out_sort))
  (l1type_to_sort in_z3tp1) (z3type_to_fun_sorts out_z3tp)


----------------------------------------------------------------------
-- Converting from Z3Expr to Z3 ASTs
----------------------------------------------------------------------

-- | Result type for converting a 'Z3Expr' to Z3
data Z3Res a where
  Z3Res_base :: L1Type a -> Z3m Z3.AST -> Z3Res a
  Z3Res_fun :: L1Type a -> (Z3.AST -> Z3Res b) -> Z3Res (a -> b)

-- | Extract a computation of a 'Z3.AST' from a 'Z3Res' of base type
z3res_to_ast :: L1Type a -> Z3Res a -> Z3m Z3.AST
z3res_to_ast _ (Z3Res_base _ m) = m
z3res_to_ast l1tp (Z3Res_fun _ _) = no_functional_l1type l1tp

-- | Convert a 'Z3Expr' to Z3
z3expr_to_z3 :: Z3Ctx ctx -> Mb ctx (Z3Expr a) -> Z3Res a
z3expr_to_z3 ctx [nuP| Z3Expr_var l1tp n |] =
  case mbNameBoundP n of
    Left memb -> z3ctx_lookup ctx memb
    Right n -> error "z3expr_to_z3: unbound name!"
z3expr_to_z3 ctx [nuP| Z3Expr_lit LitType_unit () |] =
  Z3Res_base l1typeRep $
  do z3info <- get
     return $ unit_ctor z3info
z3expr_to_z3 ctx [nuP| Z3Expr_lit LitType_bool b |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkBool $ mbLift b
z3expr_to_z3 ctx [nuP| Z3Expr_lit LitType_int i |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkInteger $ mbLift i
z3expr_to_z3 ctx [nuP| Z3Expr_lit lit_tp@LitType_bits x |] =
  Z3Res_base l1typeRep $
  do sort <- l1type_to_sort (L1Type_lit lit_tp)
     inBase $ Z3.mkIntegral sort $ mbLift x
z3expr_to_z3 ctx [nuP| Z3Expr_prop b |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkBool $ mbLift b
z3expr_to_z3 ctx [nuP| Z3Expr_ast l1tp ast |] =
  Z3Res_base l1tp $ return ast
z3expr_to_z3 ctx [nuP| Z3Expr_multi _ l1tp_out f args |] =
  Z3Res_base l1tp_out $ inBase $ f args
z3expr_to_z3 ctx [nuP| Z3Expr_op op |] = z3op_to_z3 ctx op
z3expr_to_z3 ctx [nuP| Z3Expr_forall l1tp body |] =
  error "FIXME HERE NOW"
z3expr_to_z3 ctx [nuP| Z3Expr_exists l1tp body |] =
  error "FIXME HERE NOW"
z3expr_to_z3 ctx [nuP| Z3Expr_and es |] =
  Z3Res_base L1Type_prop $
  do asts <- mapM (z3res_to_ast L1Type_prop . z3expr_to_z3 ctx) es
     inBase $ Z3.mkAnd asts
z3expr_to_z3 ctx [nuP| Z3Expr_or es |] =
  Z3Res_base L1Type_prop $
  do asts <- mapM (\e -> case z3expr_to_z3 ctx e of
                      Z3Res_base _ m -> m) es
     inBase $ Z3.mkOr asts
z3expr_to_z3 ctx [nuP| Z3Expr_true |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkTrue
z3expr_to_z3 ctx [nuP| Z3Expr_false |] =
  Z3Res_base l1typeRep $ inBase $ Z3.mkFalse

z3op_to_z3 :: Z3Ctx ctx -> Z3AppliedOp a -> Z3Res a
z3op_to_z3 ctx [nuP| Z3Expr_fdecl z3tp fdecl |] =
  error "FIXME HERE NOW"
z3op_to_z3 ctx [nuP| Z3Expr_op1 l1tp_a l1tp_b f |] =
  Z3Res_fun l1tp_a $ \res1 ->
  Z3Res_base l1tp_b $
  do ast1 <- z3res_to_ast l1tp_a res1
     inBase $ f ast1
z3op_to_z3 ctx [nuP| Z3Expr_op2 l1tp_a l1tp_b l1tp_c f |] =
  Z3Res_fun l1tp_a $ \res1 ->
  Z3Res_fun l1tp_b $ \res2 ->
  Z3Res_base l1tp_c $
  do ast1 <- z3res_to_ast l1tp_a res1
     ast2 <- z3res_to_ast l1tp_b res2
     inBase $ f ast1 ast2
z3op_to_z3 ctx [nuP| Z3Expr_app f arg |] =
  case z3op_to_z3 ctx f of
    Z3Res_fun g -> g $ z3expr_to_z3 arg
-}
