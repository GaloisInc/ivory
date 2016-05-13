{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, TypeOperators, DataKinds, TemplateHaskell, QuasiQuotes,
    ViewPatterns, RankNTypes #-}

module Ivory.ModelCheck.Logic2Z3 where

import Prelude ()
import Prelude.Compat hiding (exp)

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
-- Z3 types
----------------------------------------------------------------------

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

-- | Higher-order Z3 types. Note that the use of 'Z3Type1' in the domains of
-- function types technically means these are second-order types.
data Z3Type a where
  Z3Type_base :: Z3Type1 a -> Z3Type a
  Z3Type_PM :: Z3Type1 a -> Z3Type (PM a)
  Z3Type_fun :: Z3Type1 a -> Z3Type b -> Z3Type (a -> b)

-- | Convert a @'Z3Type1' a@ to an @'L1Type' a@
z3type1_to_l1type :: Z3Type1 a -> L1Type a
z3type1_to_l1type (Z3Type_lit ltp) = L1Type_lit ltp
z3type1_to_l1type Z3Type_ptr = L1Type_ptr
z3type1_to_l1type Z3Type_prop = L1Type_prop

-- | Return a base name for each type, to be used for variables
z3type1_base_name :: Z3Type1 a -> String
z3type1_base_name = error "write z3type1_base_name!"


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
  Z3Ctx_EVar :: Z3Ctx ctx -> Z3Type1 a -> Z3.AST -> Z3Ctx (ctx :> a)
  -- ^ Add an existential variable to the context, which is associated with a Z3
  -- constant symbol, given as a 'Z3.AST'
  Z3Ctx_UVar :: Z3Ctx ctx -> Z3Type1 a -> Z3Ctx (ctx :> a)
  -- ^ Add a universal variable to the context

-- | Look up a variable in a 'Z3Ctx', returning either a 'Z3.AST', for an
-- existential variable, or a deBruijn index for a universal variable
z3ctx_lookup :: Z3Ctx ctx -> Member ctx a ->
                (Z3Type1 a, Either Z3.AST Integer)
z3ctx_lookup (Z3Ctx_EVar _ z3tp1 ast) Member_Base = (z3tp1, Left ast)
z3ctx_lookup (Z3Ctx_UVar _ z3tp1) Member_Base = (z3tp1, Right 0)
z3ctx_lookup (Z3Ctx_EVar ctx _ _) (Member_Step memb) = z3ctx_lookup ctx memb
z3ctx_lookup (Z3Ctx_UVar ctx _) (Member_Step memb) =
  bimap id (bimap id (+ 1)) $ z3ctx_lookup ctx memb


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
    unsigned_bv_sorts :: (Int, Z3.Sort),
    -- | Association list of signed bit-vector types from their lengths
    signed_bv_sorts :: (Int, Z3.Sort)
  }

-- | Monad built on top of 'Z3' for building Z3 expressions relative to a given
-- variable context. The monad uses the state information in a 'Z3Info', and
-- reads a 'Z3Ctx' for the current variable context.
newtype Z3m ctx a =
  Z3m { runZ3m :: ReaderT (Z3Ctx ctx) (StateT Z3Info Z3.Z3) a }
  deriving (Functor,Applicative,Monad)

instance StateM (Z3m ctx) Z3Info where
  get = Z3m get
  set x = Z3m $ set x

instance ReaderM (Z3m ctx) (Z3Ctx ctx) where
  ask = Z3m ask

instance RunReaderM (Z3m ctx) (Z3Ctx ctx) where
  local ctx (Z3m m) = Z3m $ local ctx m

-- | A version of 'local' that can change the @ctx@ variable
localCtx :: Z3Ctx ctx' -> Z3m ctx' a -> Z3m ctx a
localCtx ctx (Z3m m) =
  Z3m $ lift $ runReaderT ctx m

instance BaseM (Z3m ctx) Z3.Z3 where
  inBase m = Z3m $ lift $ lift m

instance RunM Z3.Z3 a (Maybe Z3.Logic -> Z3Opts.Opts -> IO a) where
  runM m = \logic opts -> Z3.evalZ3With logic opts m

instance RunM (Z3m ctx) a (Z3Ctx ctx -> Z3Info ->
                           Maybe Z3.Logic -> Z3Opts.Opts ->
                           IO (a, Z3Info)) where
  runM m = runM $ runZ3m m

----------------------------------------------------------------------
-- Converting logical types to Z3 types and sorts
----------------------------------------------------------------------

-- | Get the 'Z3.Sort' for a 'Z3Type'
z3type1_to_sort :: Z3Type1 a -> Z3m ctx Z3.Sort
z3type1_to_sort z3tp = error "Implement z3type1_sort!"


----------------------------------------------------------------------
-- Context-related monadic operations
----------------------------------------------------------------------

-- | Make a fresh Z3 symbol
mkFreshSym :: String -> Z3m ctx Z3.Symbol
mkFreshSym str =
  do z3info <- get
     sym <- inBase $ Z3.mkStringSymbol $ str ++ show (fresh_id z3info)
     set $ z3info { fresh_id = fresh_id z3info + 1 }
     return sym

-- | Run a 'Z3m' computation in a context extended with a universal variable
withUVar :: Z3Type1 a -> Z3m (ctx :> a) b -> Z3m ctx b
withUVar z3tp1 m =
  do ctx <- ask
     localCtx (Z3Ctx_UVar ctx z3tp1) m

-- | Run a 'Z3m' computation in a context extended with an existential variable
withEVar :: Z3Type1 a -> Z3m (ctx :> a) b -> Z3m ctx b
withEVar z3tp1 m =
  do ctx <- ask
     sort <- z3type1_to_sort z3tp1
     sym <- mkFreshSym $ z3type1_base_name z3tp1
     ast <- inBase $ Z3.mkVar sym sort
     localCtx (Z3Ctx_EVar ctx z3tp1 ast) m


----------------------------------------------------------------------
-- Typed Z3 expressions
----------------------------------------------------------------------

-- | Type-level K combinator, for constant type functions
newtype K a b = K { unK :: a }

-- | The representation of a 'Memory' in Z3
data Z3Memory mm =
  Z3Memory
  {
    z3memArrays :: MapList (K Z3.FuncDecl) mm,
    -- ^ One Z3 function for each storable type
    z3memPtrArray :: Z3.FuncDecl,
    -- ^ Z3 function for storing the 'Ptr' type
    z3memLengths :: Z3.FuncDecl,
    -- ^ Z3 function containing the lengths of the various arrays
    z3memLastAlloc :: Z3.AST
    -- ^ Z3 expression containing the last-allocated pointer value
  }

-- | FIXME: documentation
data Z3Expr tag ctx a where
  Z3Expr_ast :: Z3Type1 a -> Z3.AST -> Z3Expr tag ctx a
{-
  Z3Expr_mem :: Z3Memory (Storables tag) ->
                Z3Expr ctx (Memory (Storables tag))
-}
  Z3Expr_fun :: (Z3Expr tag ctx a -> Z3m ctx (Z3Expr tag ctx b)) ->
                Z3Expr tag ctx (a -> b)
  Z3Expr_pm :: Z3Type1 a ->
               (Z3Memory (Storables tag) -> Z3Expr tag ctx a ->
                Z3Memory (Storables tag) -> Z3m ctx (Z3Expr tag ctx Prop)) ->
               Z3Expr tag ctx (PM a)


----------------------------------------------------------------------
-- Expression-building operations
----------------------------------------------------------------------

-- | Build a Z3 expression for a free variable inside the 'Z3m' monad
z3var :: Member ctx a -> Z3m ctx (Z3Expr tag ctx a)
z3var memb =
  do ctx <- ask
     case z3ctx_lookup ctx memb of
       (z3tp1, Left ast) -> return (Z3Expr_ast z3tp1 ast)
       (z3tp1, Right i) ->
         do z3sort <- z3type1_to_sort z3tp1
            ast <- inBase $ Z3.mkBound (fromInteger i) z3sort
            return (Z3Expr_ast z3tp1 ast)

-- | Build a Z3 expression for a literal
z3literal :: LitType a -> a -> Z3m ctx (Z3Expr tag ctx (Literal a))
z3literal LitType_unit () =
  do z3info <- get
     ast <- inBase $ Z3.mkApp (unit_ctor z3info) []
     return $ Z3Expr_ast z3type1Rep ast
z3literal LitType_bool bool =
  do ast <- inBase $ Z3.mkBool bool
     return $ Z3Expr_ast z3type1Rep ast
z3literal LitType_int i =
  do z3info <- get
     ast <- inBase $ Z3.mkNumeral (show i) (integer_sort z3info)
     return $ Z3Expr_ast z3type1Rep ast
z3literal lit_tp@LitType_bits i =
  do sort <- z3type1_to_sort (Z3Type_lit lit_tp)
     ast <- inBase $ Z3.mkNumeral (error "FIXME: print an arbitrary FiniteBits") sort
     return $ Z3Expr_ast (Z3Type_lit lit_tp) ast


----------------------------------------------------------------------
-- Translating Ops
----------------------------------------------------------------------

-- | Helper type that bundles 'Z3m' together with 'Z3Expr'
newtype Z3InterpRes tag ctx a =
  Z3InterpRes { runZ3InterpRes :: Z3m ctx (Z3Expr tag ctx a) }

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
