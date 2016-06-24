{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances,
    TypeOperators, DataKinds, EmptyCase, NamedFieldPuns,
    TemplateHaskell, QuasiQuotes, ViewPatterns, RankNTypes, KindSignatures #-}

module Ivory.ModelCheck.Logic2Z3 where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Bits
import Data.Word
import Data.List
import Data.Typeable
import Numeric.Natural

import MonadLib
import MonadLib.Monads
--import MonadLib (StateM(..),StateT(..),runStateT,
--                 ReaderM(..),RunReaderM(..),ReaderT,runReaderT,MonadT(..))
--import Control.Monad.IO.Class (MonadIO(..))

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
  Z3Ctx_EVar :: Z3Ctx ctx -> L1Type a -> AST a -> Z3Ctx (ctx :> a)
  -- ^ Add an existential variable to the context, which is associated with a Z3
  -- constant symbol, given as an 'AST'
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
  = Z3CtxLookupRes_EVar (L1Type a) (AST a)
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

-- | This is something like the resumption monad, but with an extra functor...
newtype ResumpT f m a =
  ResumpT { runResumpT :: m (Either (f (ResumpT f m a)) a) }

instance (Monad m, Functor f) => Monad (ResumpT f m) where
  return x = ResumpT $ return $ Right x
  ResumpT m >>= f =
    ResumpT $ do frm_or_a <- m
                 case frm_or_a of
                   Left frm -> return $ Left $ fmap (\m' -> m' >>= f) frm
                   Right a -> runResumpT (f a)

instance (Monad m, Functor f) => Functor (ResumpT f m) where
instance (Monad m, Functor f) => Applicative (ResumpT f m) where

instance (Functor f, StateM m s) => StateM (ResumpT f m) s where
  get = ResumpT $ Right <$> get
  set s = ResumpT $ (set s >> return (Right ()))

instance Functor f => MonadT (ResumpT f) where
  lift m = ResumpT $ Right <$> m

-- | Lift an 'f a' into 'ResumpT'
liftFResumpT :: (Monad m, Functor f) => f a -> ResumpT f m a
liftFResumpT fa = ResumpT $ return $ Left $ fmap return fa

instance (Functor f, RunM m (Either (f (ResumpT f m a)) a) r) =>
         RunM (ResumpT f m) a r where
  runM m = runM (runResumpT m)

-- | Run a 'ResumpT' computation, assuming that @m@ can be coerced to an @f@
runResumpToF :: (Monad m, Monad f) => (forall b. m b -> f b) ->
                 ResumpT f m a -> f a
runResumpToF run_m (ResumpT m) =
  run_m m >>= \frm_or_a ->
  case frm_or_a of
    Left frm -> frm >>= runResumpToF run_m
    Right a -> return a

{-
instance (Functor f, RunM m a r) => RunM (ResumpT m) a r where
  runM m = runM (runResumpT m)
-}


-- | Monad built on top of 'Z3' for building Z3 expressions relative to a given
-- variable context. The monad uses the state information in a 'Z3Info', reads a
-- 'Z3Ctx' for the current variable context, and also outputs a set of
-- assertions, given as type 'AST', to pass to the current solver. Additionally,
-- we use a resumption monad structure to embed the 'IO' monad, since, even the
-- the 'Z3.Z3' monad contains 'IO', it requires the MonadIO class, which is in
-- the transformers packatge...
newtype Z3m ctx a =
  Z3m { runZ3m :: ReaderT (Z3Ctx ctx)
                  (StateT (Z3Info, [AST Prop]) (ResumpT IO Z3.Z3)) a }
  deriving (Functor, Applicative, Monad)

instance ReaderM (Z3m ctx) (Z3Ctx ctx) where
  ask = Z3m $ ask

instance RunReaderM (Z3m ctx) (Z3Ctx ctx) where
  local ctx (Z3m m) = Z3m $ local ctx m

localCtx :: Z3Ctx ctx -> Z3m ctx a -> Z3m ctx' a
localCtx ctx (Z3m m) = Z3m $ lift $ runReaderT ctx m

instance StateM (Z3m ctx) Z3Info where
  get = Z3m $ fst <$> get
  set info =
    Z3m $ sets_ $ \(_, props) -> (info, props)

instance WriterM (Z3m ctx) [AST Prop] where
  put props =
    Z3m $ do (info, props') <- get
             set (info, props' ++ props)

instance RunWriterM (Z3m ctx) [AST Prop] where
  collect (Z3m m) =
    Z3m $ do (info, orig_props) <- get
             set (info, [])
             ret <- m
             (info', props') <- get
             set (info', orig_props)
             return (ret, props')

instance BaseM (Z3m ctx) Z3.Z3 where
  inBase m = Z3m $ lift $ lift $ lift m

-- | Lift an 'IO' computation into 'Z3m'
liftIO :: IO a -> Z3m ctx a
liftIO io = Z3m $ lift $ lift $ liftFResumpT io

-- | The options we will pass to Z3 by default
defaultZ3Options :: Z3Opts.Opts
defaultZ3Options = Z3Opts.stdOpts

-- | The logic we use in Z3, where 'Nothing' is the Z3 default logic
defaultZ3Logic :: Maybe Z3.Logic
defaultZ3Logic = Nothing

instance RunM Z3.Z3 a (Maybe Z3.Logic -> Z3Opts.Opts -> IO a) where
  runM m logic opts = Z3.evalZ3With logic opts m

instance RunM (Z3m ctx) a (Maybe Z3.Logic -> Z3Opts.Opts ->
                           Z3Ctx ctx -> IO a) where
  runM (Z3m m) logic opts ctx =
    liftM fst $ runResumpToF (Z3.evalZ3With logic opts) $
    lift mkZ3Info >>= \info ->
    runStateT (info, []) $ 
    runReaderT ctx m

-- | Run a 'Z3m' computation, using the default Z3 options and logic and an
-- existential 'Z3Ctx'
evalZ3m :: MapRList L1FunType ctx -> Z3m ctx a -> IO a
evalZ3m tps m =
  runM (mkZ3Ctx tps >>= \ctx -> localCtx ctx m)
  defaultZ3Logic defaultZ3Options Z3Ctx_nil


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

-- | Make a fresh constant as an 'AST' from a first-order type
mkFreshConstant :: L1Type a -> Z3m ctx (AST a)
mkFreshConstant l1tp =
  do sym <- mkFreshSymbol (l1type_base_name l1tp)
     sort <- l1type_to_sort l1tp
     AST <$> (inBase $ Z3.mkConst sym sort)

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
withEVar :: L1Type a -> Z3m (ctx :> a) b -> Z3m ctx (AST a, b)
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
-- Typed Z3 ASTs
----------------------------------------------------------------------

-- | Typed Z3 ASTs
newtype AST a = AST { unAST :: Z3.AST }

-- | Helper function to build an 'AST' from a 'Z3.Z3' computation
astInBase :: Z3.Z3 Z3.AST -> Z3m ctx (AST a)
astInBase m = AST <$> inBase m

-- | Lift a unary Z3 function to a function on typed 'AST's in 'Z3m'
z3LiftUnary :: (Z3.AST -> Z3.Z3 Z3.AST) -> AST a -> Z3m ctx (AST a)
z3LiftUnary z3f (AST z3ast) = astInBase $ z3f z3ast

-- | Lift a binary Z3 function to a function on typed 'AST's in 'Z3m'
z3LiftBinary :: (Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST) -> AST a -> AST a ->
                Z3m ctx (AST a)
z3LiftBinary z3f (AST z3ast1) (AST z3ast2) =
  astInBase $ z3f z3ast1 z3ast2

-- | Turn any literal into an 'AST'
z3Literal :: LitType a -> a -> Z3m ctx (AST (Literal a))
z3Literal LitType_unit () =
  do f <- liftM unit_ctor get
     astInBase $ Z3.mkApp f []
z3Literal LitType_bool b = astInBase $ Z3.mkBool b
z3Literal LitType_int i = astInBase $ Z3.mkInteger i
z3Literal lit_tp@LitType_bits bv =
    do sort <- l1type_to_sort (L1Type_lit lit_tp)
       astInBase $ Z3.mkIntegral bv sort

-- | The 'AST' for the null pointer
z3NullPtr :: Z3m ctx (AST Ptr)
z3NullPtr = astInBase $ Z3.mkInteger 0

-- | The 'AST' for a global variable pointer. NOTE: global variables are
-- represented as *negative* numbers.
z3GlobalVar :: Natural -> Z3m ctx (AST Ptr)
z3GlobalVar n = astInBase $ Z3.mkInteger (- (toInteger n))

-- | The 'AST' for the next pointer
z3NextPtr :: AST Ptr -> Z3m ctx (AST Ptr)
z3NextPtr (AST z3ast) =
  do z3ast_one <- inBase $ Z3.mkInteger 1
     astInBase $ Z3.mkAdd [z3ast, z3ast_one]

-- | The absolute value function on 'AST's
z3Abs :: LitType a -> AST (Literal a) -> Z3m ctx (AST (Literal a))
z3Abs _ _ = error "Z3 absolute value function not (yet?) supported"

-- | The signum function on 'AST's
z3Signum :: LitType a -> AST (Literal a) -> Z3m ctx (AST (Literal a))
z3Signum _ _ = error "Z3 signum function not (yet?) supported"

-- | The negation function on 'AST's
z3Neg :: LitType a -> AST (Literal a) -> Z3m ctx (AST (Literal a))
z3Neg LitType_unit = return
z3Neg LitType_bool = z3LiftUnary Z3.mkNot
z3Neg LitType_int = z3LiftUnary Z3.mkUnaryMinus
z3Neg lit_tp@LitType_bits = z3LiftUnary Z3.mkBvneg

-- | The bit complementation function on 'AST's
z3Complement :: LitType a -> AST (Literal a) -> Z3m ctx (AST (Literal a))
z3Complement lit_tp@LitType_bits = z3LiftUnary Z3.mkBvnot
z3Complement _ =
  error "Z3 complementation function called on non-bitvector value!"

-- | Apply an 'ArithOp1' to an 'AST'
z3ArithOp1 :: ArithOp1 -> LitType a -> AST (Literal a) ->
              Z3m ctx (AST (Literal a))
z3ArithOp1 Op1_Abs = z3Abs
z3ArithOp1 Op1_Signum = z3Signum
z3ArithOp1 Op1_Neg = z3Neg
z3ArithOp1 Op1_Complement = z3Complement

-- | The binary addition function on 'AST's
z3Add :: LitType a -> AST (Literal a) -> AST (Literal a) ->
         Z3m ctx (AST (Literal a))
z3Add LitType_unit = \_ -> return
z3Add LitType_bool = z3LiftBinary (\x y -> Z3.mkOr [x,y])
z3Add LitType_int = z3LiftBinary (\x y -> Z3.mkAdd [x,y])
z3Add LitType_bits = z3LiftBinary Z3.mkBvadd

-- | The binary subtraction function on 'AST's
z3Sub :: LitType a -> AST (Literal a) -> AST (Literal a) ->
         Z3m ctx (AST (Literal a))
z3Sub LitType_unit = \_ -> return
z3Sub LitType_bool = error "Z3 conversion: subtraction on Booleans!"
z3Sub LitType_int = z3LiftBinary (\x y -> Z3.mkSub [x,y])
z3Sub LitType_bits = z3LiftBinary Z3.mkBvsub

-- | The binary multiplication function on 'AST's
z3Mult :: LitType a -> AST (Literal a) -> AST (Literal a) ->
          Z3m ctx (AST (Literal a))
z3Mult LitType_unit = \_ -> return
z3Mult LitType_bool = z3LiftBinary (\x y -> Z3.mkAnd [x,y])
z3Mult LitType_int = z3LiftBinary (\x y -> Z3.mkMul [x,y])
z3Mult LitType_bits = z3LiftBinary Z3.mkBvmul

-- | The binary division function on 'AST's
z3Div :: LitType a -> AST (Literal a) -> AST (Literal a) ->
         Z3m ctx (AST (Literal a))
z3Div LitType_unit = \_ -> return
z3Div LitType_bool = error "Z3 conversion: division on Booleans!"
z3Div LitType_int = z3LiftBinary Z3.mkDiv
z3Div (LitType_bits :: LitType bv)
  | isSigned (0 :: bv) = z3LiftBinary Z3.mkBvsdiv
z3Div (LitType_bits :: LitType bv) = z3LiftBinary Z3.mkBvudiv

-- | The binary modulo function on 'AST's
z3Mod :: LitType a -> AST (Literal a) -> AST (Literal a) ->
         Z3m ctx (AST (Literal a))
z3Mod LitType_unit = \_ -> return
z3Mod LitType_bool = error "Z3 conversion: modulus on Booleans!"
z3Mod LitType_int = z3LiftBinary Z3.mkMod
z3Mod (LitType_bits :: LitType bv)
  | isSigned (0 :: bv) = z3LiftBinary Z3.mkBvsmod
z3Mod (LitType_bits :: LitType bv) = z3LiftBinary Z3.mkBvurem

-- | The bitwise and function on 'AST's
z3BitAnd :: LitType a -> AST (Literal a) -> AST (Literal a) ->
            Z3m ctx (AST (Literal a))
z3BitAnd LitType_bits = z3LiftBinary Z3.mkBvand
z3BitAnd _ = error "Z3 bitwise and function called on non-bitvector values!"

-- | The bitwise and function on 'AST's
z3BitOr :: LitType a -> AST (Literal a) -> AST (Literal a) ->
           Z3m ctx (AST (Literal a))
z3BitOr LitType_bits = z3LiftBinary Z3.mkBvor
z3BitOr _ = error "Z3 bitwise or function called on non-bitvector values!"

-- | The bitwise xor function on 'AST's
z3BitXor :: LitType a -> AST (Literal a) -> AST (Literal a) ->
            Z3m ctx (AST (Literal a))
z3BitXor LitType_bits = z3LiftBinary Z3.mkBvxor
z3BitXor _ = error "Z3 bitwise xor function called on non-bitvector values!"

-- | Apply an 'ArithOp2' to an 'AST'
z3ArithOp2 :: ArithOp2 -> LitType a -> AST (Literal a) -> AST (Literal a) ->
              Z3m ctx (AST (Literal a))
z3ArithOp2 Op2_Add = z3Add
z3ArithOp2 Op2_Sub = z3Sub
z3ArithOp2 Op2_Mult = z3Mult
z3ArithOp2 Op2_Div = z3Div
z3ArithOp2 Op2_Mod = z3Mod
z3ArithOp2 Op2_BitAnd = z3BitAnd
z3ArithOp2 Op2_BitOr = z3BitOr
z3ArithOp2 Op2_BitXor = z3BitXor

-- | The equality function on 'AST's with Boolean type
z3EqBool :: AST a -> AST a -> Z3m ctx (AST (Literal Bool))
z3EqBool (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkEq z3ast1 z3ast2

-- | The less-than function on 'AST's
z3Lt :: L1Type a -> AST a -> AST a -> Z3m ctx (AST (Literal Bool))
z3Lt (L1Type_lit LitType_unit) _ _ =
  error "Z3 less-than function called on unit values!"
z3Lt (L1Type_lit LitType_unit) _ _ =
  error "Z3 less-than function called on Boolean values!"
z3Lt (L1Type_lit LitType_int) (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkLt z3ast1 z3ast2
z3Lt (L1Type_lit (LitType_bits :: LitType bv)) (AST z3ast1) (AST z3ast2)
  | isSigned (0 :: bv) = astInBase $ Z3.mkBvslt z3ast1 z3ast2
z3Lt (L1Type_lit LitType_bits) (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkBvult z3ast1 z3ast2
z3Lt L1Type_ptr (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkLt z3ast1 z3ast2
z3Lt L1Type_prop _ _ =
  error "Z3 less-than function called on propositional values!"

-- | The less-than-or-equal-to function on 'AST's
z3Le :: L1Type a -> AST a -> AST a -> Z3m ctx (AST (Literal Bool))
z3Le (L1Type_lit LitType_unit) _ _ =
  error "Z3 less-than function called on unit values!"
z3Le (L1Type_lit LitType_bool) _ _ =
  error "Z3 less-than function called on Boolean values!"
z3Le (L1Type_lit LitType_int) (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkLe z3ast1 z3ast2
z3Le (L1Type_lit (LitType_bits :: LitType bv)) (AST z3ast1) (AST z3ast2)
  | isSigned (0 :: bv) = astInBase $ Z3.mkBvsle z3ast1 z3ast2
z3Le (L1Type_lit LitType_bits) (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkBvule z3ast1 z3ast2
z3Le L1Type_ptr (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkLe z3ast1 z3ast2
z3Le L1Type_prop _ _ =
  error "Z3 less-than-or-equal-to function called on propositional values!"

-- | Apply an 'ArithCmp' to two 'Z3.AST's
z3ArithCmp :: ArithCmp -> L1Type a -> AST a -> AST a ->
              Z3m ctx (AST (Literal Bool))
z3ArithCmp OpCmp_EQ = \_ -> z3EqBool
z3ArithCmp OpCmp_LT = z3Lt
z3ArithCmp OpCmp_LE = z3Le

-- | Apply a coercion function from one 'LitType' to another to an 'AST'
z3Coerce :: LitType f -> LitType t -> AST (Literal f) ->
            Z3m ctx (AST (Literal t))
z3Coerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) (AST z3ast)
  | finiteBitSize (0 :: t) == finiteBitSize (0 :: f) =
    return (AST z3ast)
z3Coerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) (AST z3ast)
  | isSigned (0 :: f) && finiteBitSize (0 :: t) > finiteBitSize (0 :: f) =
    astInBase $
    Z3.mkSignExt (finiteBitSize (0 :: t) - finiteBitSize (0 :: f)) z3ast
z3Coerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) (AST z3ast)
  | finiteBitSize (0 :: t) > finiteBitSize (0 :: f) =
    astInBase $
    Z3.mkZeroExt (finiteBitSize (0 :: t) - finiteBitSize (0 :: f)) z3ast
z3Coerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) (AST z3ast) =
  astInBase $ Z3.mkExtract (finiteBitSize (0 :: t) - 1) 0 z3ast
z3Coerce LitType_int (LitType_bits :: LitType t) (AST z3ast) =
  astInBase $ Z3.mkInt2bv (finiteBitSize (0 :: t)) z3ast
z3Coerce (LitType_bits :: LitType f) LitType_int (AST z3ast) =
  astInBase $ Z3.mkBv2int z3ast (isSigned (0 :: f))
z3Coerce LitType_int LitType_int ast = return ast
z3Coerce LitType_bool LitType_bool ast = return ast
z3Coerce LitType_unit LitType_unit ast = return ast
z3Coerce _ _ _ = error "Z3 Coercion not supported!"

-- | Build an if-then-else expression
z3IfThenElse :: AST (Literal Bool) -> AST a -> AST a -> Z3m ctx (AST a)
z3IfThenElse (AST z3ast_cond) (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkIte z3ast_cond z3ast1 z3ast2

-- | The "true" 'AST'
z3True :: Z3m ctx (AST Prop)
z3True = astInBase $ Z3.mkTrue

-- | The "false" 'AST'
z3False :: Z3m ctx (AST Prop)
z3False = astInBase $ Z3.mkFalse

-- | Build a conjunction
z3And :: AST Prop -> AST Prop -> Z3m ctx (AST Prop)
z3And = z3LiftBinary $ \x y -> Z3.mkAnd [x,y]

-- | Build a disjunction
z3Or :: AST Prop -> AST Prop -> Z3m ctx (AST Prop)
z3Or = z3LiftBinary $ \x y -> Z3.mkOr [x,y]

-- | Build a negation
z3Not :: AST Prop -> Z3m ctx (AST Prop)
z3Not = z3LiftUnary Z3.mkNot

-- | The equality function on 'AST's
z3Eq :: AST a -> AST a -> Z3m ctx (AST Prop)
z3Eq (AST z3ast1) (AST z3ast2) =
  astInBase $ Z3.mkEq z3ast1 z3ast2

-- | Convert a Boolean 'AST' to a propositional one
z3IsTrue :: AST (Literal Bool) -> Z3m ctx (AST Prop)
z3IsTrue (AST z3ast1) = return $ AST z3ast1

-- | Build a universal quantifier around a computation of an 'AST'
z3Forall :: L1Type a -> Z3m (ctx ':> a) (AST Prop) -> Z3m ctx (AST Prop)
z3Forall l1tp body_m =
  do body_ast <- withUVar l1tp body_m
     sym <- mkFreshSymbol (l1type_base_name l1tp)
     sort <- l1type_to_sort l1tp
     astInBase $ Z3.mkForall [] [sym] [sort] $ unAST body_ast

-- | Build an existential quantifier around a computation of an 'AST'
z3Exists :: L1Type a -> Z3m (ctx ':> a) (AST Prop) -> Z3m ctx (AST Prop)
z3Exists l1tp body_m =
     snd <$> withEVar l1tp body_m

-- | Build a let-binding around a computation of an 'AST'
z3Let :: L1Type a -> AST a -> Z3m (ctx ':> a) (AST Prop) ->
         Z3m ctx (AST Prop)
z3Let l1tp rhs body_m =
    do (const_ast, ret) <- withEVar l1tp body_m
       equality_ast <- z3Eq const_ast rhs
       put [equality_ast]
       return ret


----------------------------------------------------------------------
-- Converting from our reachability logic into Z3
----------------------------------------------------------------------

-- | Helper type for an 'AST' inside a 'Z3m' computation
newtype Z3m_AST ctx a = Z3m_AST { unZ3m_AST :: Z3m ctx (AST a) }

-- | Helper function for extracting a 'Z3.AST' from a typed argument passed to
-- 'interpOpB' or 'interpVarB' in a 'BindingApply'
extractL1ArgAST :: L1Type a -> BindingApply Z3m_AST ctx a -> Z3m ctx (AST a)
extractL1ArgAST l1tp arg =
  unZ3m_AST $ elimL1BindingApplyF l1tp $ unBindingApply arg

-- | Apply a Z3 function constant to a list of arguments
z3ApplyArgs :: Z3.FuncDecl -> LTypeArgs a args ret ->
               MapList (BindingApply Z3m_AST ctx) args ->
               Z3m ctx (AST ret)
z3ApplyArgs fdecl tp_args args = helper fdecl [] tp_args args where
  helper :: Z3.FuncDecl -> [Z3.AST] -> LTypeArgs a args ret ->
            MapList (BindingApply Z3m_AST ctx) args ->
            Z3m ctx (AST ret)
  helper fdecl prev_args (LTypeArgs_base _) _ =
    astInBase $ Z3.mkApp fdecl (reverse prev_args)
  helper fdecl prev_args (LTypeArgs_pm _) _ =
    astInBase $ Z3.mkApp fdecl (reverse prev_args)
  helper fdecl prev_args (LTypeArgs_fun (LType_base l1tp) tp_args) args =
    do let (arg_ba, args') = ml_first_rest args
       arg <- extractL1ArgAST l1tp arg_ba
       helper fdecl (unAST arg : prev_args) tp_args args'
  helper _ _ (LTypeArgs_fun _ _) _ =
    error "z3ApplyArgs: converting a higher-order variable to Z3!"

-- | Helper function for extracting a 'Z3.AST' from an argument of type
-- @'Literal' a@ passed to 'interpOpB' or 'interpVarB'
extractLitArgAST :: BindingApply Z3m_AST ctx (Literal a) ->
                    Z3m ctx (AST (Literal a))
extractLitArgAST arg = unZ3m_AST $ unBindingApply arg

-- | Helper function for extracting a 'Z3.AST' from an argument of type 'Ptr'
-- passed to 'interpOpB' or 'interpVarB'
extractPtrArgAST :: BindingApply Z3m_AST ctx Ptr -> Z3m ctx (AST Ptr)
extractPtrArgAST arg = unZ3m_AST $ unBindingApply arg

-- | Helper function for extracting a 'Z3.AST' from an argument of type 'Prop'
-- passed to 'interpOpB' or 'interpVarB'
extractPropArgAST :: BindingApply Z3m_AST ctx Prop -> Z3m ctx (AST Prop)
extractPropArgAST arg = unZ3m_AST $ unBindingApply arg

-- Instance for converting LExprs to Z3 ASTs
instance LBindingExprAlgebra tag Z3m_AST where

  -- Interpret a variable, given as a Member in the current context, into Z3
  interpVarB _ tp_args memb args =
    Z3m_AST $
    do z3ctx <- ask
       case (z3ctx_lookup z3ctx memb, tp_args) of
         (Z3CtxLookupRes_EVar _ ast, LTypeArgs_base _) ->
           return ast
         (Z3CtxLookupRes_EVar l1tp _, LTypeArgs_pm _) ->
           no_pm_l1type l1tp
         (Z3CtxLookupRes_EVar l1tp _, LTypeArgs_fun _ _) ->
           no_functional_l1type l1tp
         (Z3CtxLookupRes_FVar _ fdecl, _) ->
           z3ApplyArgs fdecl tp_args args
         (Z3CtxLookupRes_UVar l1tp i, LTypeArgs_base _) ->
           do sort <- l1type_to_sort l1tp
              astInBase $ Z3.mkBound (fromInteger i) sort
         (Z3CtxLookupRes_UVar l1tp _, LTypeArgs_pm _) ->
           no_pm_l1type l1tp
         (Z3CtxLookupRes_UVar l1tp i, LTypeArgs_fun _ _) ->
           no_functional_l1type l1tp

  -- Interpret literals into Z3
  interpOpB (Op_Literal lit_tp x) _ = Z3m_AST $ z3Literal lit_tp x

  -- Interpret the arithmetic operations into Z3
  interpOpB (Op_arith1 lit_tp aop) (ml_first -> arg1) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 z3ArithOp1 aop lit_tp ast1
  interpOpB (Op_arith2 lit_tp aop) (ml_12 -> (arg1, arg2)) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractLitArgAST arg2
                 z3ArithOp2 aop lit_tp ast1 ast2
  interpOpB (Op_coerce lit_tp_from lit_tp_to) (ml_first -> arg1) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 z3Coerce lit_tp_from lit_tp_to ast1
  interpOpB (Op_cmp lit_tp acmp) (ml_12 -> (arg1, arg2)) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractLitArgAST arg2
                 z3ArithCmp acmp (L1Type_lit lit_tp) ast1 ast2
  interpOpB (Op_cond l1tp@(L1Type_lit _)) (ml_123 -> (arg1, arg2, arg3)) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractL1ArgAST l1tp arg2
                 ast3 <- extractL1ArgAST l1tp arg3
                 z3IfThenElse ast1 ast2 ast3
  interpOpB (Op_cond l1tp@L1Type_ptr) (ml_123 -> (arg1, arg2, arg3)) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractL1ArgAST l1tp arg2
                 ast3 <- extractL1ArgAST l1tp arg3
                 z3IfThenElse ast1 ast2 ast3
  interpOpB (Op_cond l1tp@L1Type_prop) (ml_123 -> (arg1, arg2, arg3)) =
    Z3m_AST $ do ast1 <- extractLitArgAST arg1
                 ast2 <- extractL1ArgAST l1tp arg2
                 ast3 <- extractL1ArgAST l1tp arg3
                 z3IfThenElse ast1 ast2 ast3
  interpOpB Op_null_ptr _ = Z3m_AST z3NullPtr
  interpOpB (Op_global_var n) _ = Z3m_AST $ z3GlobalVar n
  interpOpB Op_next_ptr (ml_first -> arg1) =
    Z3m_AST $ do ast1 <- extractPtrArgAST arg1
                 z3NextPtr ast1
  interpOpB (Op_ptr_cmp acmp) (ml_12 -> (arg1, arg2)) =
    Z3m_AST $ do ast1 <- extractPtrArgAST arg1
                 ast2 <- extractPtrArgAST arg2
                 z3ArithCmp acmp l1typeRep ast1 ast2

  -- Interpret the first-order propositional operations into Z3
  interpOpB Op_true _ = Z3m_AST z3True
  interpOpB Op_false _ = Z3m_AST z3False
  interpOpB Op_and (ml_12 -> (arg1, arg2)) =
    Z3m_AST $ do ast1 <- extractPropArgAST arg1
                 ast2 <- extractPropArgAST arg2
                 z3And ast1 ast2
  interpOpB Op_or (ml_12 -> (arg1, arg2)) =
    Z3m_AST $ do ast1 <- extractPropArgAST arg1
                 ast2 <- extractPropArgAST arg2
                 z3Or ast1 ast2
  interpOpB Op_not (ml_first -> arg1) =
    Z3m_AST $ do ast1 <- extractPropArgAST arg1
                 z3Not ast1
  interpOpB (Op_eq l1tp) (ml_12 -> (arg1, arg2)) =
    Z3m_AST $ do ast1 <- extractL1ArgAST l1tp arg1
                 ast2 <- extractL1ArgAST l1tp arg2
                 z3Eq ast1 ast2
  interpOpB Op_istrue (ml_first -> arg1) =
    -- NOTE: Op_istrue is the identity when converted to Z3
    Z3m_AST $ (z3IsTrue <=< extractLitArgAST) arg1

  -- Interpret the quantifiers into Z3
  interpOpB (Op_forall l1tp) (ml_first -> (BindingApply (Z3m_AST body_m))) =
    Z3m_AST $ z3Forall l1tp body_m
  interpOpB (Op_exists l1tp) (ml_first -> (BindingApply (Z3m_AST body_m))) =
    Z3m_AST $ z3Exists l1tp body_m
  interpOpB (Op_let l1tp) (ml_12 -> (rhs, BindingApply (Z3m_AST body_m))) =
    Z3m_AST $ do rhs_ast <- extractL1ArgAST l1tp rhs
                 z3Let l1tp rhs_ast body_m

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
lprop_to_z3ast :: LProp tag -> Z3m RNil (AST Prop)
lprop_to_z3ast prop =
  unZ3m_AST $ interpExprB Proxy prop

-- | Top-level call to convert an 'LProp'-in-binding into a Z3 expression
mb_lprop_to_z3ast :: Mb ctx (LProp tag) -> Z3m ctx (AST Prop)
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
  liftM2 (:>:) (evalZ3Ctx z3model ctx) (evalAST z3model l1tp $ unAST ast)
evalZ3Ctx z3model (Z3Ctx_FVar ctx ftp fdecl) =
  liftM2 (:>:) (evalZ3Ctx z3model ctx) (evalFuncDecl z3model ftp fdecl)
evalZ3Ctx z3model (Z3Ctx_UVar _ _) =
  error "evalZ3Ctx: formula has a top-level universal variable!"

-- | Dummy type to indicate the Z3 solver in the 'SMTSolver' class. Includes a
-- debugging level.
data Z3Solver = Z3Solver { z3DebugLevel :: Int,
                           z3Opts :: Z3Opts.Opts,
                           z3Logic :: Maybe Z3.Logic }

-- | Default 'Z3Solver'
defaultZ3Solver = Z3Solver { z3DebugLevel = 0,
                             z3Opts = Z3Opts.stdOpts,
                             z3Logic = Nothing }

-- | Conditionally perform a computation if the debug level of the given solver
-- is at least the indicated level
z3ifDebug :: Int -> Z3Solver -> Z3m ctx () -> Z3m ctx ()
z3ifDebug level solver m =
  if z3DebugLevel solver >= level then m else return ()

-- | Conditionally print a string to stdout if the debug level of the given
-- solver is at least the indicated level
z3debug :: Int -> Z3Solver -> String -> Z3m ctx ()
z3debug level solver str =
  z3ifDebug level solver $ liftIO (putStrLn str)

instance SMTSolver Z3Solver where
  smtSetDebugLevel level solver = solver { z3DebugLevel = level }
  smtSolve solver const_tps props =
    evalZ3m const_tps $
    do (converted_asts, collected_asts) <-
         collect $ mapM mb_lprop_to_z3ast props
       let z3_props = map unAST $ collected_asts ++ converted_asts
       z3ifDebug 1 solver $ do
         liftIO $ putStrLn ""
         liftIO $ putStrLn "Performing Z3 query:"
         mapM_ (liftIO . putStrLn <=< inBase . Z3.astToString) z3_props
         liftIO $ putStrLn ""
       result <- inBase $ Z3.solverCheckAssumptions z3_props
       case result of
         Z3.Sat ->
           do ctx <- ask
              z3debug 1 solver "Z3: query is satisfiable!"
              z3model <- inBase Z3.solverGetModel
              vals <- inBase $ evalZ3Ctx z3model ctx
              return (SMT_sat vals)
         Z3.Unsat ->
           z3debug 1 solver "Z3: query is unsatisfiable!" >>
           return SMT_unsat
         Z3.Undef ->
           do err_str <- inBase Z3.solverGetReasonUnknown
              z3debug 1 solver ("Z3: error running query: " ++ err_str)
              return $ SMT_unknown err_str
