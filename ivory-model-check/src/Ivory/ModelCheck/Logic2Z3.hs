{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, ScopedTypeVariables,
    TypeOperators, DataKinds, EmptyCase,
    TemplateHaskell, QuasiQuotes, ViewPatterns, RankNTypes #-}

module Ivory.ModelCheck.Logic2Z3 where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Bits
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
data Z3Info tag =
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
    signed_bv_sorts :: [(Int, Z3.Sort)],
    -- | List of exceptions that have been seen so far
    seen_exceptions :: [LException tag]
  }

-- | Monad built on top of 'Z3' for building Z3 expressions relative to a given
-- variable context. The monad uses the state information in a 'Z3Info', and
-- reads a 'Z3Ctx' for the current variable context.
newtype Z3m tag ctx a =
  Z3m { runZ3m :: ReaderT (Z3Ctx ctx) (StateT (Z3Info tag) Z3.Z3) a }
  deriving (Functor,Applicative,Monad)

instance StateM (Z3m tag ctx) (Z3Info tag) where
  get = Z3m get
  set x = Z3m $ set x

instance ReaderM (Z3m tag ctx) (Z3Ctx ctx) where
  ask = Z3m ask

instance RunReaderM (Z3m tag ctx) (Z3Ctx ctx) where
  local ctx (Z3m m) = Z3m $ local ctx m

-- | A version of 'local' that can change the @ctx@ variable
localCtx :: Z3Ctx ctx' -> Z3m tag ctx' a -> Z3m tag ctx a
localCtx ctx (Z3m m) =
  Z3m $ lift $ runReaderT ctx m

instance BaseM (Z3m tag ctx) Z3.Z3 where
  inBase m = Z3m $ lift $ lift m

instance RunM Z3.Z3 a (Maybe Z3.Logic -> Z3Opts.Opts -> IO a) where
  runM m = \logic opts -> Z3.evalZ3With logic opts m

instance RunM (Z3m tag ctx) a (Z3Ctx ctx -> Z3Info tag ->
                               Maybe Z3.Logic -> Z3Opts.Opts ->
                               IO (a, Z3Info tag)) where
  runM m = runM $ runZ3m m

-- | Look up the position of an exception in the 'seen_exceptions' list, adding
-- it to the list if necessary
intForExceptionM :: Eq (LException tag) => LException tag ->
                    Z3m tag ctx Integer
intForExceptionM exc =
  do z3info <- get
     let seen_excs = seen_exceptions z3info
     case elemIndex exc seen_excs of
       Just i -> return $ toInteger i
       Nothing ->
         do set $ z3info { seen_exceptions = seen_excs ++ [exc] }
            return $ toInteger $ length seen_excs

-- | Look up an exception by number
exceptionForIntM :: Integer -> Z3m tag ctx (LException tag)
exceptionForIntM i =
  do z3info <- get
     return $ genericIndex (seen_exceptions z3info) i


----------------------------------------------------------------------
-- Converting logical types to Z3 types and sorts
----------------------------------------------------------------------

-- | Get the 'Z3.Sort' for a 'Z3Type1'
z3type1_to_sort :: Z3Type1 a -> Z3m tag ctx Z3.Sort
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
z3type_to_fun_sorts :: Z3Type a -> Z3m tag ctx ([Z3.Sort],Z3.Sort)
z3type_to_fun_sorts (Z3Type_base z3tp1) =
  liftM (\sort -> ([], sort)) $ z3type1_to_sort z3tp1
z3type_to_fun_sorts (Z3Type_fun in_z3tp1 out_z3tp) =
  liftM2 (\sort (sorts,out_sort) -> (sort:sorts, out_sort))
  (z3type1_to_sort in_z3tp1) (z3type_to_fun_sorts out_z3tp)


----------------------------------------------------------------------
-- Context manipulation in the Z3m monad
----------------------------------------------------------------------

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


----------------------------------------------------------------------
-- Typed Z3 expressions
----------------------------------------------------------------------

-- | The Z3 representation of memory arrays of a given type
data Z3MemFun a = Z3MemFun (Z3Type1 a) Z3.FuncDecl

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

-- | FIXME: documentation
data Z3Expr tag ctx a where
  Z3Expr_lit :: LitType a -> a -> Z3Expr tag ctx (Literal a)
  Z3Expr_prop :: Bool -> Z3Expr tag ctx Prop
  Z3Expr_ast :: Z3Type1 a -> Z3m tag ctx Z3.AST -> Z3Expr tag ctx a
{-
  Z3Expr_mem :: Z3Memory (LStorables tag) ->
                Z3Expr ctx (Memory (LStorables tag))
-}
  Z3Expr_fun :: (Z3Expr tag ctx a -> Z3Expr tag ctx b) ->
                Z3Expr tag ctx (a -> b)
  Z3Expr_pm :: Z3Type1 a ->
               Z3PM tag ctx (Z3Expr tag ctx a) ->
               Z3Expr tag ctx (PM a)

-- | FIXME: documentation
newtype Z3PM tag ctx a =
  Z3PM { runZ3PM ::
           Z3Memory (LStorables tag) ->
           (Maybe (Z3PMRet tag ctx a),
            [(LException tag, Z3PMRet tag ctx ())]) }

-- | Helper type for defining 'Z3PM'
type Z3PMRet tag ctx a =
  (a, [Z3Expr tag ctx Prop], Z3Memory (LStorables tag))

-- | Helper type to represent a 'Z3Expr' in a 'Z3m' computation
newtype Z3ExprM tag ctx a =
  Z3ExprM { runZ3ExprM :: Z3m tag ctx (Z3Expr tag ctx a) }


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
-- The Z3 predicate monad
----------------------------------------------------------------------

instance Functor (Z3PM tag ctx) where
  fmap f (Z3PM m) =
    Z3PM $ \mem ->
    do (maybe_pmret, exc_pmrets) <- m mem
       case maybe_pmret of
         Just (a, props, mem') -> return (Just (f a, props, mem'), exc_pmrets)
         Nothing -> return (Nothing, exc_pmrets)


instance Applicative (Z3PM tag ctx) where
  -- pure x = Z3PM $ \mem -> return (Just (x, [], mem), [])

instance Eq (LException tag) => Monad (Z3PM tag ctx) where
  return x = Z3PM $ \mem -> return (Just (x, [], mem), [])
  m >>= f = Z3PM $ \mem ->
    do (maybe_ret_m, exc_ms) <- runZ3PM m mem
       case maybe_ret_m of
         Just (x, props, mem') ->
           do (ret_m, exc_ms') <- runZ3PM (f x) mem'
              exc_ms'' <- joinAListsM joinPMRets exc_ms exc_ms'
              return (ret_m, exc_ms'')
         Nothing -> return (Nothing, exc_ms)

-- | Monad for joining memories
type JoinMemM tag ctx a =
  WriterT ([Z3Expr tag ctx Prop], [Z3Expr tag ctx Prop]) (Z3m tag ctx) a

joinFDecls :: String -> Z3Type a -> Z3.FuncDecl -> Z3.FuncDecl ->
                 JoinMemM tag ctx Z3.FuncDecl
joinFDecls base_name z3tp fdecl1 fdecl2 =
  if fdecl1 == fdecl2 then
    return fdecl1
  else
    do fdecl <- lift $ mkFreshFDecl base_name z3tp
       prop1 <-
         lift $ runZ3ExprM $
         z3equals z3tp @@ z3expr_fdecl z3tp fdecl @@ z3expr_fdecl z3tp fdecl1
       prop2 <-
         lift $ runZ3ExprM $
         z3equals z3tp @@ z3expr_fdecl z3tp fdecl @@ z3expr_fdecl z3tp fdecl2
       put ([prop1], [prop2])
       return fdecl

joinASTs :: String -> Z3Type1 a -> Z3.AST -> Z3.AST ->
            JoinMemM tag ctx Z3.AST
joinASTs base_name z3tp1 ast1 ast2 =
  if ast1 == ast2 then
    return ast1
  else
    do ast <- mkFreshConst base_name z3tp1
       prop1 <-
         runZ3ExprM $
         z3equals (Z3Type_base z3tp1) @@
         Z3Expr_ast z3tp1 ast @@ Z3Expr_ast z3tp1 ast1
       prop2 <-
         runZ3ExprM $
         z3equals (Z3Type_base z3tp1) @@
         Z3Expr_ast z3tp1 ast @@ Z3Expr_ast z3tp1 ast2
       put ([prop1], [prop2])
       return ast

joinMemArrays :: MapList Z3MemFun mm -> MapList Z3MemFun mm ->
                 JoinMemM tag ctx (MapList Z3MemFun mm)
joinMemArrays Nil Nil = return Nil
joinMemArrays (Cons (Z3MemFun z3tp1 fdecl1) fs1) (Cons (Z3MemFun _ fdecl2) fs2) =
  do fdecl' <- joinFDecls z3tp1 fdecl1 fdecl2
     fs' <- joinMemArrays fs1 fs2
     return $ Cons (Z3MemFun z3tp1 fdecl1) fs'

-- | FIXME: documentation
joinZ3Memories ::
  Z3Memory mm -> Z3Memory mm ->
  (Z3Memory mm, ([Z3Expr tag ctx Prop], [Z3Expr tag ctx Prop]))
joinZ3Memories mem1 mem2 = error "FIXME: write joinZ3Memories"

-- | FIXME: documentation
joinPMRets :: Z3PMRet tag ctx () -> Z3PMRet tag ctx () ->
              Z3m tag ctx (Z3PMRet tag ctx ())
joinPMRets (_, props1, mem1) (_, props2, mem2) =
  do (mem', (mem_props1, mem_props2)) <- joinZ3Memories mem1 mem2
     prop_ret <-
       runZ3ExprM $
       z3or @@ (z3and_exprs $ props1 ++ mem_props1) @@
       (z3and_exprs $ props2 ++ mem_props2)
     return ((), [prop_ret], mem')
-}

----------------------------------------------------------------------
-- Translating Ops
----------------------------------------------------------------------

{-
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
