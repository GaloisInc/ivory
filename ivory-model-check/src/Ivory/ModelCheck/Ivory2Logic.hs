{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}

module Ivory.ModelCheck.Ivory2Logic where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Bits
import Data.Word
import Data.Int
import Data.List
import Data.Typeable
import Text.PrettyPrint
import Numeric
import Numeric.Natural

import Debug.Trace

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MonadLib
import MonadLib.Monads

import Data.Binding.Hobbits
import Ivory.ModelCheck.Logic

import qualified Ivory.Language.Syntax.Names as I
import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Language.Syntax.AST as I


----------------------------------------------------------------------
-- Top-level options for converting Ivory to logic
----------------------------------------------------------------------

-- | The options that can be passed to the Ivory reachability solver
data ILOpts =
  ILOpts
  {
    debugLevel :: Int,
    -- ^ Debugging level: 0 = no debugging; 1 = print queries and results; 2 =
    -- print everything
    skipCompilerAsserts :: Bool,
    -- ^ Whether to skip (i.e., not generate asserts for) the compiler-inserted
    -- assertions
    loopUnrollingMax :: Integer,
    -- ^ The maximum number of times to unroll a loop; i.e., all loops with
    -- maximum bound no greater than this number will be unrolled
    inlineCalls :: Bool
    -- ^ Whether to inline function calls
  }

-- | Default options for the Ivory reachability solver
defaultOpts = ILOpts { debugLevel = 0, skipCompilerAsserts = False,
                       loopUnrollingMax = 5, inlineCalls = False }


----------------------------------------------------------------------
-- The Ivory version of the reachability logic
----------------------------------------------------------------------

-- | Tag type indicating the Ivory reachability logic
data IvoryLogic

-- | Typed expressions in the Ivory reachability logic
type ILExpr = LExpr IvoryLogic

-- | Propositions in the Ivory reachability logic
type ILProp = LProp IvoryLogic

-- | Transition relations in the Ivory reachability logic
type ILPM = LPM IvoryLogic

-- | The exceptions in the Ivory reachability logic, which include all
-- non-local exits
data IvoryExn
  = IvoryError
    -- ^ An error, i.e., an assertion failure
  | IvoryBreak
    -- ^ A break out of a loop
  | IvoryReturn
    -- ^ A return from a function
  deriving Eq

$(mkNuMatching [t| IvoryExn |])

instance Liftable IvoryExn where
  mbLift [nuP| IvoryError |] = IvoryError
  mbLift [nuP| IvoryBreak |] = IvoryBreak
  mbLift [nuP| IvoryReturn |] = IvoryReturn

-- Gives the exception type and memory model for the Ivory reachability logic
instance LExprTag IvoryLogic where
  type LStorables IvoryLogic = '[ Word64 , Rational ]
  type LException IvoryLogic = IvoryExn

-- | The type of a 'Memory' used by Ivory
type IvoryMemory = Memory '[ Word64, Rational ]


----------------------------------------------------------------------
-- A GADT version of the Ivory types
----------------------------------------------------------------------

-- | First-order Ivory types
data I1Type a where
  ITyVoid :: I1Type (Literal ())
  ITyBits :: (Typeable a, Integral a, FiniteBits a, Liftable a) =>
             I1Type (Literal a)
  ITyIndex :: Integer -> I1Type (Literal Int32)
  ITyBool :: I1Type (Literal Bool)
  ITyChar :: I1Type (Literal Char)
  ITyFloat :: I1Type (Literal Rational)
  ITyDouble :: I1Type (Literal Rational)
  ITyPtr :: IPtrSort -> I1Type a -> I1Type Ptr
  ITyArrayPtr :: Maybe IPtrSort -> Int -> I1Type a -> I1Type Ptr
  ITyStructPtr :: Maybe IPtrSort -> String -> I1Type Ptr
  ITyCArrayPtr :: Maybe IPtrSort -> I1Type a -> I1Type Ptr
  ITyOpaque :: I1Type (Literal ())

-- | The different sorts of pointers in Ivory
data IPtrSort
  = IPtrSort_Ptr
  | IPtrSort_Ref
  | IPtrSort_ConstRef
  deriving Eq

-- | Convert a first-order Ivory type to a first-order logical type
ivory2L1Type :: I1Type a -> L1Type a
ivory2L1Type ITyVoid = l1typeRep
ivory2L1Type ITyBits = L1Type_lit LitType_bits
ivory2L1Type (ITyIndex _) = l1typeRep
ivory2L1Type ITyBool = l1typeRep
ivory2L1Type ITyChar = error "ivory2L1Type: Char type not yet implemented!"
ivory2L1Type ITyFloat = l1typeRep
ivory2L1Type ITyDouble = l1typeRep
ivory2L1Type (ITyPtr _ _) = l1typeRep
ivory2L1Type (ITyArrayPtr _ _ _) = l1typeRep
ivory2L1Type (ITyStructPtr _ _) = l1typeRep
ivory2L1Type (ITyCArrayPtr _ _) = l1typeRep
ivory2L1Type ITyOpaque = l1typeRep

-- | Some existentially-quantified first-order Ivory type
data SomeI1Type where
  SomeI1Type :: I1Type a -> SomeI1Type

-- | Flatten a pointer sort into a function on Ivory types
flattenPtrSort :: Maybe IPtrSort -> I.Type -> I.Type
flattenPtrSort Nothing itp = itp
flattenPtrSort (Just IPtrSort_Ptr) itp = I.TyPtr itp
flattenPtrSort (Just IPtrSort_Ref) itp = I.TyRef itp
flattenPtrSort (Just IPtrSort_ConstRef) itp = I.TyConstRef itp

-- | Convert a GADT Ivory type to its "flat" counterpart
flattenI1Type :: I1Type a -> I.Type
flattenI1Type ITyVoid = I.TyVoid
flattenI1Type (ITyBits :: I1Type lit_bv) =
  -- README: because we cannot directly match the type of ITyBits as having the
  -- form Literal bv for some bv, we instead match this type against the type
  -- variable lit_bv, then use unLiteral to extract 0 of type bv
  let zero_bv = unLiteral (Literal 0 :: lit_bv) in
  case (isSigned zero_bv, finiteBitSize zero_bv) of
    (True, 8) -> I.TyInt I.Int8
    (True, 16) -> I.TyInt I.Int16
    (True, 32) -> I.TyInt I.Int32
    (True, 64) -> I.TyInt I.Int64
    (False, 8) -> I.TyWord I.Word8
    (False, 16) -> I.TyWord I.Word16
    (False, 32) -> I.TyWord I.Word32
    (False, 64) -> I.TyWord I.Word64
    (s, bits) ->
      error ("flattenI1Type: unexpected bit-vector type: "
             ++ (if s then "signed" else "unsigned") ++ ", "
             ++ show bits ++ " bits")
flattenI1Type (ITyIndex i) = I.TyIndex i
flattenI1Type ITyBool = I.TyBool
flattenI1Type ITyChar = I.TyChar
flattenI1Type ITyFloat = I.TyFloat
flattenI1Type ITyDouble = I.TyDouble
flattenI1Type (ITyPtr ptr_sort i1tp) =
  flattenPtrSort (Just ptr_sort) $ flattenI1Type i1tp
flattenI1Type (ITyArrayPtr maybe_ptr_sort len i1tp) =
  flattenPtrSort maybe_ptr_sort $ I.TyArr len $ flattenI1Type i1tp
flattenI1Type (ITyStructPtr maybe_ptr_sort s_name) =
  flattenPtrSort maybe_ptr_sort $ I.TyStruct s_name
flattenI1Type (ITyCArrayPtr maybe_ptr_sort i1tp) =
  flattenPtrSort maybe_ptr_sort $ I.TyCArray $ flattenI1Type i1tp
flattenI1Type ITyOpaque = I.TyOpaque

-- Show first-order Ivory types by showing their flat counterparts
instance Show (I1Type a) where
  show i1tp = show (flattenI1Type i1tp)

-- | Add a pointer sort to an Ivory logic type, coalescing pointers to structs
-- and arrays into the single struct / array pointer types
addPtrSort :: IPtrSort -> I1Type a -> I1Type Ptr
addPtrSort ps i1tp@(ITyPtr _ _) = ITyPtr ps i1tp
addPtrSort ps (ITyArrayPtr Nothing len i1tp) = ITyArrayPtr (Just ps) len i1tp
addPtrSort ps i1tp@(ITyArrayPtr (Just _) _ _) = ITyPtr ps i1tp
addPtrSort ps (ITyStructPtr Nothing s_name) = ITyStructPtr (Just ps) s_name
addPtrSort ps i1tp@(ITyStructPtr (Just _) _) = ITyPtr ps i1tp
addPtrSort ps (ITyCArrayPtr Nothing i1tp) = ITyCArrayPtr (Just ps) i1tp
addPtrSort ps i1tp@(ITyCArrayPtr (Just _) _) = ITyPtr ps i1tp
addPtrSort ps i1tp = ITyPtr ps i1tp

-- | The same as 'addPtrSort', but for existentially quantified Ivory types
addPtrSortSome :: IPtrSort -> SomeI1Type -> SomeI1Type
addPtrSortSome ps (SomeI1Type i1tp) = SomeI1Type $ addPtrSort ps i1tp

-- | Helper for 'addPtrSort' using the 'IPtrSort_Ref' sort
mkRefType :: I1Type a -> I1Type Ptr
mkRefType i1tp = addPtrSort IPtrSort_Ref i1tp

-- | Convert a "flat" Ivory type into its GADT representation
convertType :: I.Type -> SomeI1Type
convertType I.TyVoid = SomeI1Type ITyVoid
convertType (I.TyInt I.Int8) = SomeI1Type (ITyBits :: I1Type (Literal Int8))
convertType (I.TyInt I.Int16) = SomeI1Type (ITyBits :: I1Type (Literal Int16))
convertType (I.TyInt I.Int32) = SomeI1Type (ITyBits :: I1Type (Literal Int32))
convertType (I.TyInt I.Int64) = SomeI1Type (ITyBits :: I1Type (Literal Int64))
convertType (I.TyWord I.Word8) = SomeI1Type (ITyBits :: I1Type (Literal Word8))
convertType (I.TyWord I.Word16) = SomeI1Type (ITyBits :: I1Type (Literal Word16))
convertType (I.TyWord I.Word32) = SomeI1Type (ITyBits :: I1Type (Literal Word32))
convertType (I.TyWord I.Word64) = SomeI1Type (ITyBits :: I1Type (Literal Word64))
convertType (I.TyIndex upper_bound) = SomeI1Type (ITyIndex upper_bound)
convertType I.TyBool = SomeI1Type ITyBool
convertType I.TyChar = SomeI1Type ITyChar
convertType I.TyFloat = SomeI1Type ITyFloat
convertType I.TyDouble = SomeI1Type ITyDouble
convertType (I.TyProc out_type in_types) =
  error "convertType: cannot (yet) handle function types"
convertType (I.TyRef itp) = addPtrSortSome IPtrSort_Ref $ convertType itp
convertType (I.TyConstRef itp) =
  addPtrSortSome IPtrSort_ConstRef $ convertType itp
convertType (I.TyPtr itp) = addPtrSortSome IPtrSort_Ptr $ convertType itp
convertType (I.TyArr len itp) =
  case convertType itp of
    SomeI1Type i1tp -> SomeI1Type $ ITyArrayPtr Nothing len i1tp
convertType (I.TyStruct s_name) = SomeI1Type $ ITyStructPtr Nothing s_name
convertType (I.TyCArray itp) =
  case convertType itp of
    SomeI1Type i1tp -> SomeI1Type $ ITyCArrayPtr Nothing i1tp
convertType I.TyOpaque = SomeI1Type ITyOpaque

-- | Test equality for Ivory types
i1TypeEq :: I1Type a -> I1Type b -> Maybe (a :~: b)
i1TypeEq ITyVoid ITyVoid = Just Refl
i1TypeEq ITyVoid _ = Nothing
i1TypeEq ITyBits ITyBits = eqT
i1TypeEq ITyBits _ = Nothing
i1TypeEq (ITyIndex i) (ITyIndex j) | i == j = Just Refl
i1TypeEq (ITyIndex _) _ = Nothing
i1TypeEq ITyBool ITyBool = Just Refl
i1TypeEq ITyBool _ = Nothing
i1TypeEq ITyChar ITyChar = Just Refl
i1TypeEq ITyChar _ = Nothing
i1TypeEq ITyFloat ITyFloat = Just Refl
i1TypeEq ITyFloat _ = Nothing
i1TypeEq ITyDouble ITyDouble = Just Refl
i1TypeEq ITyDouble _ = Nothing
i1TypeEq (ITyPtr ps i1tp) (ITyPtr ps' i1tp')
  | ps == ps'
  = case i1TypeEq i1tp i1tp' of
      Just Refl -> Just Refl
      Nothing -> Nothing
i1TypeEq (ITyPtr _ _) _ = Nothing
i1TypeEq (ITyArrayPtr ps len i1tp) (ITyArrayPtr ps' len' i1tp')
  | len == len' && ps == ps'
  = case i1TypeEq i1tp i1tp' of
      Just Refl -> Just Refl
      Nothing -> Nothing
i1TypeEq (ITyArrayPtr _ _ _) _ = Nothing
i1TypeEq (ITyStructPtr ps s_name) (ITyStructPtr ps' s_name')
  | s_name == s_name' && ps == ps' = Just Refl
i1TypeEq (ITyStructPtr _ _) _ = Nothing
i1TypeEq (ITyCArrayPtr ps i1tp) (ITyCArrayPtr ps' i1tp')
  | ps == ps'
  = case i1TypeEq i1tp i1tp' of
      Just Refl -> Just Refl
      Nothing -> Nothing
i1TypeEq (ITyCArrayPtr _ _) _ = Nothing
i1TypeEq ITyOpaque ITyOpaque = Just Refl
i1TypeEq ITyOpaque _ = Nothing


----------------------------------------------------------------------
-- Monad for converting Ivory expressions into reachability logic
----------------------------------------------------------------------

-- | An Ivory variable that has been converted into Ivory reachability logic, by
-- associating it with a first-order type and a variable name
data SomeILName where SomeILName :: I1Type a -> Name a -> SomeILName

-- | The state information used for building transition relations
data I2LInfo =
  I2LInfo
  {
    i2l_opts :: ILOpts,
    -- ^ The configuration options passed in for converting to logic
    i2l_modules :: [I.Module],
    -- ^ The Ivory modules that are in scope
    i2l_vars :: Map.Map I.Var SomeILName,
    -- ^ The Ivory variables that are in scope, and the Ivory
    -- reachability logic variables associated with them
    i2l_syms :: Map.Map I.Sym Natural,
    -- ^ An association from Ivory global symbols and unique numbers for them
    i2l_single_err :: Maybe (Name (Literal Rational -> Literal Rational)),
    -- ^ A function for the relative round-off error for the @Float@ type
    i2l_double_err :: Maybe (Name (Literal Rational -> Literal Rational))
    -- ^ A function for the relative round-off error for the @Double@ type
  }

-- | The monad for building transition relations from Ivory expressions
newtype I2LM a =
  I2LM { runIvory2LogicM :: StateT I2LInfo (ContT ILPM Id) a }
  deriving (Functor, Applicative, Monad)

instance StateM I2LM I2LInfo where
  get = I2LM get
  set info = I2LM $ set info

-- | Typeclass for monads that allow capturing the current continuation as a
-- pure function
class Monad m => ShiftResetPureM m r | m -> r where
  shiftPureM :: ((a -> r) -> r) -> m a
  resetPureM :: m r -> m r

instance ShiftResetPureM (ContT r Id) r where
  shiftPureM f =
    callCC $ \cc ->
    let k = runId . runContT return . cc
    in abort =<< return (f k)
  resetPureM m = return $ runId $ runContT return m

instance ShiftResetPureM m r => ShiftResetPureM (StateT i m) r where
  shiftPureM f =
    do s <- get
       liftM fst $ lift $ shiftPureM $ \k -> f (\x -> k (x,s))
  resetPureM m =
    do s <- get
       lift $ resetPureM $ liftM fst $ runStateT s m

instance ShiftResetPureM I2LM ILPM where
  shiftPureM f = I2LM $ shiftPureM f
  resetPureM (I2LM m) = I2LM $ resetPureM m

-- | Add a transition to the output, by shifting the continuation and sequencing
-- its results after the given transition
addTransition :: ILPM -> I2LM ()
addTransition pm = shiftPureM $ \k -> mkSeqP pm (k ())

-- | Add a bind transition to the output, that binds the result of a transition
-- with a value to a fresh variable
addBindTransition :: LTypeable a => ILExpr (PM a) -> I2LM (ILExpr a)
addBindTransition pm = shiftPureM $ \k -> mkBindP pm k

-- | Use 'resetPureM' computation to collect a computation's transition(s)
collectTransitions :: I2LM () -> I2LM ILPM
collectTransitions m =
  resetPureM (m >> return (mkReturnP $ mkLiteral ()))

-- | Add a catch for the given exception around the transitions generated by a
-- given computation
ivoryCatch :: IvoryExn -> I2LM () -> I2LM ()
ivoryCatch exn m =
  do pm <- collectTransitions m
     addTransition $ mkCatchP exn pm (mkReturnP $ mkLiteral ())

-- | Add an assumption to the current transition relation
ivoryAssume :: ILProp -> I2LM ()
ivoryAssume prop = addTransition $ mkAssumeP prop

-- | Add an assertion to the current transition relation
ivoryAssert :: ILProp -> I2LM ()
ivoryAssert prop = addTransition $ mkAssertP IvoryError prop

-- | The number of reserved / "special" global variable numbers
numReservedSyms = 2

-- | A pointer expression used to store function arguments and return values
funArgsPtr :: ILExpr Ptr
funArgsPtr = mkOp (Op_global_var 1)

-- | A pointer expression used to store loop counters
loopCounterPtr :: ILExpr Ptr
loopCounterPtr = mkOp (Op_global_var 2)

-- | Get all the global variable symbols in a module
moduleGlobalSyms :: I.Module -> Set.Set I.Sym
moduleGlobalSyms mod =
  Set.unions [Set.fromList $ map I.externSym $ I.modExterns mod,
              Set.fromList $ map I.importSym $ I.modImports mod,
              Set.fromList $ map I.areaSym $ I.public $ I.modAreas mod,
              Set.fromList $ map I.areaSym $ I.private $ I.modAreas mod,
              Set.fromList $ map I.aiSym $ I.modAreaImports mod]

-- An I2LM computation can be "run" by giving it Ivory modules and a
-- default continuation (which is normally just "return")
instance RunM I2LM a (ILOpts -> [I.Module] -> (a -> ILPM) -> ILPM) where
  runM (I2LM m) opts mods k =
    let global_syms = Set.toList $ Set.unions $ map moduleGlobalSyms mods in
    let syms_map =
          Map.fromList $ zip global_syms [numReservedSyms + 1 ..]
    in
    runM m (I2LInfo { i2l_opts = opts,
                      i2l_modules = mods,
                      i2l_vars = Map.empty,
                      i2l_syms = syms_map })
    (return . k . fst)


----------------------------------------------------------------------
-- Handling Ivory variables
----------------------------------------------------------------------

-- | Look up the logical variable associated with an Ivory variable
lookupVar :: I.Var -> I2LM (Maybe SomeILName)
lookupVar v =
  do info <- get
     return $ Map.lookup v (i2l_vars info)

-- | Get the logical variable associated with an Ivory variable at a
-- specific type, raising an error if this does not work
getVar :: I1Type a -> I.Var -> I2LM (Name a)
getVar i1tp v =
  do maybe_some_name <- lookupVar v
     case maybe_some_name of
       Nothing -> error ("getVar: could not find Ivory variable " ++ show v)
       Just (SomeILName i1tp' n) ->
         case i1TypeEq i1tp i1tp' of
           Just Refl -> return n
           Nothing ->
             error ("getVar: Ivory variable " ++ show v ++
                    " has the wrong type; expected: " ++ show i1tp ++
                    "; found: " ++ show i1tp')

-- | Get the logical variable associated with an Ivory variable, at a specific
-- type, and convert it to a logical expression
getVarExpr :: I1Type a -> I.Var -> I2LM (ILExpr a)
getVarExpr i1tp var =
  liftM (mkVarExprTp $ LType_base $ ivory2L1Type i1tp) $ getVar i1tp var

-- | Bind a logical variable to the result of a 'PM' logical expression, and
-- return the bound name. This is done by building the expression @pm >>= k@,
-- where @k@ is the continuation of the rest of the expression being built.
bindName :: LType a -> ILExpr (PM a) -> I2LM (Name a)
bindName ltp e =
  shiftPureM $ \k -> mkOp (Op_bindP ltp ltypeRep) e $ LLambda ltp $ nu k

-- | Associate an Ivory variable name with a logic variable
linkVarAndName :: I.Var -> SomeILName -> I2LM ()
linkVarAndName var n =
  do info <- get
     set $ info { i2l_vars =
                    Map.insertWith
                    (\_ _ ->
                      error ("linkVarAndName: duplicate binding for variable "
                             ++ show var))
                    var n (i2l_vars info)
                }

-- | Bind an Ivory variable to the result of a computation @pm@, and
-- store this binding in the 'I2LInfo' transition-relation-building
-- state. The binding is done by building the expression @pm >>= k@,
-- where @k@ is the continuation of the rest of the expression being
-- built.
pmBindVar :: I.Var -> I1Type a -> ILExpr (PM a) -> I2LM ()
pmBindVar var i1tp pm =
  do n <- bindName (LType_base $ ivory2L1Type i1tp) pm
     linkVarAndName var (SomeILName i1tp n)

-- | Let-bind an Ivory variable to a value, and store this binding in
-- the 'I2LInfo' transition-relation-building state. The let-binding
-- is done by building the expression @(return x) >>= k@, where @k@ is
-- the continuation of the rest of the expression being built.
letBindVar :: I.Var -> I1Type a -> ILExpr a -> I2LM ()
letBindVar var i1tp (LVar (LTypeArgs_base _) n _) =
  -- Special case: if the RHS is already a variable, no need to bind a new one!
  linkVarAndName var (SomeILName i1tp n)
letBindVar var i1tp rhs =
  pmBindVar var i1tp (mkReturnP_tp (ivory2L1Type i1tp) rhs)

-- | Let-bind an Ivory variable to a value of unknown type (see 'letBindVar')
letBindVarSome :: I.Var -> SomeILExpr -> I2LM ()
letBindVarSome var (SomeILExpr i1tp e) = letBindVar var i1tp e

-- | Similar to 'letBindVar', except the given Ivory variable is
-- existentially-bound, meaning no explicit value is given for it.
exBindVar :: I.Var -> I1Type a -> I2LM ()
exBindVar var i1tp = pmBindVar var i1tp (mkExistsP_tp $ ivory2L1Type i1tp)

-- | Similar to 'exBindVar', but where the type of the variable is given as an
-- Ivory type, using the 'I.Typed' construction
exBindTypedVar :: I.Typed I.Var -> I2LM ()
exBindTypedVar typed_var =
  case convertType (I.tType typed_var) of
    SomeI1Type i1tp -> exBindVar (I.tValue typed_var) i1tp


----------------------------------------------------------------------
-- Handling floating-point round-off error
----------------------------------------------------------------------

-- | Generate a floating-point error function symbol given two parameters: the
-- relative error (aka the "machine epsilon") and the max non-infinite number
-- (FIXME: this does not correctly handle numbers very close to 0...)
mkFloatErrFun :: Rational -> Rational ->
                 I2LM (Name (Literal Rational -> Literal Rational))
mkFloatErrFun rel_err max_val =
  do err_fun <- bindName ltypeRep (mkExistsP_ftp l1funTypeRep)
     ivoryAssert $
       mkForall l1typeRep $ \r ->
       mkOr [mkLt (mkLiteral max_val) r,
             mkLt (mkLiteral (- max_val)) r,
             mkAnd [mkLe (mkLiteral (- rel_err)) (mkVar Proxy err_fun r),
                    mkLe (mkVar Proxy err_fun r) (mkLiteral rel_err)]]
     return err_fun

-- | Look up the round-off error function for the single-precision
-- floating-point type, creating it if necessary
getSingleErrFun :: I2LM (Name (Literal Rational -> Literal Rational))
getSingleErrFun =
  do s <- get
     case i2l_single_err s of
       Just err_fun -> return err_fun
       Nothing ->
         do err_fun <- mkFloatErrFun (2 ^^ (-24)) (2 ^^ 128)
            set $ s { i2l_single_err = Just err_fun }
            return err_fun

-- | Look up the round-off error function for the double-precision
-- floating-point type, creating it if necessary
getDoubleErrFun :: I2LM (Name (Literal Rational -> Literal Rational))
getDoubleErrFun =
  do s <- get
     case i2l_double_err s of
       Just err_fun -> return err_fun
       Nothing ->
         do err_fun <- mkFloatErrFun (2 ^^ (-53)) (2 ^^ 128)
            set $ s { i2l_double_err = Just err_fun }
            return err_fun

-- | Round an expression if its type requires it
roundIfNeeded :: I1Type a -> ILExpr a -> I2LM (ILExpr a)
roundIfNeeded ITyFloat e =
  do err_fun <- getSingleErrFun
     return $ e * (mkVar Proxy err_fun e)
roundIfNeeded ITyDouble e =
  do err_fun <- getDoubleErrFun
     return $ e * (mkVar Proxy err_fun e)
roundIfNeeded _ e = return e


----------------------------------------------------------------------
-- Coercion between (first-order) Ivory types
----------------------------------------------------------------------

-- | Coerce an Ivory logic expression from one Ivory type to another
ivoryCoerce :: I1Type f -> I1Type t -> ILExpr f -> I2LM (ILExpr t)
ivoryCoerce (ivory2L1Type -> L1Type_lit lit_tp_f) (ITyIndex bound) e =
  -- To coerce to an index type, first coerce to Int32 then take the mod
  return $
  mkArithOp2 litTypeRep Op2_Mod (mkCoerce lit_tp_f litTypeRep e)
  (convertInteger ITyBits bound)
ivoryCoerce ITyDouble ITyFloat e =
  -- Reducing floating-point precision causes extra rounding
  roundIfNeeded ITyFloat e
ivoryCoerce (ivory2L1Type -> L1Type_lit lit_tp_f)
  (ivory2L1Type -> L1Type_lit lit_tp_t) e =
  -- Default: use the underlying coercion in the reachability logic
  return $ mkCoerce lit_tp_f lit_tp_t e
ivoryCoerce i1tp_from i1tp_to _ =
  error ("ivoryCoerce: Could not coerce from type " ++ show i1tp_from
         ++ " to type " ++ show i1tp_to)

-- | Coerce an expression of unknown type to a given type
ivoryCoerceSome :: I1Type a -> SomeILExpr -> I2LM (ILExpr a)
ivoryCoerceSome i1tp_to (SomeILExpr i1tp_from e) =
  ivoryCoerce i1tp_from i1tp_to e


----------------------------------------------------------------------
-- Ivory state manipulation
----------------------------------------------------------------------

-- | Assert that a pointer and index pair is valid: check that the pointer is no
-- greater than the last-allocated pointer, and that the index is non-negative
-- and less than the length associated with the pointer. NOTE: negative pointers
-- are ok, as these are used to represent constant, global pointers.
assertPtrIndexOK :: ILExpr Ptr -> ILExpr (Literal Word64) -> I2LM ()
assertPtrIndexOK ptr ix =
  addTransition $
  mkBindP (mkOp (Op_readP ReadOp_last_alloc)) $ \last_alloc ->
  mkBindP (mkOp (Op_readP ReadOp_length) ptr) $ \len ->
  mkAssertP IvoryError $
  mkAnd [mkIsTrue (mkOp (Op_ptr_cmp OpCmp_LE) ptr last_alloc),
         mkIsTrue (mkOp (Op_cmp litTypeRep OpCmp_LT) ix len),
         mkIsTrue (mkOp (Op_cmp litTypeRep OpCmp_LE) (mkLiteral 0) ix)]

-- | Bind a name that is the result of an array read, of the given pointer at
-- the given index, of type 'Word64'. This is done by building the expression
-- @(readP read_op args) >>= k@ in the eventual result of the continuation,
-- where @k@ is the continuation of the rest of the expression being built.
readArrayNameWord64 :: ILExpr Ptr -> ILExpr (Literal Word64) ->
                       I2LM (Name (Literal Word64))
readArrayNameWord64 ptr ix =
  bindName ltypeRep $ mkOp (Op_readP (ReadOp_array Elem_base)) ptr ix

-- | Similar to 'readArrayNameWord64', but read a 'Rational' value instead
readArrayNameRational :: ILExpr Ptr -> ILExpr (Literal Word64) ->
                         I2LM (Name (Literal Rational))
readArrayNameRational ptr ix =
  bindName ltypeRep $
  mkOp (Op_readP (ReadOp_array $ Elem_cons Elem_base)) ptr ix

-- | Similar to 'readArrayNameWord64', but read a pointer value instead
readArrayNamePtr :: ILExpr Ptr -> ILExpr (Literal Word64) -> I2LM (Name Ptr)
readArrayNamePtr ptr ix =
  bindName ltypeRep $ mkOp (Op_readP ReadOp_ptr_array) ptr ix

-- | Read an array value, binding the result to a fresh local name and coercing
-- the name if necessary
readArray :: I1Type a -> ILExpr Ptr -> ILExpr (Literal Word64) ->
             I2LM (ILExpr a)
readArray ITyVoid ptr ix = return $ mkLiteral ()
readArray itp@ITyBits ptr ix =
  do n <- readArrayNameWord64 ptr ix
     ivoryCoerce ITyBits itp $ mkVar Proxy n
readArray itp@(ITyIndex _) ptr ix =
  do n <- readArrayNameWord64 ptr ix
     ivoryCoerce ITyBits itp $ mkVar Proxy n
readArray itp@ITyBool ptr ix =
  do n <- readArrayNameWord64 ptr ix
     ivoryCoerce ITyBits itp $ mkVar Proxy n
readArray ITyChar ptr ix =
  error "readArray: type Char not yet supported"
readArray ITyFloat ptr ix =
  do n <- readArrayNameRational ptr ix
     return $ mkVar Proxy n
readArray ITyDouble ptr ix =
  do n <- readArrayNameRational ptr ix
     return $ mkVar Proxy n
readArray (ITyPtr _ _) ptr ix =
  do n <- readArrayNamePtr ptr ix
     return $ mkVar Proxy n
readArray (ITyArrayPtr _ _ _) ptr ix =
  do n <- readArrayNamePtr ptr ix
     return $ mkVar Proxy n
readArray (ITyStructPtr _ _) ptr ix =
  do n <- readArrayNamePtr ptr ix
     return $ mkVar Proxy n
readArray (ITyCArrayPtr _ _) ptr ix =
  do n <- readArrayNamePtr ptr ix
     return $ mkVar Proxy n
readArray ITyOpaque ptr ix = return $ mkLiteral ()

-- | Perform a 'readArray' using a flattened Ivory type
readArraySome :: I.Type -> ILExpr Ptr -> ILExpr (Literal Word64) ->
                 I2LM SomeILExpr
readArraySome itp ptr ix =
  case convertType itp of
    SomeI1Type i1tp -> SomeILExpr i1tp <$> readArray i1tp ptr ix

-- | Update an Ivory 64-bit word array value
updateArrayWord64 :: ILExpr Ptr -> ILExpr (Literal Word64) ->
                     ILExpr (Literal Word64) -> I2LM ()
updateArrayWord64 ptr ix v =
  addTransition $ mkOp (Op_updateP $ UpdateOp_array Elem_base) ptr ix v

-- | Update an Ivory 'Rational' array value
updateArrayRational :: ILExpr Ptr -> ILExpr (Literal Word64) ->
                       ILExpr (Literal Rational) -> I2LM ()
updateArrayRational ptr ix v =
  addTransition $
  mkOp (Op_updateP $ UpdateOp_array $ Elem_cons Elem_base) ptr ix v

-- | Update an Ivory 'Ptr' array value
updateArrayPtr :: ILExpr Ptr -> ILExpr (Literal Word64) -> ILExpr Ptr -> I2LM ()
updateArrayPtr ptr ix v =
  addTransition $ mkOp (Op_updateP UpdateOp_ptr_array) ptr ix v

-- | Update an Ivory array value, coercing the written value if necessary
updateArray :: I1Type a -> ILExpr Ptr -> ILExpr (Literal Word64) -> ILExpr a ->
               I2LM ()
updateArray ITyVoid _ _ _ =
  -- Writing a void object is just a no-op
  return ()
updateArray itp@ITyBits ptr ix v =
  updateArrayWord64 ptr ix =<< ivoryCoerce itp ITyBits v
updateArray itp@(ITyIndex _) ptr ix v =
  updateArrayWord64 ptr ix =<< ivoryCoerce itp ITyBits v
updateArray itp@ITyBool ptr ix v =
  updateArrayWord64 ptr ix =<< ivoryCoerce itp ITyBits v
updateArray ITyChar ptr ix v =
  error "updateArray: Char type not yet handled"
updateArray ITyFloat ptr ix v = updateArrayRational ptr ix v
updateArray ITyDouble ptr ix v = updateArrayRational ptr ix v
updateArray (ITyPtr _ _) ptr ix v = updateArrayPtr ptr ix v
updateArray (ITyArrayPtr _ _ _) ptr ix v = updateArrayPtr ptr ix v
updateArray (ITyStructPtr _ _) ptr ix v = updateArrayPtr ptr ix v
updateArray (ITyCArrayPtr _ _) ptr ix v = updateArrayPtr ptr ix v
updateArray ITyOpaque ptr ix v = return ()

-- | Perform an 'updateArray' with a value of an unknown type
updateArraySome :: ILExpr Ptr -> ILExpr (Literal Word64) -> SomeILExpr ->
                   I2LM ()
updateArraySome ptr ix (SomeILExpr i1tp v) = updateArray i1tp ptr ix v

-- | Allocate an Ivory array of a given length, returning a pointer to it
allocateArray :: ILExpr (Literal Word64) -> I2LM (ILExpr Ptr)
allocateArray len =
  addBindTransition $
  mkSeqP (mkOp (Op_updateP UpdateOp_alloc) len) $
  mkOp (Op_readP ReadOp_last_alloc)

-- | Let-bind the special Ivory variable "retval" to the result of a function
-- call, by reading this result from the current memory
bindRetval :: I1Type a -> I2LM ()
bindRetval ITyVoid =
  -- Special case: don't bind a "void"-typed variable
  return ()
bindRetval i1tp =
  do ret <- readArray i1tp funArgsPtr (mkLiteral 0)
     letBindVar I.retval i1tp ret


----------------------------------------------------------------------
-- Looking up Ivory information from Ivory modules
----------------------------------------------------------------------

-- | Look up a named value in a list of modules or signal an error
moduleLookup :: (Show key, Eq key) => (I.Module -> I.Visible res) ->
                (res -> key) -> key -> String -> [I.Module] -> res
moduleLookup readMod readKey key res_name mods =
  let elems =
        concatMap (I.public . readMod) mods ++
        concatMap (I.private . readMod) mods in
  case find ((==) key . readKey) elems of
    Just elem -> elem
    Nothing ->
      error $ "moduleLookup: " ++ res_name ++ " " ++ show key ++ " not found!"

-- | Look up a 'Struct' by name
lookupStruct :: String -> [I.Module] -> I.Struct
lookupStruct s_name mods =
  moduleLookup I.modStructs struct_name s_name "struct" mods
  where
    struct_name (I.Struct s_name' _) = s_name'
    struct_name (I.Abstract s_name' _) = s_name'

-- | Do a 'lookupStruct' inside the 'I2LM' monad
lookupStructM :: String -> I2LM I.Struct
lookupStructM s_name = lookupStruct s_name <$> i2l_modules <$> get

-- | Get the type and the index number of a field in a given struct
lookupStructField :: String -> String -> [I.Module] -> (I.Type, Int)
lookupStructField s_name f_name mods =
  helper f_name $ lookupStruct s_name mods
  where
    helper f_name (I.Struct _ fields) =
      find_helper f_name 0 fields
    helper _ (I.Abstract _ _) =
      error "lookupStructField: abstract struct!"
    find_helper f_name _ [] =
      error ("lookupStructField: could not find field " ++ f_name)
    find_helper f_name i (fld:_) | I.tValue fld == f_name = (I.tType fld, i)
    find_helper f_name i (_:flds) = find_helper f_name (i+1) flds

-- | Do a 'lookupStructField' inside the 'I2LM' monad
lookupStructFieldM :: String -> String -> I2LM (I.Type, Int)
lookupStructFieldM s_name f_name =
  lookupStructField s_name f_name <$> i2l_modules <$> get

-- | Look up the number of fields in a struct
lookupStructLengthM :: String -> I2LM Int
lookupStructLengthM s_name =
  do s <- lookupStructM s_name
     case s of
       I.Struct _ fields -> return $ length fields
       _ -> error "lookupStructLengthM: abstract struct!"

-- | Look up an Ivory function
lookupProc :: I.Sym -> [I.Module] -> I.Proc
lookupProc sym mods =
  moduleLookup I.modProcs I.procSym sym "function" mods

-- | Do a 'lookupProc' inside the 'I2LM' monad
lookupProcM :: I.Sym -> I2LM I.Proc
lookupProcM sym = lookupProc sym <$> i2l_modules <$> get


----------------------------------------------------------------------
-- Converting Ivory expressions into Ivory reachability logic
----------------------------------------------------------------------

-- | An Ivory expression that has been converted into an Ivory reachability
-- logic expression of some unknown first-order type
data SomeILExpr where
  SomeILExpr :: I1Type a -> ILExpr a -> SomeILExpr

-- | Convert a symbol to a pointer
convertSymbol :: I.Sym -> I2LM (ILExpr Ptr)
convertSymbol sym =
  do info <- get
     case Map.lookup sym (i2l_syms info) of
       Just i -> return $ mkOp (Op_global_var i)
       Nothing ->
         error $ "convertSymbol: unexpected symbol: " ++ sym
         {-
         do let i = i2l_next_sym_int info
            set $ info { i2l_syms =
                           Map.insertWith
                           (\_ _ ->
                             error ("convertSymbol: duplicate binding for symbol "
                                    ++ show sym))
                           sym
                           i
                           (i2l_syms info)
                       , i2l_next_sym_int = i+1 }
            return $ mkOp (Op_global_var (-i))
          -}

-- | Convert an 'Integer' to a logical expression of a given type. We assume
-- that, for floating-point types, integers can be represented exactly.
convertInteger :: I1Type a -> Integer -> ILExpr a
convertInteger ITyBits i = mkLiteralTp LitType_bits $ fromInteger i
convertInteger (ITyIndex bound) i = mkLiteral $ fromInteger $ mod i bound
convertInteger ITyFloat i = mkLiteral $ fromInteger i
convertInteger ITyDouble i = mkLiteral $ fromInteger i
convertInteger i1tp _ =
  error ("convertInteger: unsupported type: " ++ show i1tp)

-- | Convert an Ivory literal to a logical expression. We assume that, for
-- floating-point types, literal values can be represented exactly.
convertLiteral :: I1Type a -> I.Literal -> ILExpr a
convertLiteral i1tp (I.LitInteger i) = convertInteger i1tp i
convertLiteral ITyFloat (I.LitFloat x) = mkLiteral $ toRational x
convertLiteral ITyDouble (I.LitDouble x) = mkLiteral $ toRational x
convertLiteral _ (I.LitChar c) =
  error "convertLiteral: characters not (yet) supported"
convertLiteral ITyBool (I.LitBool b) = mkLiteral b
convertLiteral (ivory2L1Type -> L1Type_ptr) I.LitNull = mkOp Op_null_ptr
convertLiteral _ (I.LitString str) =
  error "convertLiteral: strings not (yet) supported"
convertLiteral _ _ =
  error "convertLiteral: literal used at incorrect type"

-- | Convert an expressions an apply an 'ArithOp1' to it
convertArithOp1 :: I1Type a -> ArithOp1 -> I.Expr -> I2LM (ILExpr a)
convertArithOp1 i1tp@(ivory2L1Type -> L1Type_lit lit_tp) aop ie =
  do e <- convertExpr i1tp ie
     roundIfNeeded i1tp $ mkOp (Op_arith1 lit_tp aop) e
convertArithOp1 i1tp _ _ =
  error ("convertArithOp1: arithmetic on unsupported type: " ++ show i1tp)

-- | Convert two expressions an apply an 'ArithOp2' to them
convertArithOp2 :: I1Type a -> ArithOp2 -> I.Expr -> I.Expr -> I2LM (ILExpr a)
convertArithOp2 i1tp@(ivory2L1Type -> L1Type_lit lit_tp) aop ie1 ie2 =
  do e1 <- convertExpr i1tp ie1
     e2 <- convertExpr i1tp ie2
     roundIfNeeded i1tp $ mkOp (Op_arith2 lit_tp aop) e1 e2
convertArithOp2 i1tp _ _ _ =
  error ("convertArithOp2: arithmetic on unsupported type: " ++ show i1tp)

-- | Convert two expressions an apply an 'ArithCmp' to them
convertArithCmp :: I.Type -> ArithCmp -> I.Expr -> I.Expr ->
                   I2LM (ILExpr (Literal Bool))
convertArithCmp (convertType -> SomeI1Type i1tp) acmp ie1 ie2 =
  do e1 <- convertExpr i1tp ie1
     e2 <- convertExpr i1tp ie2
     return $ mkArithCmpTp (ivory2L1Type i1tp) acmp e1 e2

-- | Convert an Ivory expression to a logical expression using an Ivory type
convertSomeExpr :: I.Type -> I.Expr -> I2LM SomeILExpr
convertSomeExpr itp ie =
  case convertType itp of
    SomeI1Type i1tp -> SomeILExpr i1tp <$> convertExpr i1tp ie

-- | Convert a typed Ivory expression to a logical expression
convertTypedExpr :: I.Typed I.Expr -> I2LM SomeILExpr
convertTypedExpr typed_ie =
  convertSomeExpr (I.tType typed_ie) (I.tValue typed_ie)

-- | Convert an Ivory expression to a logical expression of a specific type
convertExpr :: I1Type a -> I.Expr -> I2LM (ILExpr a)

-- Symbols and variables
convertExpr i1tp (I.ExpSym sym) =
  do ptr_expr <- convertSymbol sym
     readArray i1tp ptr_expr (mkLiteral 0)
convertExpr i1tp (I.ExpExtern ext) =
  do ptr_expr <- convertSymbol (I.externSym ext)
     readArray i1tp ptr_expr (mkLiteral 0)
convertExpr i1tp (I.ExpVar v) = getVarExpr i1tp v

-- Literals
convertExpr i1tp (I.ExpLit lit) =
  return $ convertLiteral i1tp lit

-- Array and struct dereferencing
convertExpr i1tp (I.ExpLabel (I.TyStruct s_name) s_iexpr f_name) =
  do s_expr <- convertExpr (ITyStructPtr Nothing s_name) s_iexpr
     (_, f_ix) <- lookupStructFieldM s_name f_name
     assertPtrIndexOK s_expr (mkLiteral64 f_ix)
     readArray i1tp s_expr (mkLiteral64 f_ix)
convertExpr i1tp (I.ExpIndex arr_itp arr_iexpr ix_itp ix_iexpr) =
  do let arr_i1tp =
           case convertType arr_itp of
             -- FIXME: test that the element type == i1tp
             SomeI1Type arr_i1tp@(ITyArrayPtr Nothing _ _) -> arr_i1tp
             SomeI1Type arr_i1tp@(ITyCArrayPtr Nothing _) -> arr_i1tp
             _ -> error ("convertExpr: array indexing at non-array type!")
     arr_expr <- convertExpr arr_i1tp arr_iexpr
     ix_expr <- ivoryCoerceSome ITyBits =<< convertSomeExpr ix_itp ix_iexpr
     assertPtrIndexOK arr_expr ix_expr
     readArray i1tp arr_expr ix_expr

-- Coercion between types
convertExpr i1tp (I.ExpToIx iexpr bound) =
  -- README: Ivory sometimes seems to automatically convert index types back to
  -- 32-bit signed integer types, so we generalize this and allow any upcast
  -- here
  ivoryCoerce (ITyIndex bound) i1tp =<< convertExpr (ITyIndex bound) iexpr
convertExpr i1tp (I.ExpSafeCast from_itp iexpr) =
  ivoryCoerceSome i1tp =<< convertSomeExpr from_itp iexpr

-- Comparison operations
convertExpr ITyBool (I.ExpOp (I.ExpEq itp) [ie1, ie2]) =
  convertArithCmp itp OpCmp_EQ ie1 ie2
convertExpr ITyBool (I.ExpOp (I.ExpNeq itp) [ie1, ie2]) =
  liftM mkNotBool $ convertArithCmp itp OpCmp_EQ ie1 ie2
convertExpr ITyBool (I.ExpOp (I.ExpLt leq_flag itp) [ie1, ie2]) =
  convertArithCmp itp (if leq_flag then OpCmp_LE else OpCmp_LT) ie1 ie2
convertExpr ITyBool (I.ExpOp (I.ExpGt leq_flag itp) [ie1, ie2]) =
  convertArithCmp itp (if leq_flag then OpCmp_LE else OpCmp_LT) ie2 ie1

-- Boolean operations
convertExpr i1tp@ITyBool (I.ExpOp I.ExpNot [ie]) =
  liftM mkNotBool $ convertExpr i1tp ie
convertExpr i1tp@ITyBool (I.ExpOp I.ExpAnd [ie1,ie2]) =
  liftM2 mkAndBool (convertExpr i1tp ie1) (convertExpr i1tp ie2)
convertExpr i1tp@ITyBool (I.ExpOp I.ExpOr [ie1,ie2]) =
  liftM2 mkOrBool (convertExpr i1tp ie1) (convertExpr i1tp ie2)
convertExpr i1tp (I.ExpOp I.ExpCond [ie1,ie2,ie3]) =
  do e1 <- convertExpr ITyBool ie1
     e2 <- convertExpr i1tp ie2
     e3 <- convertExpr i1tp ie3
     return $ mkCond (ivory2L1Type i1tp) e1 e2 e3

-- Arithmetic operations
convertExpr i1tp (I.ExpOp I.ExpMul [ie1,ie2]) =
  convertArithOp2 i1tp Op2_Mult ie1 ie2
convertExpr i1tp (I.ExpOp I.ExpAdd [ie1,ie2]) =
  convertArithOp2 i1tp Op2_Add ie1 ie2
convertExpr i1tp (I.ExpOp I.ExpSub [ie1,ie2]) =
  convertArithOp2 i1tp Op2_Sub ie1 ie2
convertExpr i1tp (I.ExpOp I.ExpNegate [ie1]) =
  convertArithOp1 i1tp Op1_Neg ie1
convertExpr i1tp (I.ExpOp I.ExpAbs [ie1]) =
  convertArithOp1 i1tp Op1_Abs ie1
convertExpr i1tp (I.ExpOp I.ExpSignum [ie1]) =
  convertArithOp1 i1tp Op1_Signum ie1
convertExpr i1tp (I.ExpOp I.ExpDiv [ie1,ie2]) =
  convertArithOp2 i1tp Op2_Div ie1 ie2
convertExpr i1tp (I.ExpOp I.ExpMod [ie1,ie2]) =
  convertArithOp2 i1tp Op2_Mod ie1 ie2
convertExpr i1tp@(ivory2L1Type -> L1Type_lit lit_tp) (I.ExpOp I.ExpRecip [ie1]) =
  do e1 <- convertExpr i1tp ie1
     roundIfNeeded i1tp $ mkArithOp2 lit_tp Op2_Div (convertInteger i1tp 1) e1

-- Floating-point operations (FIXME!)

-- Bitwise operations
convertExpr i1tp (I.ExpOp I.ExpBitAnd [ie1,ie2]) =
  convertArithOp2 i1tp Op2_BitAnd ie1 ie2
convertExpr i1tp (I.ExpOp I.ExpBitOr [ie1,ie2]) =
  convertArithOp2 i1tp Op2_BitOr ie1 ie2
convertExpr i1tp (I.ExpOp I.ExpBitXor [ie1,ie2]) =
  convertArithOp2 i1tp Op2_BitXor ie1 ie2
convertExpr i1tp (I.ExpOp I.ExpBitComplement [ie1]) =
  convertArithOp1 i1tp Op1_Complement ie1

-- FIXME: bit-shifting operations!

-- Getting the address of a global
convertExpr (ivory2L1Type -> L1Type_ptr) (I.ExpAddrOfGlobal sym) =
  -- FIXME: should the type matter...?
  convertSymbol sym

-- Max/min of a type, and sizeof a type
convertExpr i1tp (I.ExpMaxMin is_max) =
  -- FIXME HERE: implement this!
  error "convertExpr: max and min not yet implemented!"
convertExpr i1tp (I.ExpSizeOf itp) =
  -- FIXME HERE: implement this!
  error "convertExpr: sizeof not yet implemented!"
convertExpr i1tp e =
  error ("convertExpr: could not convert expression: " ++ show e)


----------------------------------------------------------------------
-- Converting Ivory initialization expressions into logic
----------------------------------------------------------------------

-- | Convert an Ivory initializer into a logical expression. Struct and array
-- initializers become pointers.
convertInit :: I1Type a -> I.Init -> I2LM (ILExpr a)
convertInit i1tp I.InitZero =
  return $
  case ivory2L1Type i1tp of
    L1Type_lit lit_tp -> mkLiteralTp lit_tp (litDefaultTp lit_tp)
    L1Type_ptr -> mkOp Op_null_ptr
    L1Type_prop -> error "convertInit: local var of type Prop!"
convertInit i1tp (I.InitExpr itp' ie) =
  case convertType itp' of
    SomeI1Type (i1TypeEq i1tp -> Just Refl) ->
      convertExpr i1tp ie
    _ ->
      error "convertInit: init expression type did not match expected type!"
convertInit (ITyStructPtr Nothing s_name) (I.InitStruct init_flds) =
  do len <- lookupStructLengthM s_name
     ptr <- allocateArray $ mkLiteral64 len
     forM_ init_flds
       (\(f_name,init) ->
         do (f_itp, f_ix) <- lookupStructFieldM s_name f_name
            case convertType f_itp of
              SomeI1Type f_i1tp ->
                do e <- convertInit f_i1tp init
                   updateArray f_i1tp ptr (mkLiteral64 f_ix) e)
     return ptr
convertInit _ (I.InitStruct _) =
  error "convertInit: struct initialization for non-struct type!"
convertInit arr_i1tp (I.InitArray init_elems) =
  case arr_i1tp of
    ITyArrayPtr Nothing len elem_i1tp ->
      convertArrayInit len elem_i1tp init_elems
    ITyCArrayPtr Nothing elem_i1tp ->
      convertArrayInit (length init_elems) elem_i1tp init_elems
    _ -> error "convertInit: array initialization for non-array type!"
  where
    convertArrayInit :: Int -> I1Type a -> [I.Init] -> I2LM (ILExpr Ptr)
    convertArrayInit len i1tp init_elems =
      do elems <- mapM (convertInit i1tp) init_elems
         ptr <- allocateArray $ mkLiteral64 (1 + len)
         forM_ (zip elems [1 .. len])
           (\(e, ix) -> updateArray i1tp ptr (mkLiteral64 ix) e)
         return ptr


----------------------------------------------------------------------
-- Handling Ivory Requires and Ensures Conditions
----------------------------------------------------------------------

-- | Convert an Ivory condition into a proposition
convertCond :: I.Cond -> I2LM (ILExpr Prop)
convertCond (I.CondBool ie) =
  do e <- convertExpr ITyBool ie
     return $ mkIsTrue e
convertCond (I.CondDeref (convertType -> SomeI1Type i1tp) ie_ptr var cond) =
  do ptr <- convertExpr (mkRefType i1tp) ie_ptr
     ptr_val <- readArray i1tp ptr (mkLiteral 0)
     assertPtrIndexOK ptr (mkLiteral 0)
     letBindVar var i1tp ptr_val
     convertCond cond

-- | Convert a 'Require' clause into a proposition
convertRequire :: I.Require -> I2LM (ILExpr Prop)
convertRequire req = convertCond $ I.getRequire req

-- | Convert an 'Ensure' clause into a proposition
convertEnsure :: I.Ensure -> I2LM (ILExpr Prop)
convertEnsure req = convertCond $ I.getEnsure req


----------------------------------------------------------------------
-- Converting Ivory statements into Ivory reachability logic
----------------------------------------------------------------------

-- | Convert a block of Ivory statements to a transition relation and add it to
-- the current transition relation
convertBlock :: I.Block -> I2LM ()
convertBlock = mapM_ convertStmt

-- | Convert an Ivory statement to a transition relation and add it to the
-- current transition relation
convertStmt :: I.Stmt -> I2LM ()

-- If-then-else statements --> disjunctive transition relations
convertStmt (I.IfTE ie stmts1 stmts2) =
  do e <- convertExpr ITyBool ie
     let prop = mkIsTrue e
     pm1 <- collectTransitions $ convertBlock stmts1
     pm2 <- collectTransitions $ convertBlock stmts2
     addTransition $
       mkOrP (mkSeqP (mkAssumeP prop) pm1)
       (mkSeqP (mkAssumeP (mkNot prop)) pm2)

-- Assertions --> errors if the assertion does not hold; i.e., they
-- become @if prop then ('returnP' ()) else raise 'IvoryError'@
convertStmt (I.Assert ie) =
  do e <- convertExpr ITyBool ie
     ivoryAssert (mkIsTrue e)

-- Compiler-inserted assertions, which are the same as normal assertions
convertStmt (I.CompilerAssert ie) =
  get >>= \info ->
  if skipCompilerAsserts (i2l_opts info) then
    return ()
  else
    do e <- convertExpr ITyBool ie
       ivoryAssert (mkIsTrue e)

-- Assumes --> assumptions in the Ivory reachability logic
convertStmt (I.Assume ie) =
  do e <- convertExpr ITyBool ie
     ivoryAssume (mkIsTrue e)

-- Return statements --> write the returned value to the returnValue
-- global variable and then raise an 'IvoryReturn' exception
convertStmt (I.Return typed_ie) =
  do e <- convertTypedExpr typed_ie
     updateArraySome funArgsPtr (mkLiteral 0) e
     addTransition $ mkRaiseP IvoryReturn

-- Return statements with no return value --> just raise 'IvoryReturn'
convertStmt I.ReturnVoid = addTransition $ mkRaiseP IvoryReturn

-- Dereference statements --> read a pointer and bind the result to a variable
convertStmt (I.Deref (convertType -> SomeI1Type i1tp) var ie) =
  do ptr_expr <- convertExpr (mkRefType i1tp) ie
     assertPtrIndexOK ptr_expr (mkLiteral 0)
     res_expr <- readArray i1tp ptr_expr (mkLiteral 0)
     letBindVar var i1tp res_expr

-- Assignment statements
convertStmt (I.Store (convertType -> SomeI1Type i1tp) ilhs irhs) =
  do ptr <- convertExpr (mkRefType i1tp) ilhs
     val <- convertExpr i1tp irhs
     updateArray i1tp ptr (mkLiteral 0) val

-- Assignment statements
convertStmt (I.Assign (convertType -> SomeI1Type i1tp) var irhs) =
  do val <- convertExpr i1tp irhs
     letBindVar var i1tp val

-- Function calls
convertStmt (I.Call (convertType -> SomeI1Type ret_i1tp) maybe_ret_var
             (I.NameSym fn_sym) iargs) =
  do p <- lookupProcM fn_sym
     -- We wrap the actual call in a reset, since it binds local variables (in
     -- order to assert the pre-/post-conditions)
     pm <- collectTransitions $
       do
         -- Step 1: Convert the arguments
         args <- mapM convertTypedExpr iargs
         -- Step 2: Let-bind the arguments
         mapM_ (uncurry letBindVarSome) (zip (map I.tValue $ I.procArgs p) args)
         -- Step 3: Assert the preconditions
         mapM_ (ivoryAssert <=< convertRequire) (I.procRequires p)
         -- Step 4: Inline the call or insert an arbitrary transition
         inline_p <- inlineCalls <$> i2l_opts <$> get
         if inline_p then convertBlock (I.procBody p) else
           -- FIXME HERE NOW: add a call here to havoc memory
           error "convertStmt: non-inlined function calls not yet supported"
         -- Step 5: Bind the special variable "retval" to the output
         bindRetval ret_i1tp
         -- Step 6: Assume the postconditions
         mapM_ (ivoryAssert <=< convertEnsure) (I.procEnsures p)
     -- Now add the call as a transition outside the reset
     addTransition pm
     -- Finally, bind the return variable to the return value, if necessary
     case maybe_ret_var of
       Just ret_var ->
         do ret_val <- readArray ret_i1tp funArgsPtr (mkLiteral 0)
            letBindVar ret_var ret_i1tp ret_val
       Nothing -> return ()

-- Indirect function calls: not handled yet!
convertStmt (I.Call (convertType -> SomeI1Type ret_i1tp) maybe_ret_var
             (I.NameVar fn_var) iargs) =
  error "convertStmt: indirect function calls not (yet) handled"

-- Local variable decl --> create an lvalue and return its address
convertStmt (I.Local (convertType -> SomeI1Type i1tp) var init) =
  do ptr <-
       case i1tp of
         i1tp@(ITyStructPtr Nothing _) ->
           -- convertInit already promotes struct values to pointers, so we just
           -- return the value created by convertInit
           convertInit i1tp init
         i1tp@(ITyArrayPtr Nothing _ _) ->
           -- Similarly with array values
           convertInit i1tp init
         i1tp@(ITyCArrayPtr Nothing _) ->
           -- Similarly with C array values
           convertInit i1tp init
         _ ->
           -- Otherwise, we allocate a reference for the value returned by
           -- convertInit, and store it there
           do e <- convertInit i1tp init
              ptr <- allocateArray (mkLiteral 1)
              updateArray i1tp ptr (mkLiteral 0) e
              return ptr
     -- Now we let-bind var to ptr
     letBindVar var (mkRefType i1tp) ptr

-- Reference copy: do a shallow copy of the array pointed to by irhs to ilhs
convertStmt (I.RefCopy (convertType -> SomeI1Type i1tp) ilhs irhs) =
  do ptr_rhs <- convertExpr (mkRefType i1tp) irhs
     ptr_lhs <- convertExpr (mkRefType i1tp) ilhs
     addTransition $ mkOp (Op_updateP UpdateOp_ptr_copy) ptr_rhs ptr_lhs

-- Reference allocation: this is essentially the identity in the logic (its
-- actual use in Ivory is to convert C lvalues into C rvalues)
convertStmt (I.AllocRef (convertType -> SomeI1Type i1tp) var nm) =
  do ptr <-
       case nm of
         I.NameVar var' -> getVarExpr (mkRefType i1tp) var'
         I.NameSym sym -> convertSymbol sym
     letBindVar var (mkRefType i1tp) ptr

-- Loops
convertStmt (I.Loop max_iters var istart incr body) =
  do let i1tp = ITyIndex max_iters
     -- Convert the start value and store it in the global loop variable
     start <- convertExpr i1tp istart
     updateArray i1tp loopCounterPtr (mkLiteral 0) start
     -- Extract the loop bound and whether the loop is an increment loop
     let (ibound, loop_is_incr_p) =
           case incr of
             I.IncrTo ibound -> (ibound, True)
             I.DecrTo ibound -> (ibound, False)
     -- Convert the loop bound into a logical expression
     loop_bound <- convertExpr i1tp ibound
     -- Convert body (inside a reset, as it binds vars, raises exns, etc.)
     body_pm <- collectTransitions $ do
       -- Read the loop counter into var
       ctr_val <- readArray i1tp loopCounterPtr (mkLiteral 0)
       letBindVar var i1tp ctr_val
       -- Run the loop test, raising IvoryBreak if it fails
       let loop_test =
             mkIsTrue $
             if loop_is_incr_p then mkLtBool ctr_val loop_bound
             else mkLtBool loop_bound ctr_val
       addTransition $
         mkOrP (mkSeqP (mkAssumeP loop_test) (mkReturnP $ mkLiteral ()))
         (mkSeqP (mkAssumeP (mkNot loop_test)) (mkRaiseP IvoryBreak))
       -- Run the body
       convertBlock body
       -- Compute the new counter value (NOTE: we don't re-read from
       -- loopCounterPtr in case a sub-computation contains a loop)
       let upd_op = if loop_is_incr_p then Op1_Incr else Op1_Decr
       let ctr_val' = mkOp (Op_arith1 litTypeRep upd_op) ctr_val
       updateArray i1tp loopCounterPtr (mkLiteral 0) ctr_val'

     -- Read the options to get the loop unrolling maximum
     unroll_max <- loopUnrollingMax <$> i2l_opts <$> get
     if max_iters <= unroll_max then
       -- Unrolling loops = replicate body_pm max_iters times, catching any
       -- IvoryBreak exceptions if the loop ends early
       ivoryCatch IvoryBreak (replicateM_ (fromInteger max_iters) $
                              addTransition body_pm)
       else error ("convertStmt: loop unrolling bound exceeded; "
                   ++ "non-unrolled loops not yet handled!") -- FIXME HERE

-- Forever Loops --> not handled yet
convertStmt (I.Forever body) =
  error "convertStmt: forever loops not yet handled!" -- FIXME HERE

-- Break --> raise an IvoryBreak exception
convertStmt I.Break = addTransition $ mkRaiseP IvoryBreak

-- Comment --> no-op
convertStmt (I.Comment _) = return ()


----------------------------------------------------------------------
-- Converting Ivory procedures into Ivory reachability logic
----------------------------------------------------------------------

-- | Convert an Ivory procedure into a transition relation for an arbitrary call
-- to that procedure. This reads all the arguments from funArgsPtr (the global
-- variable with index 1), assumes the "require" propositions, and asserts all
-- the "ensures" after the body has executed.
convertProc :: I.Proc -> I2LM ILPM
convertProc p =
  collectTransitions $
  do
    -- Print the proc if debugging is on
    debug_level <- debugLevel <$> i2l_opts <$> get
    if debug_level >= 3 then
      traceM ("\nConverting proc " ++ show (I.procSym p)
              ++ "\nRequires:\n" ++ show (I.procRequires p)
              ++ "\nEnsures:\n" ++ show (I.procEnsures p)
              ++ "\nBody:\n" ++ show (I.procBody p)) else return ()
    -- Assume that the length of funArgsPtr is as big as needed
    args_len <- addBindTransition $ mkOp (Op_readP ReadOp_length) funArgsPtr
    ivoryAssume $ mkIsTrue $
      mkLeBool (mkLiteral64 $ length $ I.procArgs p) args_len
    -- Read each variable from funArgsPtr
    forM_ (zip (I.procArgs p) [0 ..]) $ \(tv, ix) ->
      do arg_val <- readArraySome (I.tType tv) funArgsPtr (mkLiteral ix)
         letBindVarSome (I.tValue tv) arg_val
    -- Assume all the Requires
    mapM_ (ivoryAssume <=< convertRequire) (I.procRequires p)
    -- Transition according to the body of the procedure, catching any returns
    ivoryCatch IvoryReturn (convertBlock (I.procBody p))
    -- Bind the return value to the special Ivory "retval" variable
    case convertType (I.procRetTy p) of
      SomeI1Type i1tp -> bindRetval i1tp
    -- Finally, assert all the Ensures
    mapM_ (ivoryAssert <=< convertEnsure) $ I.procEnsures p


----------------------------------------------------------------------
-- Testing if an error is reachable in an Ivory procedure
----------------------------------------------------------------------

-- | Model-check an Ivory procedure, testing if an assertion failure can be
-- reached from some input state
modelCheckProc :: SMTSolver solver => solver -> ILOpts -> [I.Module] ->
                  I.Proc -> IO (SMTResult IvoryMemory)
modelCheckProc solver opts mods p =
  exn_reachable (smtSetDebugLevel (debugLevel opts) solver) IvoryError $
  runM (convertProc p) opts mods id


----------------------------------------------------------------------
-- Pretty-printing a memory as the input to a given procedure
----------------------------------------------------------------------

data IvoryMemPPInfo =
  IvoryMemPPInfo
  {
    ivoryPP_mem :: IvoryMemory,
    -- ^ The 'IvoryMemory' being printed
    ivoryPP_seen_ptrs :: [Ptr],
    -- ^ A list of all the pointers that have already been printed, to avoid
    -- circular printing loops
    ivoryPP_mods :: [I.Module]
    -- ^ The Ivory modules in scope
  }

-- | A state monad for pretty-printing 'IvoryMemory's
newtype IvoryMemPP a = IvoryMemPP { runIvoryMemPP :: State IvoryMemPPInfo a }
                       deriving (Functor, Applicative, Monad)

instance StateM IvoryMemPP IvoryMemPPInfo where
  get = IvoryMemPP get
  set = IvoryMemPP . set

instance RunM IvoryMemPP a (IvoryMemory -> [I.Module] -> a) where
  runM (IvoryMemPP m) mem mods =
    fst $ runState (IvoryMemPPInfo
                    { ivoryPP_mem = mem, ivoryPP_seen_ptrs = [],
                      ivoryPP_mods = mods }) m

-- | Read a 'Word64' from the 'IvoryMemory' being printed
ppReadMem64 :: Ptr -> Word64 -> IvoryMemPP Word64
ppReadMem64 ptr ix =
  readMemoryLit ptr ix <$> ivoryPP_mem <$> get

-- | Read a 'Ptr' from the 'IvoryMemory' being printed
ppReadMemPtr :: Ptr -> Word64 -> IvoryMemPP Ptr
ppReadMemPtr ptr ix =
  readMemoryPtr ptr ix <$> ivoryPP_mem <$> get

-- | Read the length associated with a 'Ptr' in the 'IvoryMemory' being printed
ppPtrLen :: Ptr -> IvoryMemPP Word64
ppPtrLen ptr = readMemoryLen ptr <$> ivoryPP_mem <$> get

-- | Pretty-print a pointer, given a computation for printing its contents. This
-- either prints the pointer value followed by the contents, if the pointer has
-- not yet been printed, or simply prints the pointer value if it has.
ppIvoryPtr :: Ptr -> IvoryMemPP Doc -> IvoryMemPP Doc
ppIvoryPtr ptr ppm =
  do let ptr_pp = text $ showHex (unPtr ptr) ""
     info <- get
     (if elem ptr (ivoryPP_seen_ptrs info) then
        return ptr_pp
      else do set $ info { ivoryPP_seen_ptrs = ptr : ivoryPP_seen_ptrs info }
              pp <- ppm
              return $ hang (ptr_pp <+> equals) 2 pp)

-- | Pretty-print a value in memory at a given Ivory type
ppIvoryVal :: I.Type -> Ptr -> Word64 -> IvoryMemPP Doc
ppIvoryVal I.TyVoid _ _ = return $ text "()"
ppIvoryVal (I.TyInt _) ptr ix =
  -- FIXME HERE: convert to the proper number of bits + sign
  text . showString "0x" . flip showHex "" <$> ppReadMem64 ptr ix
ppIvoryVal (I.TyWord _) ptr ix =
  -- FIXME HERE: convert to the proper number of bits
  text . show <$> ppReadMem64 ptr ix
ppIvoryVal (I.TyIndex _) ptr ix =
  -- FIXME HERE: should we take a modulus here?
  text . show <$> ppReadMem64 ptr ix
ppIvoryVal I.TyBool ptr ix =
  -- FIXME HERE: Boolean conversion...?
  text . show <$> ppReadMem64 ptr ix
ppIvoryVal I.TyChar ptr ix =
  -- FIXME HERE: Character conversion...?
  text . show <$> ppReadMem64 ptr ix
ppIvoryVal I.TyFloat ptr ix =
  error "ppIvoryVal: floats not yet supported"
ppIvoryVal I.TyDouble ptr ix =
  error "ppIvoryVal: doubles not yet supported"
ppIvoryVal (I.TyProc _ _) ptr ix =
  error "ppIvoryVal: function pointers not (yet?) supported"
ppIvoryVal (I.TyRef itp) ptr ix =
  do ptr' <- ppReadMemPtr ptr ix
     ppIvoryPtr ptr' $ ppIvoryVal itp ptr' 0
ppIvoryVal (I.TyConstRef itp) ptr ix =
  -- FIXME HERE: is a constant reference different from a normal one?
  do ptr' <- ppReadMemPtr ptr ix
     ppIvoryPtr ptr' $ ppIvoryVal itp ptr' 0
ppIvoryVal (I.TyPtr itp) ptr ix =
  do ptr' <- ppReadMemPtr ptr ix
     if ptr' == nullPtr then return $ text "null" else
       ppIvoryPtr ptr' $ ppIvoryVal itp ptr' 0
ppIvoryVal (I.TyArr len itp) ptr ix =
  -- FIXME: check if the length is correct...?
  do ptr' <- ppReadMemPtr ptr ix
     ptr_len <- ppPtrLen ptr'
     ppIvoryPtr ptr' $ do pps <- mapM (ppIvoryVal itp ptr') [0 .. ptr_len - 1]
                          return $ fsep $ punctuate comma pps
ppIvoryVal (I.TyCArray itp) ptr ix =
  do ptr' <- ppReadMemPtr ptr ix
     ptr_len <- ppPtrLen ptr'
     ppIvoryPtr ptr' $ do pps <- mapM (ppIvoryVal itp ptr') [0 .. ptr_len - 1]
                          return $ fsep $ punctuate comma pps
ppIvoryVal (I.TyStruct s_name) ptr ix =
  do ptr' <- ppReadMemPtr ptr ix
     struct <- lookupStruct s_name <$> ivoryPP_mods <$> get
     case struct of
       I.Abstract _ _ -> return $ text ("<Abstract struct: " ++ s_name ++ ">")
       I.Struct _ flds ->
         ppIvoryPtr ptr' $
         do pps <- forM (zip [0 .. ] flds) $ \(ix', fld) ->
              hang (text (I.tValue fld) <+> equals) 2 <$>
              ppIvoryVal (I.tType fld) ptr' ix'
            return $ fsep $ punctuate comma pps
ppIvoryVal I.TyOpaque ptr ix = return $ text "<Opaque>"

-- | Pretty-print the arguments to an Ivory function
ppIvoryArgs :: [I.Typed I.Var] -> IvoryMemPP Doc
ppIvoryArgs args =
  do pps <- forM (zip [0 .. ] args) $ \(ix, arg) ->
       hang (text (show $ I.tValue arg) <+> equals) 2 <$>
       ppIvoryVal (I.tType arg) (Ptr $ -1) ix
     return $ fsep pps
