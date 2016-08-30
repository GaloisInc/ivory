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
defaultOpts :: ILOpts
defaultOpts = ILOpts { debugLevel = 0, skipCompilerAsserts = False,
                       loopUnrollingMax = 5, inlineCalls = False }


----------------------------------------------------------------------
-- A GADT version of the Ivory types
----------------------------------------------------------------------

-- | Dummy datatype used to represent the Ivory pointer type, which is different
-- from the built-in reachability logic pointer type: Ivory pointers are
-- represented as a pair of a base pointer plus an offset
data IvoryPtr

-- | The different sorts of pointers in Ivory
data IPtrSort
  = IPtrSort_Ptr
    -- ^ General pointers, e.g., from C
  | IPtrSort_Ref
    -- ^ Non-nullable pointers
  | IPtrSort_ConstRef
    -- ^ Non-nullable pointers whose contents are read-only
  deriving Eq

-- | Dummy datatype used for Ivory structs and arrays (not pointers to them, but
-- the objects themselves), which cannot be directly used as values, but instead
-- can only be manipulated via pointers
data IvoryMemObj

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
  ITyPtr :: IPtrSort -> I1Type a -> I1Type IvoryPtr
  ITyArray :: Int -> I1Type a -> I1Type IvoryMemObj
  ITyStruct :: String -> I1Type IvoryMemObj
  ITyCArray :: I1Type a -> I1Type IvoryMemObj
  ITyOpaque :: I1Type (Literal ())

-- | Some existentially-quantified first-order Ivory type
data SomeI1Type where
  SomeI1Type :: I1Type a -> SomeI1Type

-- | Flatten a pointer sort into a function on Ivory types
flattenPtrSort :: IPtrSort -> I.Type -> I.Type
flattenPtrSort IPtrSort_Ptr itp = I.TyPtr itp
flattenPtrSort IPtrSort_Ref itp = I.TyRef itp
flattenPtrSort IPtrSort_ConstRef itp = I.TyConstRef itp

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
  flattenPtrSort ptr_sort $ flattenI1Type i1tp
flattenI1Type (ITyArray len i1tp) = I.TyArr len $ flattenI1Type i1tp
flattenI1Type (ITyStruct s_name) = I.TyStruct s_name
flattenI1Type (ITyCArray i1tp) =
  I.TyCArray $ flattenI1Type i1tp
flattenI1Type ITyOpaque = I.TyOpaque

-- Show first-order Ivory types by showing their flat counterparts
instance Show (I1Type a) where
  show i1tp = show (flattenI1Type i1tp)

-- | The result of converting an 'I1Type' to reachability logic
data I1ConvType a where
  I1ConvLitType :: LitType a -> I1ConvType (Literal a)
  I1ConvPtrType :: I1ConvType IvoryPtr
  I1ConvObjType :: I1ConvType IvoryMemObj

-- | Convert a first-order Ivory type to reachability logic via an 'I1ConvType'
convertI1Type :: I1Type a -> I1ConvType a
convertI1Type ITyVoid = I1ConvLitType litTypeRep
convertI1Type ITyBits = I1ConvLitType LitType_bits
convertI1Type (ITyIndex _) = I1ConvLitType litTypeRep
convertI1Type ITyBool = I1ConvLitType litTypeRep
convertI1Type ITyChar = error "convertI1Type: Char type not yet implemented!"
convertI1Type ITyFloat = I1ConvLitType litTypeRep
convertI1Type ITyDouble = I1ConvLitType litTypeRep
convertI1Type (ITyPtr _ _) = I1ConvPtrType
convertI1Type (ITyArray _ _) = I1ConvObjType
convertI1Type (ITyStruct _) = I1ConvObjType
convertI1Type (ITyCArray _) = I1ConvObjType
convertI1Type ITyOpaque = I1ConvLitType litTypeRep

-- | Helper for building a reference type
mkRefType :: I1Type a -> I1Type IvoryPtr
mkRefType i1tp = ITyPtr IPtrSort_Ref i1tp

-- | Convert a "flat" Ivory type into its GADT representation
gadtType :: I.Type -> SomeI1Type
gadtType I.TyVoid = SomeI1Type ITyVoid
gadtType (I.TyInt I.Int8) = SomeI1Type (ITyBits :: I1Type (Literal Int8))
gadtType (I.TyInt I.Int16) = SomeI1Type (ITyBits :: I1Type (Literal Int16))
gadtType (I.TyInt I.Int32) = SomeI1Type (ITyBits :: I1Type (Literal Int32))
gadtType (I.TyInt I.Int64) = SomeI1Type (ITyBits :: I1Type (Literal Int64))
gadtType (I.TyWord I.Word8) = SomeI1Type (ITyBits :: I1Type (Literal Word8))
gadtType (I.TyWord I.Word16) = SomeI1Type (ITyBits :: I1Type (Literal Word16))
gadtType (I.TyWord I.Word32) = SomeI1Type (ITyBits :: I1Type (Literal Word32))
gadtType (I.TyWord I.Word64) = SomeI1Type (ITyBits :: I1Type (Literal Word64))
gadtType (I.TyIndex upper_bound) = SomeI1Type (ITyIndex upper_bound)
gadtType I.TyBool = SomeI1Type ITyBool
gadtType I.TyChar = SomeI1Type ITyChar
gadtType I.TyFloat = SomeI1Type ITyFloat
gadtType I.TyDouble = SomeI1Type ITyDouble
gadtType (I.TyProc out_type in_types) =
  error "gadtType: cannot (yet) handle function types"
gadtType (I.TyRef itp) =
  case gadtType itp of
    SomeI1Type i1tp -> SomeI1Type $ ITyPtr IPtrSort_Ref i1tp
gadtType (I.TyConstRef itp) =
  case gadtType itp of
    SomeI1Type i1tp -> SomeI1Type $ ITyPtr IPtrSort_ConstRef i1tp
gadtType (I.TyPtr itp) =
  case gadtType itp of
    SomeI1Type i1tp -> SomeI1Type $ ITyPtr IPtrSort_Ptr i1tp
gadtType (I.TyArr len itp) =
  case gadtType itp of
    SomeI1Type i1tp -> SomeI1Type $ ITyArray len i1tp
gadtType (I.TyStruct s_name) = SomeI1Type $ ITyStruct s_name
gadtType (I.TyCArray itp) =
  case gadtType itp of
    SomeI1Type i1tp -> SomeI1Type $ ITyCArray i1tp
gadtType I.TyOpaque = SomeI1Type ITyOpaque

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
i1TypeEq (ITyArray len i1tp) (ITyArray len' i1tp')
  | len == len'
  = case i1TypeEq i1tp i1tp' of
      Just Refl -> Just Refl
      Nothing -> Nothing
i1TypeEq (ITyArray _ _) _ = Nothing
i1TypeEq (ITyStruct s_name) (ITyStruct s_name')
  | s_name == s_name' = Just Refl
i1TypeEq (ITyStruct _) _ = Nothing
i1TypeEq (ITyCArray i1tp) (ITyCArray i1tp') =
  case i1TypeEq i1tp i1tp' of
    Just Refl -> Just Refl
    Nothing -> Nothing
i1TypeEq (ITyCArray _) _ = Nothing
i1TypeEq ITyOpaque ITyOpaque = Just Refl
i1TypeEq ITyOpaque _ = Nothing

-- | Test type equality, like 'i1TypeEq', but allow some automatic conversions
-- (like indexed types --> Int32) that maintain the same logical type
i1TypeEqAuto :: I1Type a -> I1Type b -> Maybe (a :~: b)
i1TypeEqAuto (ITyIndex i) i1tp@ITyBits =
  i1TypeEq (ITyBits :: I1Type (Literal Int32)) i1tp
i1TypeEqAuto i1tp i1tp' = i1TypeEq i1tp i1tp'


----------------------------------------------------------------------
-- The Ivory version of the reachability logic
----------------------------------------------------------------------

-- | Tag type indicating the Ivory reachability logic
data IvoryLogic

-- | Propositions in the Ivory reachability logic
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

-- | The result of converting an Ivory variable into the logic
data IConvName a where
  IName :: LitType a -> Name (Literal a) -> IConvName (Literal a)
  -- ^ Normal variable of first-order type @a@
  IPtrName :: Name Ptr -> Name (Literal Word64) -> IConvName IvoryPtr
  -- ^ Pair of variable names, for a pointer and an offset

-- | An Ivory variable of some unknown first-order type
data SomeIConvName where
  SomeIConvName :: I1Type a -> IConvName a -> SomeIConvName

-- | The result of converting an Ivory expression into the rechability logic
data IConvExpr a where
  IExpr :: LitType a -> ILExpr (Literal a) -> IConvExpr (Literal a)
  -- ^ Normal logical expression of type first-order @a@
  IPtrExpr :: ILExpr Ptr -> ILExpr (Literal Word64) -> IConvExpr IvoryPtr
  -- ^ Pair of logical expressions, for a pointer plus an offset

-- | Extract the 'Literal' 'ILExpr' from a 'Literal' 'IConvExpr'
extractLitExpr :: IConvExpr (Literal a) -> ILExpr (Literal a)
extractLitExpr (IExpr _ e) = e

-- | Make an Ivory proposition from an 'IConvExpr'
mkIsTrueIvory :: IConvExpr (Literal Bool) -> ILProp
mkIsTrueIvory (IExpr _ e) = mkIsTrue e

-- | Extract the base 'Ptr' and index of an Ivory pointer expression
basePtrAndIndex :: IConvExpr IvoryPtr -> (ILExpr Ptr, ILExpr (Literal Word64))
basePtrAndIndex (IPtrExpr ptr_expr ix_expr) = (ptr_expr, ix_expr)

-- | Add an index to an Ivory pointer expression
ptrAdd :: IConvExpr IvoryPtr -> IConvExpr (Literal Word64) -> IConvExpr IvoryPtr
ptrAdd (IPtrExpr ptr_expr ix_expr) (IExpr _ offset_expr) =
  IPtrExpr ptr_expr $ ix_expr + offset_expr

-- | Add an integral index to an Ivory pointer expression
ptrAddI :: Integral a => IConvExpr IvoryPtr -> a -> IConvExpr IvoryPtr
ptrAddI ptr i = ptrAdd ptr $ IExpr litTypeRep $ mkLiteral $ fromIntegral i

-- | Apply a unary function on 'Literal' 'ILExpr's to one on 'IConvExpr's
applyLitFun1 :: (ILExpr (Literal a) -> ILExpr (Literal a)) ->
               IConvExpr (Literal a) -> IConvExpr (Literal a)
applyLitFun1 f (IExpr lit_tp e) = IExpr lit_tp $ f e

-- | Lift a binary function on 'Literal' 'ILExpr's to one on 'IConvExpr's
applyLitFun2 :: (ILExpr (Literal a) -> ILExpr (Literal a) ->
                ILExpr (Literal a)) ->
               IConvExpr (Literal a) -> IConvExpr (Literal a) ->
               IConvExpr (Literal a)
applyLitFun2 f (IExpr lit_tp e1) (IExpr _ e2) = IExpr lit_tp $ f e1 e2

-- | An Ivory reachability logic of some unknown first-order Ivory type
data SomeIConvExpr where
  SomeIConvExpr :: I1Type a -> IConvExpr a -> SomeIConvExpr

-- | Convert an Ivory reachability logic variable to an expression
ivoryName2Expr :: IConvName a -> IConvExpr a
ivoryName2Expr (IName lit_tp n) = IExpr lit_tp $ mkLitVar lit_tp n
ivoryName2Expr (IPtrName n1 n2) =
  IPtrExpr (mkVar Proxy n1) (mkVar Proxy n2)


----------------------------------------------------------------------
-- Monad for converting Ivory expressions into reachability logic
----------------------------------------------------------------------

-- | The state information used for building transition relations
data I2LInfo =
  I2LInfo
  {
    i2l_opts :: ILOpts,
    -- ^ The configuration options passed in for converting to logic
    i2l_modules :: [I.Module],
    -- ^ The Ivory modules that are in scope
    i2l_vars :: Map.Map I.Var SomeIConvName,
    -- ^ The Ivory variables that are in scope, and the Ivory
    -- reachability logic variables associated with them
    i2l_syms :: Map.Map I.Sym Natural,
    -- ^ An association from Ivory global symbols and unique numbers for them
    i2l_single_err :: Maybe (Name (Literal Rational -> Literal Rational)),
    -- ^ A function for the relative round-off error for the @Float@ type
    i2l_double_err :: Maybe (Name (Literal Rational -> Literal Rational))
    -- ^ A function for the relative round-off error for the @Double@ type
  }

-- | The monad for converting Ivory statements into the Ivory reachability logic
newtype I2LConvM a =
  I2LConvM { runIvory2LogicM :: StateT I2LInfo (ContT ILPM Id) a }
  deriving (Functor, Applicative, Monad)

instance StateM I2LConvM I2LInfo where
  get = I2LConvM get
  set info = I2LConvM $ set info

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

instance ShiftResetPureM I2LConvM ILPM where
  shiftPureM f = I2LConvM $ shiftPureM f
  resetPureM (I2LConvM m) = I2LConvM $ resetPureM m

-- | Add a transition to the output, by shifting the continuation and sequencing
-- its results after the given transition
addTransition :: ILPM -> I2LConvM ()
addTransition pm = shiftPureM $ \k -> mkSeqP pm (k ())

-- | Add a bind transition to the output, that binds the result of a transition
-- with a value to a fresh variable
addBindTransition :: LTypeable a => ILExpr (PM a) -> I2LConvM (ILExpr a)
addBindTransition pm = shiftPureM $ \k -> mkBindP pm k

-- | Use 'resetPureM' computation to collect a computation's transition(s)
collectTransitions :: I2LConvM () -> I2LConvM ILPM
collectTransitions m =
  resetPureM (m >> return (mkReturnP $ mkLiteral ()))

-- | Add a catch for the given exception around the transitions generated by a
-- given computation
ivoryCatch :: IvoryExn -> I2LConvM () -> I2LConvM ()
ivoryCatch exn m =
  do pm <- collectTransitions m
     addTransition $ mkCatchP exn pm (mkReturnP $ mkLiteral ())

-- | Add an assumption to the current transition relation
ivoryAssume :: ILProp -> I2LConvM ()
ivoryAssume prop = addTransition $ mkAssumeP prop

-- | Add an assertion to the current transition relation
ivoryAssert :: ILProp -> I2LConvM ()
ivoryAssert prop = addTransition $ mkAssertP IvoryError prop

-- | The number of reserved / "special" global variable numbers
numReservedSyms = 2

-- | A pointer expression used to store function arguments and return values
funArgsPtr :: IConvExpr IvoryPtr
funArgsPtr = IPtrExpr (mkOp (Op_global_var 1)) (mkLiteral 0)

-- | A pointer expression used to store loop counters
loopCounterPtr :: IConvExpr IvoryPtr
loopCounterPtr = IPtrExpr (mkOp (Op_global_var 2)) (mkLiteral 0)

-- | Get all the global variable symbols in a module
moduleGlobalSyms :: I.Module -> Set.Set I.Sym
moduleGlobalSyms mod =
  Set.unions [Set.fromList $ map I.externSym $ I.modExterns mod,
              Set.fromList $ map I.importSym $ I.modImports mod,
              Set.fromList $ map I.areaSym $ I.public $ I.modAreas mod,
              Set.fromList $ map I.areaSym $ I.private $ I.modAreas mod,
              Set.fromList $ map I.aiSym $ I.modAreaImports mod]

-- An I2LConvM computation can be "run" by giving it Ivory modules and a
-- default continuation (which is normally just "return")
instance RunM I2LConvM a (ILOpts -> [I.Module] -> (a -> ILPM) -> ILPM) where
  runM (I2LConvM m) opts mods k =
    let global_syms = Set.toList $ Set.unions $ map moduleGlobalSyms mods in
    let syms_map =
          Map.fromList $ zip global_syms [numReservedSyms + 1 ..]
    in
    runM m (I2LInfo { i2l_opts = opts,
                      i2l_modules = mods,
                      i2l_vars = Map.empty,
                      i2l_syms = syms_map,
                      i2l_single_err = Nothing,
                      i2l_double_err = Nothing })
    (return . k . fst)


----------------------------------------------------------------------
-- Handling Ivory variables
----------------------------------------------------------------------

-- | Look up the logical variable associated with an Ivory variable
lookupVar :: I.Var -> I2LConvM (Maybe SomeIConvName)
lookupVar v =
  do info <- get
     return $ Map.lookup v (i2l_vars info)

-- | Get the logical variable associated with an Ivory variable at a
-- specific type, raising an error if this does not work
getVar :: I1Type a -> I.Var -> I2LConvM (IConvName a)
getVar i1tp v =
  do maybe_some_name <- lookupVar v
     case maybe_some_name of
       Nothing -> error ("getVar: could not find Ivory variable " ++ show v)
       Just (SomeIConvName i1tp' n) ->
         case i1TypeEqAuto i1tp' i1tp of
           Just Refl -> return n
           Nothing ->
             error ("getVar: Ivory variable " ++ show v ++
                    " has the wrong type; expected: " ++ show i1tp ++
                    "; found: " ++ show i1tp')

-- | Get the logical variable associated with an Ivory variable, at a specific
-- type, and convert it to a logical expression
getVarExpr :: I1Type a -> I.Var -> I2LConvM (IConvExpr a)
getVarExpr i1tp var = ivoryName2Expr <$> getVar i1tp var

-- | Bind a logical variable to the result of a 'PM' logical expression, and
-- return the bound name. This is done by building the expression @pm >>= k@,
-- where @k@ is the continuation of the rest of the expression being built.
bindName :: LType a -> ILExpr (PM a) -> I2LConvM (Name a)
bindName ltp e =
  shiftPureM $ \k -> mkOp (Op_bindP ltp ltypeRep) e $ LLambda ltp $ nu k

-- | Associate an Ivory variable name with a logic variable
linkVarAndName :: I.Var -> SomeIConvName -> I2LConvM ()
linkVarAndName var n =
  do info <- get
     set $ info { i2l_vars =
                    Map.insertWith
                    (\_ _ ->
                      error ("linkVarAndName: duplicate binding for variable "
                             ++ show var))
                    var n (i2l_vars info)
                }

-- | Clear the variable bindings from the current computation
clearVarBindings :: I2LConvM ()
clearVarBindings =
  do info <- get
     set $ info { i2l_vars = Map.empty }

-- | Bind an Ivory variable to the result of a computation of a regular
-- first-order type
pmBindLitVar :: I.Var -> I1Type (Literal a) -> LitType a ->
                ILExpr (PM (Literal a)) -> I2LConvM ()
pmBindLitVar var i1tp lit_tp pm =
  do n <- bindName (LType_base $ L1Type_lit lit_tp) pm
     linkVarAndName var (SomeIConvName i1tp (IName lit_tp n))

-- | Bind an Ivory pointer variable to the results of two computations, one for
-- each of the pointer components
pmBindPtrVar :: I.Var -> I1Type IvoryPtr ->
                ILExpr (PM Ptr) -> ILExpr (PM (Literal Word64)) -> I2LConvM ()
pmBindPtrVar var i1tp pm1 pm2 =
  do n1 <- bindName ltypeRep pm1
     n2 <- bindName ltypeRep pm2
     linkVarAndName var (SomeIConvName i1tp (IPtrName n1 n2))

-- | Let-bind an Ivory variable to a value, and store this binding in
-- the 'I2LInfo' transition-relation-building state. The let-binding
-- is done by building the expression @(return x) >>= k@, where @k@ is
-- the continuation of the rest of the expression being built.
letBindVar :: I.Var -> I1Type a -> IConvExpr a -> I2LConvM ()
letBindVar var i1tp (IExpr lit_tp (LVar (LTypeArgs_base _) n _)) =
  -- Special case: if the RHS is already a variable, no need to bind a new one!
  linkVarAndName var (SomeIConvName i1tp (IName lit_tp n))
letBindVar var i1tp (IPtrExpr (LVar (LTypeArgs_base _) n1 _)
                     (LVar (LTypeArgs_base _) n2 _)) =
  -- Special case: as above, if the RHS is a pair of variables for a pointer
  linkVarAndName var (SomeIConvName i1tp (IPtrName n1 n2))
letBindVar var i1tp (IExpr lit_tp e) =
  pmBindLitVar var i1tp lit_tp (mkReturnP_tp (L1Type_lit lit_tp) e)
letBindVar var i1tp (IPtrExpr e1 e2) =
  pmBindPtrVar var i1tp (mkReturnP_tp l1typeRep e1) (mkReturnP_tp l1typeRep e2)

-- | Let-bind an Ivory variable to a value of unknown type (see 'letBindVar')
letBindVarSome :: I.Var -> SomeIConvExpr -> I2LConvM ()
letBindVarSome var (SomeIConvExpr i1tp e) = letBindVar var i1tp e

-- | Similar to 'letBindVar', except the given Ivory variable is
-- existentially-bound, meaning no explicit value is given for it.
exBindVar :: I.Var -> I1Type a -> I2LConvM ()
exBindVar var i1tp@(convertI1Type -> I1ConvLitType lit_tp) =
  pmBindLitVar var i1tp lit_tp (mkExistsP_tp (L1Type_lit lit_tp))
exBindVar var i1tp@(convertI1Type -> I1ConvPtrType) =
  pmBindPtrVar var i1tp (mkExistsP_tp l1typeRep) (mkExistsP_tp l1typeRep)
exBindVar var i1tp@(convertI1Type -> I1ConvObjType) =
  error "exBindVar: cannot bind variables for memory objects / regions"

-- | Similar to 'exBindVar', but where the type of the variable is given as an
-- Ivory type, using the 'I.Typed' construction
exBindTypedVar :: I.Typed I.Var -> I2LConvM ()
exBindTypedVar typed_var =
  case gadtType (I.tType typed_var) of
    SomeI1Type i1tp -> exBindVar (I.tValue typed_var) i1tp


----------------------------------------------------------------------
-- Handling floating-point round-off error
----------------------------------------------------------------------

-- | Generate a floating-point error function symbol given two parameters: the
-- relative error (aka the "machine epsilon") and the max non-infinite number
-- (FIXME: this does not correctly handle numbers very close to 0...)
mkFloatErrFun :: Rational -> Rational ->
                 I2LConvM (Name (Literal Rational -> Literal Rational))
mkFloatErrFun rel_err max_val =
  do err_fun <- bindName ltypeRep (mkExistsP_ftp l1funTypeRep)
     ivoryAssume $
       mkForall l1typeRep $ \r ->
       mkOr [mkLt (mkLiteral max_val) r,
             mkLt (mkLiteral (- max_val)) r,
             mkAnd [mkLe (mkLiteral (- rel_err)) (mkVar Proxy err_fun r),
                    mkLe (mkVar Proxy err_fun r) (mkLiteral rel_err)]]
     return err_fun

-- | Look up the round-off error function for the single-precision
-- floating-point type, creating it if necessary
getSingleErrFun :: I2LConvM (Name (Literal Rational -> Literal Rational))
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
getDoubleErrFun :: I2LConvM (Name (Literal Rational -> Literal Rational))
getDoubleErrFun =
  do s <- get
     case i2l_double_err s of
       Just err_fun -> return err_fun
       Nothing ->
         do err_fun <- mkFloatErrFun (2 ^^ (-53)) (2 ^^ 128)
            set $ s { i2l_double_err = Just err_fun }
            return err_fun

-- | Round an expression if its type requires it
roundIfNeeded :: I1Type a -> IConvExpr a -> I2LConvM (IConvExpr a)
roundIfNeeded ITyFloat (IExpr lit_tp e) =
  do err_fun <- getSingleErrFun
     return $ IExpr lit_tp $ e * (mkVar Proxy err_fun e)
roundIfNeeded ITyDouble (IExpr lit_tp e) =
  do err_fun <- getDoubleErrFun
     return $ IExpr lit_tp $ e * (mkVar Proxy err_fun e)
roundIfNeeded _ e = return e


----------------------------------------------------------------------
-- Coercion between (first-order) Ivory types
----------------------------------------------------------------------

-- | Coerce an Ivory logic expression from one Ivory type to another
ivoryCoerce :: I1Type f -> I1Type t -> IConvExpr f -> I2LConvM (IConvExpr t)
ivoryCoerce (convertI1Type ->
             I1ConvLitType lit_tp_f) (ITyIndex bound) (IExpr _ e) =
  -- To coerce to an index type, first coerce to Int32 then take the mod
  return $ IExpr litTypeRep $
  mkArithOp2 litTypeRep Op2_Mod (mkCoerce lit_tp_f litTypeRep e)
  (convertInteger ITyBits bound)
ivoryCoerce ITyDouble ITyFloat e =
  -- Reducing floating-point precision causes extra rounding
  roundIfNeeded ITyFloat e
ivoryCoerce (convertI1Type -> I1ConvLitType lit_tp_f)
  (convertI1Type -> I1ConvLitType lit_tp_t) (IExpr _ e) =
  -- Default: use the underlying coercion in the reachability logic
  return $ IExpr lit_tp_t $ mkCoerce lit_tp_f lit_tp_t e
ivoryCoerce i1tp_from i1tp_to _ =
  error ("ivoryCoerce: Could not coerce from type " ++ show i1tp_from
         ++ " to type " ++ show i1tp_to)

-- | Coerce an expression of unknown type to a given type
ivoryCoerceSome :: I1Type a -> SomeIConvExpr -> I2LConvM (IConvExpr a)
ivoryCoerceSome i1tp_to (SomeIConvExpr i1tp_from e) =
  ivoryCoerce i1tp_from i1tp_to e


----------------------------------------------------------------------
-- Ivory state manipulation
----------------------------------------------------------------------

-- | Assert that a pointer and index pair is valid: check that the pointer is no
-- greater than the last-allocated pointer, and that the index is non-negative
-- and less than the length associated with the pointer. NOTE: negative pointers
-- are ok, as these are used to represent constant, global pointers.
assertPtrIndexOK :: ILExpr Ptr -> ILExpr (Literal Word64) -> I2LConvM ()
assertPtrIndexOK ptr ix =
  addTransition $
  mkBindP (mkOp (Op_readP ReadOp_last_alloc)) $ \last_alloc ->
  mkBindP (mkOp (Op_readP ReadOp_length) ptr) $ \len ->
  mkAssertP IvoryError $
  mkAnd [mkIsTrue (mkOp (Op_ptr_cmp OpCmp_LE) ptr last_alloc),
         mkIsTrue (mkOp (Op_cmp litTypeRep OpCmp_LT) ix len),
         mkIsTrue (mkOp (Op_cmp litTypeRep OpCmp_LE) (mkLiteral 0) ix)]

-- | Same as 'assertPtrIndexOk' on an Ivory pointer expression
assertPtrOK :: IConvExpr IvoryPtr -> I2LConvM ()
assertPtrOK ptr =
  let (base_ptr, ix) = basePtrAndIndex ptr in
  assertPtrIndexOK base_ptr ix

-- | Bind a name that is the result of an array read, of the given pointer at
-- the given index, of type 'Word64'. This is done by building the expression
-- @(readP read_op args) >>= k@ in the eventual result of the continuation,
-- where @k@ is the continuation of the rest of the expression being built.
readArrayNameWord64 :: ILExpr Ptr -> ILExpr (Literal Word64) ->
                       I2LConvM (Name (Literal Word64))
readArrayNameWord64 ptr ix =
  bindName ltypeRep $ mkOp (Op_readP (ReadOp_array Elem_base)) ptr ix

-- | Similar to 'readArrayNameWord64', but read a 'Rational' value instead
readArrayNameRational :: ILExpr Ptr -> ILExpr (Literal Word64) ->
                         I2LConvM (Name (Literal Rational))
readArrayNameRational ptr ix =
  bindName ltypeRep $
  mkOp (Op_readP (ReadOp_array $ Elem_cons Elem_base)) ptr ix

-- | Similar to 'readArrayNameWord64', but read a pointer value instead
readArrayNamePtr :: ILExpr Ptr -> ILExpr (Literal Word64) -> I2LConvM (Name Ptr)
readArrayNamePtr ptr ix =
  bindName ltypeRep $ mkOp (Op_readP ReadOp_ptr_array) ptr ix

-- | Read an array value, binding the result to a fresh local name and coercing
-- the name if necessary
readArray :: I1Type a -> IConvExpr IvoryPtr -> I2LConvM (IConvExpr a)
readArray i1tp@(convertI1Type -> I1ConvLitType lit_tp@LitType_unit) _ =
  -- Reading a unit = just return a unit value
  return $ IExpr lit_tp $ mkLiteral ()
readArray i1tp@(convertI1Type ->
                I1ConvLitType LitType_bool) (IPtrExpr ptr ix) =
  -- Reading a Boolean: read a Word64 and convert it to a Boolean
  do n <- readArrayNameWord64 ptr ix
     ivoryCoerce ITyBits i1tp $ IExpr litTypeRep $ mkVar Proxy n
readArray i1tp@(convertI1Type ->
                I1ConvLitType LitType_bits) (IPtrExpr ptr ix) =
  -- Reading any bitstring type: read a Word64 and convert it
  do n <- readArrayNameWord64 ptr ix
     ivoryCoerce ITyBits i1tp $ IExpr litTypeRep $ mkVar Proxy n
readArray i1tp@(convertI1Type -> I1ConvLitType LitType_rat) (IPtrExpr ptr ix) =
  -- Reading any real-valued type: read a Rational (no need to convert it); also
  -- note that there is no need to round, since we assume rounding happened
  -- before it was written
  do n <- readArrayNameRational ptr ix
     return $ IExpr litTypeRep $ mkVar Proxy n
readArray i1tp@(convertI1Type -> I1ConvPtrType) (IPtrExpr ptr ix) =
  -- Reading an Ivory pointer: read the underlying pointer value at the given
  -- index, and read the offset value at the succeeding index
  do n_ptr <- readArrayNamePtr ptr ix
     n_ix <- readArrayNameWord64 ptr (ix+1)
     return $ IPtrExpr (mkVar Proxy n_ptr) (mkVar Proxy n_ix)
readArray i1tp _ =
  error ("readArray at unsupported type: " ++ show i1tp)

-- | Perform a 'readArray' using a flattened Ivory type
readArraySome :: I.Type -> IConvExpr IvoryPtr -> I2LConvM SomeIConvExpr
readArraySome itp ptr =
  case gadtType itp of
    SomeI1Type i1tp -> SomeIConvExpr i1tp <$> readArray i1tp ptr

-- | Update an Ivory 64-bit word array value
updateArrayWord64 :: IConvExpr IvoryPtr -> IConvExpr (Literal Word64) ->
                     I2LConvM ()
updateArrayWord64 (IPtrExpr ptr ix) (IExpr _ v) =
  addTransition $ mkOp (Op_updateP $ UpdateOp_array Elem_base) ptr ix v

-- | Update an Ivory 'Rational' array value
updateArrayRational :: IConvExpr IvoryPtr -> IConvExpr (Literal Rational) ->
                       I2LConvM ()
updateArrayRational (IPtrExpr ptr ix) (IExpr _ v) =
  addTransition $
  mkOp (Op_updateP $ UpdateOp_array $ Elem_cons Elem_base) ptr ix v

-- | Update a 'Ptr' array value
updateArrayPtr :: IConvExpr IvoryPtr -> IConvExpr IvoryPtr -> I2LConvM ()
updateArrayPtr (IPtrExpr ptr ix) (IPtrExpr v_ptr v_ix) =
  (addTransition $ mkOp (Op_updateP UpdateOp_ptr_array) ptr ix v_ptr) >>
  (addTransition $
   mkOp (Op_updateP $ UpdateOp_array Elem_base) ptr (ix+1) v_ix)

-- | Update an Ivory array value, coercing the written value if necessary
updateArray :: I1Type a -> IConvExpr IvoryPtr -> IConvExpr a -> I2LConvM ()
updateArray i1tp@(convertI1Type -> I1ConvLitType LitType_unit) _ _ =
  -- Writing a unit object is just a no-op
  return ()
updateArray i1tp@(convertI1Type -> I1ConvLitType LitType_bool) ptr v =
  -- Writing a Boolean: convert to a Word64 and write that
  updateArrayWord64 ptr =<< ivoryCoerce i1tp ITyBits v
updateArray i1tp@(convertI1Type -> I1ConvLitType LitType_bits) ptr v =
  -- Writing any bitstring type: convert to a Word64 and write that
  updateArrayWord64 ptr =<< ivoryCoerce i1tp ITyBits v
updateArray i1tp@(convertI1Type -> I1ConvLitType LitType_rat) ptr v =
  -- Writing any Rational type: write it directly (assume it is already rounded)
  updateArrayRational ptr v
updateArray i1tp@(convertI1Type -> I1ConvPtrType) ptr v =
  -- Writing an Ivory pointer: write the Ptr to the given index and write the
  -- index at the next index
  updateArrayPtr ptr v
updateArray i1tp _ _ =
  error ("updateArray at unsupported type: " ++ show i1tp)

-- | Perform an 'updateArray' with a value of an unknown type
updateArraySome :: IConvExpr IvoryPtr -> SomeIConvExpr -> I2LConvM ()
updateArraySome ptr (SomeIConvExpr i1tp v) = updateArray i1tp ptr v

-- | Allocate an Ivory array of a given length, returning a pointer to it
allocateArray :: ILExpr (Literal Word64) -> I2LConvM (IConvExpr IvoryPtr)
allocateArray len =
  do ptr <-
       addBindTransition $
       mkSeqP (mkOp (Op_updateP UpdateOp_alloc) len) $
       mkOp (Op_readP ReadOp_last_alloc)
     return (IPtrExpr ptr 0)

-- | Let-bind the special Ivory variable "retval" to the result of a function
-- call, by reading this result from the current memory
bindRetval :: I1Type a -> I2LConvM ()
bindRetval ITyVoid =
  -- Special case: don't bind a "void"-typed variable
  return ()
bindRetval i1tp =
  do ret <- readArray i1tp funArgsPtr
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

-- | Do a 'lookupStruct' inside the 'I2LConvM' monad
lookupStructM :: String -> I2LConvM I.Struct
lookupStructM s_name = lookupStruct s_name <$> i2l_modules <$> get

-- | Get the size of a given type in memory
iTypeSizeM :: I.Type -> I2LConvM Int
iTypeSizeM I.TyVoid = return 1
iTypeSizeM (I.TyInt _) = return 1
iTypeSizeM (I.TyWord _) = return 1
iTypeSizeM (I.TyIndex _) = return 1
iTypeSizeM I.TyBool = return 1
iTypeSizeM I.TyChar = return 1
iTypeSizeM I.TyFloat = return 1
iTypeSizeM I.TyDouble = return 1
iTypeSizeM (I.TyProc _ _) = return 1
iTypeSizeM (I.TyRef _) = return 2 -- Pointers take 2 slots
iTypeSizeM (I.TyConstRef _) = return 2
iTypeSizeM (I.TyPtr _) = return 2
iTypeSizeM (I.TyArr n t) = (n *) <$> iTypeSizeM t
iTypeSizeM (I.TyStruct s_name) = structSizeM s_name
iTypeSizeM (I.TyCArray _) =
  error "iTypeSizeM: Cannot determine the size of an arbitrary C array!"
iTypeSizeM I.TyOpaque = return 1

-- | Look up the number of slots used by a struct
structSizeM :: String -> I2LConvM Int
structSizeM s_name =
  do s <- lookupStructM s_name
     case s of
       I.Struct _ fields ->
         sum <$> mapM (iTypeSizeM . I.tType) fields
       _ -> error "structSizeM: abstract struct!"

-- | Get the name, type, and starting slot number of all fields in a struct
lookupStructFieldsM :: String -> I2LConvM [(String, (I.Type, Int))]
lookupStructFieldsM s_name =
  do s <- lookupStructM s_name
     let fields =
           case s of
             I.Struct _ fields -> fields
             _ -> error "lookupStructFieldsM: abstract struct!"
     helper 0 fields
       where
         helper ix [] = return []
         helper ix (f : fs) =
           do size <- iTypeSizeM (I.tType f)
              rest <- helper (ix + size) fs
              return $ (I.tValue f, (I.tType f, ix)) : rest

-- | Get the type and the starting slot number of a field in a given struct
lookupStructFieldM :: String -> String -> I2LConvM (I.Type, Int)
lookupStructFieldM s_name f_name =
  do fs <- lookupStructFieldsM s_name
     case lookup f_name fs of
       Just res -> return res
       Nothing -> 
         error ("lookupStructFieldM: could not find field " ++ f_name
                ++ "in struct " ++ s_name ++ "!")

-- | Look up an Ivory function
lookupProc :: I.Sym -> [I.Module] -> I.Proc
lookupProc sym mods =
  moduleLookup I.modProcs I.procSym sym "function" mods

-- | Do a 'lookupProc' inside the 'I2LConvM' monad
lookupProcM :: I.Sym -> I2LConvM I.Proc
lookupProcM sym = lookupProc sym <$> i2l_modules <$> get


----------------------------------------------------------------------
-- Converting Ivory expressions into Ivory reachability logic
----------------------------------------------------------------------

-- | Convert a symbol to a pointer
convertSymbol :: I.Sym -> I2LConvM (IConvExpr IvoryPtr)
convertSymbol sym =
  do info <- get
     case Map.lookup sym (i2l_syms info) of
       Just i -> return $ IPtrExpr (mkOp (Op_global_var i)) 0
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
convertLiteral :: I1Type a -> I.Literal -> IConvExpr a
convertLiteral i1tp@(convertI1Type -> I1ConvLitType lit_tp) (I.LitInteger i) =
  IExpr lit_tp $ convertInteger i1tp i
convertLiteral ITyFloat (I.LitFloat x) =
  IExpr litTypeRep $ mkLiteral $ toRational x
convertLiteral ITyDouble (I.LitDouble x) =
  IExpr litTypeRep $ mkLiteral $ toRational x
convertLiteral _ (I.LitChar c) =
  error "convertLiteral: characters not (yet) supported"
convertLiteral ITyBool (I.LitBool b) = IExpr litTypeRep $ mkLiteral b
convertLiteral (ITyPtr _ _) I.LitNull =
  IPtrExpr (mkOp Op_null_ptr) 0
convertLiteral _ (I.LitString str) =
  error "convertLiteral: strings not (yet) supported"
convertLiteral _ _ =
  error "convertLiteral: literal used at incorrect type"

-- | Convert an expressions an apply an 'ArithOp1' to it
convertArithOp1 :: I1Type a -> ArithOp1 -> I.Expr -> I2LConvM (IConvExpr a)
convertArithOp1 i1tp@(convertI1Type -> I1ConvLitType _) aop ie =
  do IExpr lit_tp e <- convertExpr i1tp ie
     roundIfNeeded i1tp $ IExpr lit_tp $ mkOp (Op_arith1 lit_tp aop) e
convertArithOp1 i1tp _ _ =
  error ("convertArithOp1: arithmetic on unsupported type: " ++ show i1tp)

-- | Convert two expressions an apply an 'ArithOp2' to them
convertArithOp2 :: I1Type a -> ArithOp2 -> I.Expr -> I.Expr ->
                   I2LConvM (IConvExpr a)
convertArithOp2 i1tp@(convertI1Type -> I1ConvLitType _) aop ie1 ie2 =
  do IExpr lit_tp e1 <- convertExpr i1tp ie1
     IExpr _ e2 <- convertExpr i1tp ie2
     roundIfNeeded i1tp $ IExpr lit_tp $ mkOp (Op_arith2 lit_tp aop) e1 e2
convertArithOp2 i1tp _ _ _ =
  error ("convertArithOp2: arithmetic on unsupported type: " ++ show i1tp)

-- | Convert two expressions an apply an 'ArithCmp' to them
convertArithCmp :: I.Type -> ArithCmp -> I.Expr -> I.Expr ->
                   I2LConvM (IConvExpr (Literal Bool))
convertArithCmp (gadtType ->
                 SomeI1Type i1tp@(convertI1Type ->
                                  I1ConvLitType lit_tp)) acmp ie1 ie2 =
  do e1 <- extractLitExpr <$> convertExpr i1tp ie1
     e2 <- extractLitExpr <$> convertExpr i1tp ie2
     return $ IExpr litTypeRep $ mkArithCmpTp (L1Type_lit lit_tp) acmp e1 e2

-- | Convert an Ivory expression to a logical expression using an Ivory type
convertSomeExpr :: I.Type -> I.Expr -> I2LConvM SomeIConvExpr
convertSomeExpr itp ie =
  case gadtType itp of
    SomeI1Type i1tp -> SomeIConvExpr i1tp <$> convertExpr i1tp ie

-- | Convert a typed Ivory expression to a logical expression
convertTypedExpr :: I.Typed I.Expr -> I2LConvM SomeIConvExpr
convertTypedExpr typed_ie =
  convertSomeExpr (I.tType typed_ie) (I.tValue typed_ie)

-- | Convert an Ivory expression to a logical expression of a specific type
convertExpr :: I1Type a -> I.Expr -> I2LConvM (IConvExpr a)

-- Symbols and variables
convertExpr i1tp (I.ExpSym sym) =
  do ptr_expr <- convertSymbol sym
     readArray i1tp ptr_expr
convertExpr i1tp (I.ExpExtern ext) =
  do ptr_expr <- convertSymbol (I.externSym ext)
     readArray i1tp ptr_expr
convertExpr i1tp (I.ExpVar v) = getVarExpr i1tp v

-- Literals
convertExpr i1tp (I.ExpLit lit) =
  return $ convertLiteral i1tp lit

-- Array and struct dereferencing

convertExpr (ITyPtr ptr_sort f_tp)
  (I.ExpLabel (I.TyStruct s_name) s_iexpr f_name) =
  do (f_itp, f_ix) <- lookupStructFieldM s_name f_name
     if f_itp == flattenI1Type f_tp then return () else
       error ("convertExpr: dereferencing struct field at incorrect type!")
     s_ptr_expr <- convertExpr (ITyPtr ptr_sort $ ITyStruct s_name) s_iexpr
     return $ ptrAddI s_ptr_expr f_ix

convertExpr i1tp (I.ExpLabel itp s_iexpr f_name) =
  error "convertExpr: struct dereference at invalid type!"

convertExpr (ITyPtr ptr_sort elem_tp)
  (I.ExpIndex arr_itp arr_iexpr ix_itp ix_iexpr) =
  do let arr_i1tp =
           case arr_itp of
             I.TyArr len elem_itp
               | flattenI1Type elem_tp == elem_itp -> ITyArray len elem_tp
             I.TyCArray elem_itp
               | flattenI1Type elem_tp == elem_itp -> ITyCArray elem_tp
             _ ->
               error ("convertExpr: array index at invalid or incorrect type!")
     arr_ptr_expr <- convertExpr (ITyPtr ptr_sort arr_i1tp) arr_iexpr
     ix_expr <- ivoryCoerceSome ITyBits =<< convertSomeExpr ix_itp ix_iexpr
     return $ ptrAdd arr_ptr_expr ix_expr

-- Coercion between types
convertExpr i1tp (I.ExpToIx iexpr bound) =
  -- README: Ivory sometimes seems to automatically convert index types back to
  -- 32-bit signed integer types, so we generalize this and allow any upcast
  -- here
  ivoryCoerce (ITyIndex bound) i1tp =<<
  convertExpr (ITyBits :: I1Type (Literal Int32)) iexpr
convertExpr i1tp (I.ExpSafeCast from_itp iexpr) =
  ivoryCoerceSome i1tp =<< convertSomeExpr from_itp iexpr

-- Comparison operations
convertExpr ITyBool (I.ExpOp (I.ExpEq itp) [ie1, ie2]) =
  convertArithCmp itp OpCmp_EQ ie1 ie2
convertExpr ITyBool (I.ExpOp (I.ExpNeq itp) [ie1, ie2]) =
  applyLitFun1 mkNotBool <$> convertArithCmp itp OpCmp_EQ ie1 ie2
convertExpr ITyBool (I.ExpOp (I.ExpLt leq_flag itp) [ie1, ie2]) =
  convertArithCmp itp (if leq_flag then OpCmp_LE else OpCmp_LT) ie1 ie2
convertExpr ITyBool (I.ExpOp (I.ExpGt leq_flag itp) [ie1, ie2]) =
  convertArithCmp itp (if leq_flag then OpCmp_LE else OpCmp_LT) ie2 ie1

-- Boolean operations
convertExpr i1tp@ITyBool (I.ExpOp I.ExpNot [ie]) =
  applyLitFun1 mkNotBool <$> convertExpr i1tp ie
convertExpr i1tp@ITyBool (I.ExpOp I.ExpAnd [ie1,ie2]) =
  liftM2 (applyLitFun2 mkAndBool) (convertExpr i1tp ie1) (convertExpr i1tp ie2)
convertExpr i1tp@ITyBool (I.ExpOp I.ExpOr [ie1,ie2]) =
  liftM2 (applyLitFun2 mkOrBool) (convertExpr i1tp ie1) (convertExpr i1tp ie2)
convertExpr i1tp (I.ExpOp I.ExpCond [ie1,ie2,ie3]) =
  do e1 <- extractLitExpr <$> convertExpr ITyBool ie1
     e2 <- convertExpr i1tp ie2
     e3 <- convertExpr i1tp ie3
     case convertI1Type i1tp of
       I1ConvLitType lit_tp ->
         return $ applyLitFun2 (mkCond (L1Type_lit lit_tp) e1) e2 e3
       I1ConvPtrType ->
         -- For pointers, we need to duplicate e1, so we bind it to a name
         do n <- bindName ltypeRep (mkReturnP e1)
            let (e2_ptr, e2_ix) = basePtrAndIndex e2
            let (e3_ptr, e3_ix) = basePtrAndIndex e3
            return $
              IPtrExpr (mkCond l1typeRep (mkVar Proxy n) e2_ptr e3_ptr)
              (mkCond l1typeRep (mkVar Proxy n) e2_ix e3_ix)

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
convertExpr i1tp@(convertI1Type ->
                  I1ConvLitType lit_tp) (I.ExpOp I.ExpRecip [ie1]) =
  do e1 <- convertExpr i1tp ie1
     roundIfNeeded i1tp $
       applyLitFun1 (mkArithOp2 lit_tp Op2_Div (convertInteger i1tp 1)) e1

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
convertExpr (ITyPtr _ _) (I.ExpAddrOfGlobal sym) =
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

-- | Convert an Ivory initializer and use it to initialize the memory, of the
-- given type, pointed to by the supplied pointer
convertApplyInit :: I1Type a -> IConvExpr IvoryPtr -> I.Init -> I2LConvM ()
convertApplyInit i1tp ptr I.InitZero =
  case convertI1Type i1tp of
    I1ConvLitType lit_tp ->
      updateArray i1tp ptr $ IExpr lit_tp $
      mkLiteralTp lit_tp (litDefaultTp lit_tp)
    I1ConvPtrType ->
      updateArray i1tp ptr $ IPtrExpr (mkOp Op_null_ptr) 0
    I1ConvObjType ->
      -- zero-initializing a struct or array is a no-op
      return ()
convertApplyInit i1tp ptr (I.InitExpr itp' ie) =
  case gadtType itp' of
    SomeI1Type (i1TypeEq i1tp -> Just Refl) ->
      updateArray i1tp ptr =<< convertExpr i1tp ie
    _ ->
      error "convertApplyInit: init expression type did not match expected type!"
convertApplyInit (ITyStruct s_name) ptr (I.InitStruct f_inits) =
  forM_ f_inits $ \(f_name, f_init) ->
  do (f_itp, f_ix) <- lookupStructFieldM s_name f_name
     case gadtType f_itp of
       SomeI1Type f_i1tp ->
         convertApplyInit f_i1tp (ptrAddI ptr f_ix) f_init
convertApplyInit _ _ (I.InitStruct _) =
  error "convertApplyInit: struct initialization for non-struct type!"
convertApplyInit (ITyArray len elem_i1tp) ptr (I.InitArray init_elems) =
  do if len < length init_elems then
       error "convertApplyInit: array initializer with too many elements!"
       else return ()
     elem_size <- iTypeSizeM $ flattenI1Type elem_i1tp
     forM_ (zip [0 .. ] init_elems) $ \(n, init) ->
       convertApplyInit elem_i1tp (ptrAddI ptr (n * elem_size)) init
convertApplyInit (ITyCArray elem_i1tp) ptr (I.InitArray init_elems) =
  do elem_size <- iTypeSizeM $ flattenI1Type elem_i1tp
     forM_ (zip [0 .. ] init_elems) $ \(n, init) ->
       convertApplyInit elem_i1tp (ptrAddI ptr (n * elem_size)) init
convertApplyInit _ _ (I.InitArray _) =
  error "convertApplyInit: array initialization for non-array type!"


----------------------------------------------------------------------
-- Handling Ivory Requires and Ensures Conditions
----------------------------------------------------------------------

-- | Convert an Ivory condition into a proposition
convertCond :: I.Cond -> I2LConvM (ILExpr Prop)
convertCond (I.CondBool ie) =
  do e <- convertExpr ITyBool ie
     return $ mkIsTrueIvory e
convertCond (I.CondDeref (gadtType -> SomeI1Type i1tp) ie var cond) =
  do ptr <- convertExpr (mkRefType i1tp) ie
     assertPtrOK ptr
     v <- readArray i1tp ptr
     letBindVar var i1tp v
     convertCond cond

-- | Convert a 'Require' clause into a proposition
convertRequire :: I.Require -> I2LConvM (ILExpr Prop)
convertRequire req = convertCond $ I.getRequire req

-- | Convert an 'Ensure' clause into a proposition
convertEnsure :: I.Ensure -> I2LConvM (ILExpr Prop)
convertEnsure req = convertCond $ I.getEnsure req


----------------------------------------------------------------------
-- Converting Ivory statements into Ivory reachability logic
----------------------------------------------------------------------

-- | Convert a block of Ivory statements to a transition relation and add it to
-- the current transition relation
convertBlock :: I.Block -> I2LConvM ()
convertBlock = mapM_ convertStmt

-- | Convert an Ivory statement to a transition relation and add it to the
-- current transition relation
convertStmt :: I.Stmt -> I2LConvM ()

-- If-then-else statements --> disjunctive transition relations
convertStmt (I.IfTE ie stmts1 stmts2) =
  do e <- convertExpr ITyBool ie
     let prop = mkIsTrueIvory e
     pm1 <- collectTransitions $ convertBlock stmts1
     pm2 <- collectTransitions $ convertBlock stmts2
     addTransition $
       mkOrP (mkSeqP (mkAssumeP prop) pm1)
       (mkSeqP (mkAssumeP (mkNot prop)) pm2)

-- Assertions --> errors if the assertion does not hold; i.e., they
-- become @if prop then ('returnP' ()) else raise 'IvoryError'@
convertStmt (I.Assert ie) =
  do e <- convertExpr ITyBool ie
     ivoryAssert (mkIsTrueIvory e)

-- Compiler-inserted assertions, which are the same as normal assertions
convertStmt (I.CompilerAssert ie) =
  get >>= \info ->
  if skipCompilerAsserts (i2l_opts info) then
    return ()
  else
    do e <- convertExpr ITyBool ie
       ivoryAssert (mkIsTrueIvory e)

-- Assumes --> assumptions in the Ivory reachability logic
convertStmt (I.Assume ie) =
  do e <- convertExpr ITyBool ie
     ivoryAssume (mkIsTrueIvory e)

-- Return statements --> write the returned value to the returnValue
-- global variable and then raise an 'IvoryReturn' exception
convertStmt (I.Return typed_ie) =
  do e <- convertTypedExpr typed_ie
     updateArraySome funArgsPtr e
     addTransition $ mkRaiseP IvoryReturn

-- Return statements with no return value --> just raise 'IvoryReturn'
convertStmt I.ReturnVoid = addTransition $ mkRaiseP IvoryReturn

-- Dereference statements --> read a pointer and bind the result to a variable
convertStmt (I.Deref (gadtType -> SomeI1Type i1tp) var ie) =
  do ptr <- convertExpr (mkRefType i1tp) ie
     assertPtrOK ptr
     res_expr <- readArray i1tp ptr
     letBindVar var i1tp res_expr

-- Pointer assignment statements
convertStmt (I.Store (gadtType -> SomeI1Type i1tp) ilhs irhs) =
  do ptr <- convertExpr (mkRefType i1tp) ilhs
     val <- convertExpr i1tp irhs
     updateArray i1tp ptr val

-- Variable assignment statements
convertStmt (I.Assign (gadtType -> SomeI1Type i1tp) var irhs) =
  do val <- convertExpr i1tp irhs
     letBindVar var i1tp val

-- Function calls
convertStmt (I.Call (gadtType -> SomeI1Type ret_i1tp) maybe_ret_var
             (I.NameSym fn_sym) iargs) =
  do p <- lookupProcM fn_sym
     -- Step 1: Convert the arguments, in the context of existing bindings
     args <- mapM convertTypedExpr iargs
     -- We wrap the remaining steps in a reset, since they bind local variables
     -- (in order to assert the pre-/post-conditions)
     pm <- collectTransitions $
       do
         -- Step 2: Clear the existing bindings
         clearVarBindings
         -- Step 3: Let-bind the arguments
         mapM_ (uncurry letBindVarSome) (zip (map I.tValue $ I.procArgs p) args)
         -- Step 4: Assert the preconditions
         mapM_ (ivoryAssert <=< convertRequire) (I.procRequires p)
         -- Step 5: Inline the call or insert an arbitrary transition
         inline_p <- inlineCalls <$> i2l_opts <$> get
         if inline_p then convertBlock (I.procBody p) else
           -- FIXME HERE: add a call here to havoc memory
           error "convertStmt: non-inlined function calls not yet supported"
         -- Step 6: Bind the special variable "retval" to the output
         bindRetval ret_i1tp
         -- Step 7: Assume the postconditions
         mapM_ (ivoryAssume <=< convertEnsure) (I.procEnsures p)
     -- Now add the call as a transition outside the reset
     addTransition pm
     -- Finally, bind the return variable to the return value, if necessary
     case maybe_ret_var of
       Just ret_var ->
         do ret_val <- readArray ret_i1tp funArgsPtr
            letBindVar ret_var ret_i1tp ret_val
       Nothing -> return ()

-- Indirect function calls: not handled yet!
convertStmt (I.Call (gadtType -> SomeI1Type ret_i1tp) maybe_ret_var
             (I.NameVar fn_var) iargs) =
  error "convertStmt: indirect function calls not (yet) handled"

-- Local variable decl --> allocate a pointer and initialize its contents
convertStmt (I.Local (gadtType -> SomeI1Type i1tp) var init) =
  do size <- iTypeSizeM $ flattenI1Type i1tp
     -- Allocate memory for the new object
     ptr <- allocateArray $ mkLiteral $ fromIntegral size
     -- Initialize the new object using init
     convertApplyInit i1tp ptr init
     -- Now let-bind var to the allocated ptr
     letBindVar var (mkRefType i1tp) ptr

-- Reference copy: do a shallow copy of the array pointed to by irhs to ilhs
convertStmt (I.RefCopy (gadtType -> SomeI1Type i1tp) ilhs irhs) =
  do (rhs_ptr, rhs_ix) <- basePtrAndIndex <$> convertExpr (mkRefType i1tp) irhs
     (lhs_ptr, lhs_ix) <- basePtrAndIndex <$> convertExpr (mkRefType i1tp) ilhs
     error "FIXME HERE: update ref copy to handle offsets"
     --addTransition $ mkOp (Op_updateP UpdateOp_ptr_copy) ptr_rhs ptr_lhs

-- Reference allocation: this is essentially the identity in the logic (its
-- actual use in Ivory is to convert C lvalues into C rvalues)
convertStmt (I.AllocRef (gadtType -> SomeI1Type i1tp) var nm) =
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
     updateArray i1tp loopCounterPtr start
     -- Extract the loop bound and whether the loop is an increment loop
     let (ibound, loop_is_incr_p) =
           case incr of
             I.IncrTo ibound -> (ibound, True)
             I.DecrTo ibound -> (ibound, False)
     -- Convert the loop bound into a logical expression
     loop_bound <- extractLitExpr <$> convertExpr i1tp ibound
     -- Convert body (inside a reset, as it binds vars, raises exns, etc.)
     body_pm <- collectTransitions $ do
       -- Read the loop counter into var
       ctr_val <- readArray i1tp loopCounterPtr
       let ctr_val_e = extractLitExpr ctr_val
       letBindVar var i1tp ctr_val
       -- Run the loop test, raising IvoryBreak if it fails
       let loop_test =
             mkIsTrue $
             if loop_is_incr_p then mkLtBool ctr_val_e loop_bound
             else mkLtBool loop_bound ctr_val_e
       addTransition $
         mkOrP (mkSeqP (mkAssumeP loop_test) (mkReturnP $ mkLiteral ()))
         (mkSeqP (mkAssumeP (mkNot loop_test)) (mkRaiseP IvoryBreak))
       -- Run the body
       convertBlock body
       -- Compute the new counter value (NOTE: we don't re-read from
       -- loopCounterPtr in case a sub-computation contains a loop)
       let upd_op = if loop_is_incr_p then Op1_Incr else Op1_Decr
       let ctr_val_e' = mkOp (Op_arith1 litTypeRep upd_op) ctr_val_e
       updateArray i1tp loopCounterPtr $ IExpr litTypeRep ctr_val_e'

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
convertProc :: I.Proc -> I2LConvM ILPM
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
{-
    args_len <- addBindTransition $ mkOp (Op_readP ReadOp_length) funArgsPtr
    ivoryAssume $ mkIsTrue $
      mkLeBool (mkLiteral64 $ length $ I.procArgs p) args_len
-}

    -- Read each variable from funArgsPtr
    readArgs 0 $ I.procArgs p

    -- Assume all the Requires
    mapM_ (ivoryAssume <=< convertRequire) (I.procRequires p)
    -- Transition according to the body of the procedure, catching any returns
    ivoryCatch IvoryReturn (convertBlock (I.procBody p))

    -- Bind the return value to the special Ivory "retval" variable
    case gadtType (I.procRetTy p) of
      SomeI1Type i1tp -> bindRetval i1tp

    -- Finally, assert all the Ensures
    mapM_ (ivoryAssert <=< convertEnsure) $ I.procEnsures p
      where
        readArgs _ [] = return ()
        readArgs ix (arg : args) =
          do arg_size <- iTypeSizeM $ I.tType arg
             arg_val <- readArraySome (I.tType arg) (ptrAddI funArgsPtr ix)
             letBindVarSome (I.tValue arg) arg_val
             readArgs (ix + arg_size) args


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
