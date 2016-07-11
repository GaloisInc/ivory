{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeFamilies, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving #-}

module Ivory.ModelCheck.Ivory2Logic where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Bits
import Data.Word
import Data.Int
import Data.List
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
    unrollLoops :: Bool,
    -- ^ Whether to unroll loops in the SMT checker
    inlineCalls :: Bool
    -- ^ Whether to inline function calls
  }

-- | Default options for the Ivory reachability solver
defaultOpts = ILOpts { debugLevel = 0, skipCompilerAsserts = False,
                       unrollLoops = False, inlineCalls = False }


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
  type LStorables IvoryLogic = '[ Word64 ]
  type LException IvoryLogic = IvoryExn

-- | The type of a 'Memory' used by Ivory
type IvoryMemory = Memory '[ Word64 ]


----------------------------------------------------------------------
-- Converting Ivory types into Ivory reachability logic
----------------------------------------------------------------------

-- | Some existentially-quantified first-order type
data SomeL1Type where
  SomeL1Type :: L1Type a -> SomeL1Type

-- | Helper for converting Ivory types to first-order types
mkSomeLitType :: LitType a -> SomeL1Type
mkSomeLitType lit_tp = SomeL1Type $ L1Type_lit lit_tp

-- | Helper for building the 'Ptr' type
somePtrType :: SomeL1Type
somePtrType = SomeL1Type L1Type_ptr

-- | Convert an Ivory type into Ivory reachability logic
convertType :: I.Type -> SomeL1Type
convertType I.TyVoid = mkSomeLitType (litTypeRep :: LitType ())
convertType (I.TyInt I.Int8) = mkSomeLitType (litTypeRep :: LitType Int8)
convertType (I.TyInt I.Int16) = mkSomeLitType (litTypeRep :: LitType Int16)
convertType (I.TyInt I.Int32) = mkSomeLitType (litTypeRep :: LitType Int32)
convertType (I.TyInt I.Int64) = mkSomeLitType (litTypeRep :: LitType Int64)
convertType (I.TyWord I.Word8) = mkSomeLitType (litTypeRep :: LitType Word8)
convertType (I.TyWord I.Word16) = mkSomeLitType (litTypeRep :: LitType Word16)
convertType (I.TyWord I.Word32) = mkSomeLitType (litTypeRep :: LitType Word32)
convertType (I.TyWord I.Word64) = mkSomeLitType (litTypeRep :: LitType Word64)
convertType (I.TyIndex upper_bound) =
  mkSomeLitType (litTypeRep :: LitType Word64)
convertType I.TyBool = mkSomeLitType (litTypeRep :: LitType Bool)
convertType I.TyChar = error "convertType: cannot (yet) handle Char type"
convertType I.TyFloat = error "convertType: cannot (yet) handle Float type"
convertType I.TyDouble = error "convertType: cannot (yet) handle Double type"
convertType (I.TyProc out_type in_types) =
  error "convertType: cannot (yet) handle function types"
convertType (I.TyRef _) = somePtrType
convertType (I.TyConstRef _) = somePtrType
convertType (I.TyPtr _) = somePtrType
convertType (I.TyArr _ _) = somePtrType
convertType (I.TyStruct _) = somePtrType
convertType (I.TyCArray _) = somePtrType
convertType I.TyOpaque =
  error "convertType: cannot (yet) handle opaque types"


----------------------------------------------------------------------
-- Monad for converting Ivory expressions into reachability logic
----------------------------------------------------------------------

-- | An Ivory variable that has been converted into Ivory reachability
-- logic, by associating it with a base type and a variable name
data SomeILName where SomeILName :: L1Type a -> Name a -> SomeILName

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
    i2l_syms :: Map.Map I.Sym Natural
    -- ^ An association from Ivory global symbols and unique numbers for them
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

-- | FIXME: documentation!
addTransition :: ILPM -> I2LM ()
addTransition pm = shiftPureM $ \k -> mkSeqP pm (k ())

-- | FIXME: documentation!
addBindTransition :: L1Typeable a => ILExpr (PM a) -> I2LM (ILExpr a)
addBindTransition pm = shiftPureM $ \k -> mkBindP pm k

-- | Add an assumption to the current transition relation
ivoryAssume :: ILProp -> I2LM ()
ivoryAssume prop = addTransition $ mkAssumeP prop

-- | Add an assertion to the current transition relation
ivoryAssert :: ILProp -> I2LM ()
ivoryAssert prop = addTransition $ mkAssertP IvoryError prop

-- | The number of reserved / "special" global variable numbers
numReservedSyms = 1

-- | A pointer expression used to store function arguments and return values
funArgsPtr :: ILExpr Ptr
funArgsPtr = mkOp (Op_global_var 1)

-- | Helper function to return the unit transition
returnUnitTrans :: I2LM ILPM
returnUnitTrans = return $ mkReturnP $ mkLiteral ()

-- | Get all the global variable symbols in a module
moduleGlobalSyms :: I.Module -> Set.Set I.Sym
moduleGlobalSyms mod =
  Set.unions [Set.fromList $ map I.externSym $ I.modExterns mod]
-- FIXME HERE: Handle Areas and AreaImports?

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
getVar :: L1Type a -> I.Var -> I2LM (Name a)
getVar l1tp v =
  do maybe_some_name <- lookupVar v
     case maybe_some_name of
       Nothing -> error ("getVar: could not find Ivory variable " ++ show v)
       Just (SomeILName l1tp' n) ->
         case l1TypeEq l1tp l1tp' of
           Just Refl -> return n
           Nothing ->
             error ("getVar: Ivory variable " ++ show v ++ " has the wrong type")

-- | Get the logical variable associated with an Ivory variable, at a specific
-- type, and convert it to a logical expression
getVarExpr :: L1Type a -> I.Var -> I2LM (ILExpr a)
getVarExpr l1tp var = liftM (mkVarExprTp $ LType_base l1tp) $ getVar l1tp var

-- | Apply a name-binding operation to the output transition relation
-- of an 'I2LM' computation, and return the bound name
bindName :: ((Name a -> ILPM) -> ILPM) -> I2LM (Name a)
bindName = shiftPureM

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

-- | Apply a name-binding operation to the output transition relation
-- of an 'I2LM' computation, and associate the bound name with an
-- Ivory variable
bindVar :: I.Var -> L1Type a -> ((Name a -> ILPM) -> ILPM) -> I2LM ()
bindVar var l1tp f =
  do n <- bindName f
     linkVarAndName var (SomeILName l1tp n)

-- | Bind an Ivory variable to the result of a computation @pm@, and
-- store this binding in the 'I2LInfo' transition-relation-building
-- state. The binding is done by building the expression @pm >>= k@,
-- where @k@ is the continuation of the rest of the expression being
-- built.
pmBindVar :: I.Var -> L1Type a -> ILExpr (PM a) -> I2LM ()
pmBindVar var l1tp pm =
  bindVar var l1tp $ \k ->
  mkOp (Op_bindP l1tp l1typeRep) pm $
  LLambda (LType_base l1tp) $ nu k

-- | Let-bind an Ivory variable to a value, and store this binding in
-- the 'I2LInfo' transition-relation-building state. The let-binding
-- is done by building the expression @(return x) >>= k@, where @k@ is
-- the continuation of the rest of the expression being built.
letBindVar :: I.Var -> L1Type a -> ILExpr a -> I2LM ()
letBindVar var l1tp rhs = pmBindVar var l1tp (mkReturnP_tp l1tp rhs)

-- | Let-bind an Ivory variable to a value of unknown type (see 'letBindVar')
letBindVarSome :: I.Var -> SomeILExpr -> I2LM ()
letBindVarSome var (SomeILExpr l1tp e) = letBindVar var l1tp e

-- | Similar to 'letBindVar', except the given Ivory variable is
-- existentially-bound, meaning no explicit value is given for it.
exBindVar :: I.Var -> L1Type a -> I2LM ()
exBindVar var l1tp = pmBindVar var l1tp (mkExistsP_tp l1tp)

-- | Similar to 'exBindVar', but where the type of the variable is given as an
-- Ivory type, using the 'I.Typed' construction
exBindTypedVar :: I.Typed I.Var -> I2LM ()
exBindTypedVar typed_var =
  case convertType (I.tType typed_var) of
    SomeL1Type l1tp -> exBindVar (I.tValue typed_var) l1tp


----------------------------------------------------------------------
-- Ivory global symbols
----------------------------------------------------------------------

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
  bindName $ \k ->
  mkOp (Op_bindP l1typeRep l1typeRep)
  (mkOp (Op_readP (ReadOp_array Elem_base)) ptr ix) $
  LLambda ltypeRep $ nu k

-- | Similar to 'readArrayNameWord64', but read a pointer value instead
readArrayNamePtr :: ILExpr Ptr -> ILExpr (Literal Word64) -> I2LM (Name Ptr)
readArrayNamePtr ptr ix =
  bindName $ \k ->
  mkOp (Op_bindP l1typeRep l1typeRep)
  (mkOp (Op_readP ReadOp_ptr_array) ptr ix) $
  LLambda ltypeRep $ nu k

-- | Read an array value, binding the result to a fresh local name and coercing
-- the name if necessary. Also assert that the given pointer is valid, and that
-- th eindex is within the length bounds of the array that is pointed to.
readArray :: L1Type a -> ILExpr Ptr -> ILExpr (Literal Word64) ->
             I2LM (ILExpr a)
readArray l1tp@(L1Type_lit lit_tp) ptr ix =
  do n <- readArrayNameWord64 ptr ix
     return $ mkCoerce litTypeRep lit_tp $ mkVar Proxy n
readArray l1tp@L1Type_ptr ptr ix =
  do n <- readArrayNamePtr ptr ix
     return $ mkVarExprTp (LType_base l1tp) n
readArray L1Type_prop ptr ix =
  error "readArray: attempt to read a proposition from memory!"

-- | Perform a 'readArray' using an Ivory type
readArraySome :: I.Type -> ILExpr Ptr -> ILExpr (Literal Word64) ->
                 I2LM SomeILExpr
readArraySome itp ptr ix =
  case convertType itp of
    SomeL1Type l1tp -> SomeILExpr l1tp <$> readArray l1tp ptr ix

-- | Update an Ivory array value
updateArray :: L1Type a -> ILExpr Ptr -> ILExpr (Literal Word64) -> ILExpr a ->
               I2LM ()
updateArray (L1Type_lit lit_tp) ptr ix v =
  addTransition $
  mkOp (Op_updateP $ UpdateOp_array Elem_base) ptr ix $
  -- NOTE: this is to convert v to a Word64 expression, as that is
  -- what our memory model actually stores in Ivory
  mkCoerce lit_tp litTypeRep v
updateArray L1Type_ptr ptr ix v =
  addTransition $
  mkOp (Op_updateP UpdateOp_ptr_array) ptr ix v
updateArray L1Type_prop ptr ix v =
  error "updateArray: attempt to write a proposition to memory!"

-- | Perform an 'updateArray' with a value of an unknown type
updateArraySome :: ILExpr Ptr -> ILExpr (Literal Word64) -> SomeILExpr ->
                   I2LM ()
updateArraySome ptr ix (SomeILExpr l1tp v) = updateArray l1tp ptr ix v

-- | Allocate an Ivory array of a given length, returning a pointer to it
allocateArray :: ILExpr (Literal Word64) -> I2LM (ILExpr Ptr)
allocateArray len =
  addBindTransition $
  mkSeqP (mkOp (Op_updateP UpdateOp_alloc) len) $
  mkOp (Op_readP ReadOp_last_alloc)

-- | Let-bind the special Ivory variable "retval" to the result of a function
-- call, by reading this result from the current memory
bindRetval :: I.Type -> I2LM ()
bindRetval I.TyVoid =
  -- Special case: don't bind a "void"-typed variable
  return ()
bindRetval itp =
  do ret <- readArraySome itp funArgsPtr (mkLiteral 0)
     letBindVarSome I.retval ret


----------------------------------------------------------------------
-- Looking up Ivory information from Ivory modules
----------------------------------------------------------------------

-- | Look up a named value in a list of modules or signal an error
moduleLookup :: (Show key, Eq key) => (I.Module -> I.Visible res) ->
                (res -> key) -> key -> String -> [I.Module] -> res
moduleLookup readMod readKey key res_name mods =
  let pubElems = concatMap (I.public . readMod) mods in
  case find ((==) key . readKey) pubElems of
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
  SomeILExpr :: L1Type a -> ILExpr a -> SomeILExpr

-- | Coerce an expression of unknown type to a definite type, raising the given
-- error message if the coercion is not possible
coerceSomeExpr :: L1Type a -> String -> SomeILExpr -> ILExpr a
coerceSomeExpr (L1Type_lit to_tp) _ (SomeILExpr (L1Type_lit from_tp) e) =
  mkCoerce from_tp to_tp e
coerceSomeExpr L1Type_ptr _ (SomeILExpr L1Type_ptr e) = e
coerceSomeExpr L1Type_prop _ (SomeILExpr L1Type_prop e) = e
coerceSomeExpr _ err_str _ = error err_str

-- | Convert an 'Integer' to a logical expression of a given type
convertInteger :: L1Type a -> Integer -> ILExpr a
convertInteger (L1Type_lit LitType_int) i = mkLiteral i
convertInteger (L1Type_lit lit_tp@LitType_bits) i =
  mkLiteralTp lit_tp $ fromInteger i
convertInteger (L1Type_lit LitType_unit) _ =
  error "convertInteger: unit type!"
convertInteger (L1Type_lit LitType_bool) _ =
  error "convertInteger: Boolean type!"
convertInteger _ i =
  error "convertInteger: pointer or proposition type!"

-- | Convert an Ivory literal to a logical expression
convertLiteral :: L1Type a -> I.Literal -> ILExpr a
convertLiteral l1tp (I.LitInteger i) = convertInteger l1tp i
convertLiteral _ (I.LitFloat x) =
  error "convertLiteral: floats not (yet) supported"
convertLiteral _ (I.LitDouble x) =
  error "convertLiteral: floats not (yet) supported"
convertLiteral _ (I.LitChar c) =
  error "convertLiteral: characters not (yet) supported"
convertLiteral (L1Type_lit LitType_bool) (I.LitBool b) =
  mkLiteral b
convertLiteral L1Type_ptr I.LitNull = mkOp Op_null_ptr
convertLiteral _ (I.LitString str) =
  error "convertLiteral: strings not (yet) supported"
convertLiteral _ _ =
  error "convertLiteral: literal used at incorrect type"

-- | Helper function to convert Ivory arithmetic comparisons
convertArithCmp :: I.Type -> ArithCmp -> [I.Expr] ->
                   I2LM (ILExpr (Literal Bool))
convertArithCmp itp acmp args =
  case (convertType itp, acmp, args) of
    (SomeL1Type l1tp_sub@(L1Type_lit lit_tp), _, [ie1, ie2]) ->
      do e1 <- convertExpr l1tp_sub ie1
         e2 <- convertExpr l1tp_sub ie2
         return $ mkOp (Op_cmp lit_tp acmp) e1 e2
    (SomeL1Type l1tp_sub@L1Type_ptr, OpCmp_EQ, [ie1, ie2]) ->
      do e1 <- convertExpr l1tp_sub ie1
         e2 <- convertExpr l1tp_sub ie2
         return $ mkOp (Op_ptr_cmp OpCmp_EQ) e1 e2
    (SomeL1Type l1tp_sub@L1Type_ptr, _, _) ->
      error "convertArithCmp: inequality comparison of pointers!"
    (SomeL1Type l1tp_sub@L1Type_prop, _, _) ->
      error "convertArithCmp: comparison of propositions!"
    _ -> error "convertArithCmp: comparison on more or fewer than 2 arguments!"

-- | Convert an Ivory expression to a logical expression using an Ivory type
convertSomeExpr :: I.Type -> I.Expr -> I2LM SomeILExpr
convertSomeExpr itp ie =
  case convertType itp of
    SomeL1Type l1tp -> SomeILExpr l1tp <$> convertExpr l1tp ie

-- | Convert a typed Ivory expression to a logical expression
convertTypedExpr :: I.Typed I.Expr -> I2LM SomeILExpr
convertTypedExpr typed_ie =
  convertSomeExpr (I.tType typed_ie) (I.tValue typed_ie)

-- | Convert an Ivory expression to a logical expression of a specific type
convertExpr :: L1Type a -> I.Expr -> I2LM (ILExpr a)

-- Symbols and variables
convertExpr l1tp (I.ExpSym sym) =
  do ptr_expr <- convertSymbol sym
     readArray l1tp ptr_expr (mkLiteral 0)
convertExpr l1tp (I.ExpExtern ext) =
  do ptr_expr <- convertSymbol (I.externSym ext)
     readArray l1tp ptr_expr (mkLiteral 0)
convertExpr l1tp (I.ExpVar v) = getVarExpr l1tp v

-- Literals
convertExpr l1tp (I.ExpLit lit) =
  return $ convertLiteral l1tp lit

-- Array and struct dereferencing
convertExpr l1tp (I.ExpLabel (I.TyStruct s_name) s_iexpr f_name) =
  do s_expr <- convertExpr L1Type_ptr s_iexpr
     (_, f_ix) <- lookupStructFieldM s_name f_name
     readArray l1tp s_expr (mkLiteral $ fromInteger $ toInteger f_ix)
convertExpr l1tp (I.ExpIndex arr_elem_itp arr_iexpr ix_itp ix_iexpr) =
  do ix_expr <-
       coerceSomeExpr l1typeRep "convertExpr: index of non-numeric type!" <$>
       convertSomeExpr ix_itp ix_iexpr
     arr_expr <- convertExpr L1Type_ptr arr_iexpr
     readArray l1tp arr_expr ix_expr

-- Coercion between types
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpToIx iexpr modulus) =
  do e <- convertExpr l1tp iexpr
     return $ mkOp (Op_arith2 lit_tp Op2_Mod) e (convertInteger l1tp modulus)
convertExpr l1tp (I.ExpSafeCast from_itp iexpr) =
  coerceSomeExpr l1tp "convertExpr: disallowed cast!" <$>
  convertSomeExpr from_itp iexpr

-- Comparison operations
convertExpr (L1Type_lit LitType_bool) (I.ExpOp (I.ExpEq itp) args) =
  convertArithCmp itp OpCmp_EQ args
convertExpr (L1Type_lit LitType_bool) (I.ExpOp (I.ExpNeq itp) args) =
  liftM mkNotBool $ convertArithCmp itp OpCmp_EQ args
convertExpr (L1Type_lit LitType_bool) (I.ExpOp (I.ExpLt leq_flag itp) args) =
  convertArithCmp itp (if leq_flag then OpCmp_LE else OpCmp_LT) args
convertExpr (L1Type_lit LitType_bool) (I.ExpOp (I.ExpGt leq_flag itp) [ie1,ie2]) =
  convertArithCmp itp (if leq_flag then OpCmp_LE else OpCmp_LT) [ie2,ie1]

-- Boolean operations
convertExpr l1tp@(L1Type_lit LitType_bool) (I.ExpOp I.ExpNot [ie]) =
  liftM mkNotBool $ convertExpr l1tp ie
convertExpr l1tp@(L1Type_lit LitType_bool) (I.ExpOp I.ExpAnd [ie1,ie2]) =
  liftM2 mkAndBool (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit LitType_bool) (I.ExpOp I.ExpOr [ie1,ie2]) =
  liftM2 mkOrBool (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit _) (I.ExpOp I.ExpCond [ie1,ie2,ie3]) =
  do e1 <- convertExpr (L1Type_lit LitType_bool) ie1
     e2 <- convertExpr l1tp ie2
     e3 <- convertExpr l1tp ie3
     return $ mkOp (Op_cond l1tp) e1 e2 e3
convertExpr l1tp@L1Type_ptr (I.ExpOp I.ExpCond [ie1,ie2,ie3]) =
  do e1 <- convertExpr (L1Type_lit LitType_bool) ie1
     e2 <- convertExpr l1tp ie2
     e3 <- convertExpr l1tp ie3
     return $ mkOp (Op_cond l1tp) e1 e2 e3

-- Arithmetic operations
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpMul [ie1,ie2]) =
  liftM2 (mkArithOp2 lit_tp Op2_Mult)
  (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpAdd [ie1,ie2]) =
  liftM2 (mkArithOp2 lit_tp Op2_Add)
  (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpSub [ie1,ie2]) =
  liftM2 (mkArithOp2 lit_tp Op2_Sub)
  (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpNegate [ie1]) =
  liftM (mkArithOp1 lit_tp Op1_Neg) $ convertExpr l1tp ie1
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpAbs [ie1]) =
  liftM (mkArithOp1 lit_tp Op1_Abs) $ convertExpr l1tp ie1
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpSignum [ie1]) =
  liftM (mkArithOp1 lit_tp Op1_Signum) $ convertExpr l1tp ie1
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpDiv [ie1,ie2]) =
  liftM2 (mkArithOp2 lit_tp Op2_Div)
  (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpMod [ie1,ie2]) =
  liftM2 (mkArithOp2 lit_tp Op2_Mod)
  (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpRecip [ie1]) =
  liftM (\e -> mkArithOp2 lit_tp Op2_Div (convertInteger l1tp 1) e) $
  convertExpr l1tp ie1

-- Floating-point operations (FIXME!)

-- Bitwise operations
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpBitAnd [ie1,ie2]) =
  liftM2 (mkArithOp2 lit_tp Op2_BitAnd)
  (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpBitOr [ie1,ie2]) =
  liftM2 (mkArithOp2 lit_tp Op2_BitOr)
  (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpBitXor [ie1,ie2]) =
  liftM2 (mkArithOp2 lit_tp Op2_BitXor)
  (convertExpr l1tp ie1) (convertExpr l1tp ie2)
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpOp I.ExpBitComplement [ie1]) =
  liftM (mkArithOp1 lit_tp Op1_Complement) $ convertExpr l1tp ie1
-- FIXME: bit-shifting operations!

-- Getting the address of a global
convertExpr L1Type_ptr (I.ExpAddrOfGlobal sym) =
  convertSymbol sym

-- Max/min of a type, and sizeof a type
convertExpr l1tp (I.ExpMaxMin is_max) =
  -- FIXME HERE: implement this!
  error "convertExpr: max and min not yet implemented!"
convertExpr l1tp (I.ExpSizeOf itp) =
  -- FIXME HERE: implement this!
  error "convertExpr: sizeof not yet implemented!"
convertExpr l1tp e =
  error ("convertExpr: could not convert expression: " ++ show e)


----------------------------------------------------------------------
-- Converting Ivory initialization expressions into logic
----------------------------------------------------------------------

-- | Convert an Ivory initializer into a logical expression. Struct and array
-- initializers become pointers.
convertInit :: I.Type -> I.Init -> I2LM SomeILExpr
convertInit itp I.InitZero =
  return $
  case convertType itp of
    SomeL1Type l1tp@(L1Type_lit lit_tp) ->
       SomeILExpr l1tp $ mkLiteralTp lit_tp (litDefaultTp lit_tp)
    SomeL1Type L1Type_ptr -> SomeILExpr L1Type_ptr $ mkOp Op_null_ptr
    SomeL1Type L1Type_prop -> error "convertInit: local var of type Prop!"
convertInit itp (I.InitExpr itp' ie) =
  if itp == itp' then
    convertSomeExpr itp ie
  else
    error "convertInit: init expression type did not match expected type!"
convertInit (I.TyStruct s_name) (I.InitStruct init_flds) =
  do flds <-
       forM init_flds
       (\(f_name,init) ->
         do (f_itp, f_ix) <- lookupStructFieldM s_name f_name
            e <- convertInit f_itp init
            return (f_ix, e))
     ptr <- allocateArray $ mkLiteral64 $ 1 + maximum (map fst flds)
     forM_ flds
       (\(f_ix, SomeILExpr l1tp e) ->
         updateArray l1tp ptr (mkLiteral64 f_ix) e)
     return $ SomeILExpr L1Type_ptr ptr
convertInit _ (I.InitStruct _) =
  error "convertInit: struct initialization for non-struct type!"
convertInit arr_itp (I.InitArray init_elems) =
  do let (len, itp) = case arr_itp of
           I.TyArr len itp -> (len, itp)
           I.TyCArray itp -> (length init_elems, itp)
           _ -> error "convertInit: array initialization for non-array type!"
     elems <- mapM (convertInit itp) init_elems
     ptr <- allocateArray $ mkLiteral64 (1 + len)
     forM_ (zip elems [1 .. len])
       (\(SomeILExpr l1tp e, ix) -> updateArray l1tp ptr (mkLiteral64 ix) e)
     return $ SomeILExpr L1Type_ptr ptr


----------------------------------------------------------------------
-- Handling Ivory Requires and Ensures Conditions
----------------------------------------------------------------------

-- | Convert an Ivory condition into a proposition
convertCond :: I.Cond -> I2LM (ILExpr Prop)
convertCond (I.CondBool ie) =
  do e <- convertExpr l1typeRep ie
     return $ mkIsTrue e
convertCond (I.CondDeref itp ie_ptr var cond) =
  do ptr <- convertExpr L1Type_ptr ie_ptr
     ptr_val <- readArraySome itp ptr (mkLiteral 0)
     letBindVarSome var ptr_val
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
  do e <- convertExpr l1typeRep ie
     let prop = mkIsTrue e
     pm1 <- resetPureM $ convertBlock stmts1 >> returnUnitTrans
     pm2 <- resetPureM $ convertBlock stmts2 >> returnUnitTrans
     addTransition $
       mkOrP (mkSeqP (mkAssumeP prop) pm1)
       (mkSeqP (mkAssumeP (mkNot prop)) pm2)

-- Assertions --> errors if the assertion does not hold; i.e., they
-- become @if prop then ('returnP' ()) else raise 'IvoryError'@
convertStmt (I.Assert ie) =
  do e <- convertExpr l1typeRep ie
     ivoryAssert (mkIsTrue e)

-- Compiler-inserted assertions, which are the same as normal assertions
convertStmt (I.CompilerAssert ie) =
  get >>= \info ->
  if skipCompilerAsserts (i2l_opts info) then
    return ()
  else
    do e <- convertExpr l1typeRep ie
       ivoryAssert (mkIsTrue e)

-- Assumes --> assumptions in the Ivory reachability logic
convertStmt (I.Assume ie) =
  do e <- convertExpr l1typeRep ie
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
convertStmt (I.Deref itp var ie) =
  do ptr_expr <- convertExpr L1Type_ptr ie
     res_expr <- readArraySome itp ptr_expr (mkLiteral 0)
     letBindVarSome var res_expr

-- Assignment statements
convertStmt (I.Store itp ilhs irhs) =
  do ptr <- convertExpr L1Type_ptr ilhs
     val <- convertSomeExpr itp irhs
     updateArraySome ptr (mkLiteral 0) val

-- Assignment statements
convertStmt (I.Assign itp var irhs) =
  do val <- convertSomeExpr itp irhs
     letBindVarSome var val

-- Function calls
convertStmt (I.Call ret_itp maybe_ret_var (I.NameSym fn_sym) iargs) =
  do p <- lookupProcM fn_sym
     -- We wrap the actual call in a reset, since it binds local variables (in
     -- order to assert the pre-/post-conditions)
     pm <- resetPureM $
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
         bindRetval ret_itp
         -- Step 6: Assume the postconditions
         mapM_ (ivoryAssert <=< convertEnsure) (I.procEnsures p)
         returnUnitTrans
     -- Now add the call as a transition outside the reset
     addTransition pm
     -- Finally, bind the return variable to the return value, if necessary
     case maybe_ret_var of
       Just ret_var ->
         do ret_val <- readArraySome ret_itp funArgsPtr (mkLiteral 0)
            letBindVarSome ret_var ret_val
       Nothing -> return ()

-- Indirect function calls: not handled yet!
convertStmt (I.Call ret_itp maybe_ret_var (I.NameVar fn_var) iargs) =
  error "convertStmt: indirect function calls not (yet) handled"

-- Local variable decl --> allocation of a new reference + initialization
convertStmt (I.Local itp var init) =
  do some_expr <- convertInit itp init
     ptr <-
       case some_expr of
         SomeILExpr l1tp@(L1Type_lit lit_tp) e ->
           do ptr <- allocateArray (mkLiteral 1)
              updateArray l1tp ptr (mkLiteral 0) e
              return ptr
         SomeILExpr L1Type_ptr e -> return e
         SomeILExpr L1Type_prop e ->
           error "convertStmt: local variable of Prop type!"
     letBindVar var L1Type_ptr ptr

-- Reference copy: do a shallow copy of the array pointed to by irhs to ilhs
convertStmt (I.RefCopy _ ilhs irhs) =
  do ptr_rhs <- convertExpr L1Type_ptr irhs
     ptr_lhs <- convertExpr L1Type_ptr ilhs
     addTransition $ mkOp (Op_updateP UpdateOp_ptr_copy) ptr_rhs ptr_lhs

-- Reference allocation: this is essentially the identity in the logic (its
-- actual use in Ivory is to convert C lvalues into C rvalues)
convertStmt (I.AllocRef itp var nm) =
  do ptr <-
       case nm of
         I.NameVar var' -> getVarExpr l1typeRep var'
         I.NameSym sym -> convertSymbol sym
     letBindVar var l1typeRep ptr

-- Loops --> not handled yet
convertStmt (I.Loop max_iters var start incr body) =
  error "convertStmt: loops not yet handled!" -- FIXME HERE

-- Loops --> not handled yet
convertStmt (I.Forever body) =
  error "convertStmt: loops not yet handled!" -- FIXME HERE

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
  resetPureM $
  do
    -- Print the proc if debugging is on
    debug_level <- debugLevel <$> i2l_opts <$> get
    if debug_level >= 3 then
      traceM ("Converting proc:\n" ++ show (I.procBody p)) else return ()
    -- Assume that the length of funArgsPtr is as big as needed
    args_len <- addBindTransition $ mkOp (Op_readP ReadOp_length) funArgsPtr
    ivoryAssume $ mkIsTrue $
      mkLtBool (mkLiteral64 $ length $ I.procArgs p) args_len
    -- Read each variable from funArgsPtr
    forM_ (zip (I.procArgs p) [0 ..]) $ \(tv, ix) ->
      do arg_val <- readArraySome (I.tType tv) funArgsPtr (mkLiteral ix)
         letBindVarSome (I.tValue tv) arg_val
    -- Assume all the Requires
    mapM_ (ivoryAssume <=< convertRequire) (I.procRequires p)
    -- Transition according to the body of the procedure
    convertBlock (I.procBody p)
    -- Bind the return value to the special Ivory "retval" variable
    bindRetval (I.procRetTy p)
    -- Assert all the Ensures
    mapM_ (ivoryAssert <=< convertEnsure) $ I.procEnsures p
    returnUnitTrans


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
