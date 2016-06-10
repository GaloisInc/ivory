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

import Data.Map.Strict as Map

import MonadLib
import MonadLib.Monads

import Data.Binding.Hobbits
import Ivory.ModelCheck.Logic

import qualified Ivory.Language.Syntax.Names as I
import qualified Ivory.Language.Syntax.Type as I
import qualified Ivory.Language.Syntax.AST as I


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

instance Closable IvoryExn where
  toClosed IvoryError = $(mkClosed [| IvoryError |])
  toClosed IvoryBreak = $(mkClosed [| IvoryBreak |])
  toClosed IvoryReturn = $(mkClosed [| IvoryReturn |])

-- Gives the exception type and memory model for the Ivory reachability logic
instance LExprTag IvoryLogic where
  type LStorables IvoryLogic = '[ Word64 ]
  type LException IvoryLogic = IvoryExn


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
    i2l_modules :: [I.Module],
    -- ^ The Ivory modules that are in scope
    i2l_vars :: Map.Map I.Var SomeILName,
    -- ^ The Ivory variables that are in scope, and the Ivory
    -- reachability logic variables associated with them
    i2l_syms :: Map.Map I.Sym Integer,
    -- ^ The Ivory global symbols that that have been seen so far, and
    -- unique numbers that have been associated with them
    i2l_next_sym_int :: Integer
    -- ^ The number to be used for the next symbol we see
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
class Monad m => ShiftPureM m r | m -> r where
  shiftPureM :: ((a -> r) -> r) -> m a

instance ShiftPureM (ContT r Id) r where
  shiftPureM f =
    callCC $ \cc ->
    let k = runId . runContT return . cc
    in abort =<< return (f k)

instance ShiftPureM m r => ShiftPureM (StateT i m) r where
  shiftPureM f =
    do s <- get
       liftM fst $ lift $ shiftPureM $ \k -> f (\x -> k (x,s))

instance ShiftPureM I2LM ILPM where
  shiftPureM f = I2LM $ shiftPureM f

-- | FIXME: documentation!
addTransition :: ILPM -> I2LM ()
addTransition pm = shiftPureM $ \k -> mkSeqP pm (k ())

-- | FIXME: documentation!
addBindTransition :: L1Typeable a => ILExpr (PM a) -> I2LM (ILExpr a)
addBindTransition pm = shiftPureM $ \k -> mkBindP pm k

-- | The number of reserved / "special" global variable numbers
numReservedSyms = 1

-- | A pointer expression used to store return values
returnValuePtr :: ILExpr Ptr
returnValuePtr = mkOp (Op_global_var 1)

-- An I2LM computation can be "run" by giving it Ivory modules and a
-- default continuation (which is normally just "return")
instance RunM I2LM a ([I.Module] -> (a -> ILPM) -> ILPM) where
  runM (I2LM m) mods k =
    runM m (I2LInfo { i2l_modules = mods,
                      i2l_vars = Map.empty,
                      i2l_syms = Map.empty,
                      i2l_next_sym_int = numReservedSyms + 1 })
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

-- | Similar to 'letBindVar', except the given Ivory variable is
-- existentially-bound, meaning no explicit value is given for it.
exBindVar :: I.Var -> L1Type a -> I2LM ()
exBindVar var l1tp = pmBindVar var l1tp (mkExistsP_tp l1tp)


----------------------------------------------------------------------
-- Ivory global symbols
----------------------------------------------------------------------

-- | Convert a symbol to a pointer
convertSymbol :: I.Sym -> I2LM (ILExpr Ptr)
convertSymbol sym =
  do info <- get
     case Map.lookup sym (i2l_syms info) of
       Just i -> return $ mkOp (Op_global_var (-i))
       Nothing ->
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
-- FIXME: special-case the Word64 case for l1tp
readArray l1tp@(L1Type_lit lit_tp) ptr ix =
  do n <- readArrayNameWord64 ptr ix
     return $ mkOp (Op_coerce litTypeRep lit_tp) (mkVar Proxy n)
readArray l1tp@L1Type_ptr ptr ix =
  do n <- readArrayNamePtr ptr ix
     return $ mkVarExprTp (LType_base l1tp) n
readArray L1Type_prop ptr ix =
  error "readArray: attempt to read a proposition from memory!"

-- | Update an Ivory array value
updateArray :: L1Type a -> ILExpr Ptr -> ILExpr (Literal Word64) -> ILExpr a ->
               I2LM ()
updateArray (L1Type_lit lit_tp) ptr ix v =
  addTransition $
  mkOp (Op_updateP $ UpdateOp_array Elem_base) ptr ix $
  -- NOTE: this is to convert v to a Word64 expression, as that is
  -- what our memory model actually stores in Ivory
  mkOp (Op_coerce lit_tp litTypeRep) v
updateArray L1Type_ptr ptr ix v =
  addTransition $
  mkOp (Op_updateP UpdateOp_ptr_array) ptr ix v
updateArray L1Type_ptr ptr ix v =
  error "updateArray: attempt to write a proposition to memory!"

-- | Allocate an Ivory array of a given length, returning a pointer to it
allocateArray :: ILExpr (Literal Word64) -> I2LM (ILExpr Ptr)
allocateArray len =
  addBindTransition $
  mkSeqP (mkOp (Op_updateP UpdateOp_alloc) len) $
  mkOp (Op_readP ReadOp_last_alloc)


----------------------------------------------------------------------
-- Interpreting Ivory structs
----------------------------------------------------------------------

-- | Look up a 'Struct' by name
lookupStruct :: String -> I2LM I.Struct
lookupStruct s_name =
  liftM ((\maybe_s -> case maybe_s of
             Just s -> s
             Nothing -> error ("lookupStruct: struct "
                               ++ s_name ++ " not found!"))
         . find (struct_name_eq s_name)
         . concatMap (I.public . I.modStructs) . i2l_modules) get
  where
    struct_name_eq s_name (I.Struct s_name' _) = s_name == s_name'
    struct_name_eq s_name (I.Abstract s_name' _) = s_name == s_name'

-- | Get the type and the index number of a field in a given struct
lookupStructField :: String -> String -> I2LM (I.Type, Int)
lookupStructField s_name f_name =
  liftM (helper f_name) $ lookupStruct s_name
  where
    helper f_name (I.Struct _ fields) =
      find_helper f_name 0 fields
    helper _ (I.Abstract _ _) =
      error "lookupStructField: abstract struct!"
    find_helper f_name _ [] =
      error ("lookupStructField: could not find field " ++ f_name)
    find_helper f_name i (fld:_) | I.tValue fld == f_name = (I.tType fld, i)
    find_helper f_name i (_:flds) = find_helper f_name (i+1) flds


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
  mkSomeLitType (litTypeRep :: LitType Integer)
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
-- Converting Ivory expressions into Ivory reachability logic
----------------------------------------------------------------------

-- | An Ivory expression that has been converted into an Ivory
-- reachability logic expression of some first-order type
data SomeILExpr where
  SomeILExpr :: L1Type a -> ILExpr a -> SomeILExpr

-- | Convert an 'Integer' to a logical expression of a given type
convertInteger :: L1Type a -> Integer -> ILExpr a
convertInteger (L1Type_lit LitType_int) i = mkLiteral i
convertInteger (L1Type_lit lit_tp@LitType_bits) i =
  mkLiteralTp lit_tp $ fromInteger i
convertInteger (L1Type_lit LitType_unit) _ =
  error "convertInteger: unit type!"
convertInteger (L1Type_lit LitType_bool) _ =
  error "convertInteger: Boolean type!"

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

-- | Convert an Ivory expression to a logical expression of a specific type
convertExpr :: L1Type a -> I.Expr -> I2LM (ILExpr a)

-- Symbols and variables
convertExpr l1tp (I.ExpSym sym) =
  do ptr_expr <- convertSymbol sym
     readArray l1tp ptr_expr (mkLiteral 0)
convertExpr l1tp (I.ExpExtern ext) =
  error "convertExpr: cannot (yet) convert external symbols"
convertExpr l1tp (I.ExpVar v) =
  liftM (mkVarExprTp $ LType_base l1tp) $ getVar l1tp v

-- Literals
convertExpr l1tp (I.ExpLit lit) =
  return $ convertLiteral l1tp lit

-- Array and struct dereferencing
convertExpr l1tp (I.ExpLabel (I.TyStruct s_name) s_iexpr f_name) =
  do s_expr <- convertExpr L1Type_ptr s_iexpr
     (_, f_ix) <- lookupStructField s_name f_name
     readArray l1tp s_expr (mkLiteral $ fromInteger $ toInteger f_ix)
convertExpr l1tp (I.ExpIndex arr_elem_itp arr_iexpr ix_itp ix_iexpr) =
  case convertType ix_itp of
    SomeL1Type ix_l1tp@(L1Type_lit ix_lit_tp) ->
      do ix_expr <- convertExpr ix_l1tp ix_iexpr
         arr_expr <- convertExpr L1Type_ptr arr_iexpr
         readArray l1tp arr_expr (mkOp (Op_coerce ix_lit_tp litTypeRep) ix_expr)

-- Coercion between types
convertExpr l1tp@(L1Type_lit lit_tp) (I.ExpToIx iexpr modulus) =
  do e <- convertExpr l1tp iexpr
     return $ mkOp (Op_arith2 lit_tp Op2_Mod) e (convertInteger l1tp modulus)
convertExpr l1tp@(L1Type_lit lit_tp_to) (I.ExpSafeCast to_itp iexpr) =
  case convertType to_itp of
    SomeL1Type l1tp_from@(L1Type_lit lit_tp_from) ->
      do e <- convertExpr l1tp_from iexpr
         return $ mkOp (Op_coerce lit_tp_from lit_tp_to) e
    SomeL1Type _ ->
      error "convertExpr: attempt to cast from a non-literal type!"

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


-- | Convert an Ivory initialization expression into logic
convertInit :: L1Type a -> I.Init -> I2LM (ILExpr a)
convertInit l1tp _ = error "FIXME HERE NOW!"


----------------------------------------------------------------------
-- Converting Ivory statements into Ivory reachability logic
----------------------------------------------------------------------

-- | Convert an Ivory statement to a transition relation
convertStmt :: I.Stmt -> I2LM ILPM

-- If-then-else statements --> disjunctive transition relations
convertStmt (I.IfTE ie stmts1 stmts2) =
  do e <- convertExpr l1typeRep ie
     let prop = mkIsTrue e
     pm1 <- liftM mkSequenceP $ mapM convertStmt stmts1
     pm2 <- liftM mkSequenceP $ mapM convertStmt stmts2
     return $
       mkOrP (mkSeqP (mkAssumeP prop) pm1)
       (mkSeqP (mkAssumeP (mkNot prop)) pm2)

-- Assertions --> errors if the assertion does not hold; i.e., they
-- become @if prop then ('returnP' ()) else raise 'IvoryError'@
convertStmt (I.Assert ie) =
  do e <- convertExpr l1typeRep ie
     return $ mkAssertP IvoryError (mkIsTrue e)

-- Compiler-inserted assertions, which are the same as normal assertions
convertStmt (I.CompilerAssert ie) =
  do e <- convertExpr l1typeRep ie
     return $ mkAssertP IvoryError (mkIsTrue e)

-- Return statements --> write the returned value to the returnValue
-- global variable and then raise an 'IvoryReturn' exception
convertStmt (I.Return typed_ie) =
  case convertType (I.tType typed_ie) of
    SomeL1Type l1tp ->
      do e <- convertExpr l1tp (I.tValue typed_ie)
         updateArray l1tp returnValuePtr (mkLiteral 0) e
         return $ mkRaiseP IvoryReturn

-- Return statements with no return value --> just raise 'IvoryReturn'
convertStmt I.ReturnVoid = return $ mkRaiseP IvoryReturn

-- Dereference statements --> read a pointer and bind the result to a variable
convertStmt (I.Deref itp var ie) =
  case convertType itp of
    SomeL1Type l1tp ->
      do ptr_expr <- convertExpr L1Type_ptr ie
         res_expr <- readArray l1tp ptr_expr (mkLiteral 0)
         letBindVar var l1tp res_expr
         return $ mkReturnP $ mkLiteral ()

-- Assignment statements
convertStmt (I.Store itp ilhs irhs) =
  case convertType itp of
    SomeL1Type l1tp ->
      do ptr <- convertExpr L1Type_ptr ilhs
         val <- convertExpr l1tp irhs
         updateArray l1tp ptr (mkLiteral 0) val
         return $ mkReturnP $ mkLiteral ()

-- Function calls: not handled yet!
convertStmt (I.Call _ _ _ _) =
  error "convertStmt: function calls not yet handled!" -- FIXME HERE

-- Local variable decl --> allocation of a new reference + initialization
convertStmt (I.Local itp var init) =
  case convertType itp of
    SomeL1Type l1tp ->
      do ptr <- allocateArray (mkLiteral 1)
         v <- convertInit l1tp init
         letBindVar var L1Type_ptr ptr
         return $ mkReturnP $ mkLiteral ()

-- Reference copy: not handled yet!
convertStmt (I.RefCopy itp ilhs irhs) =
  error "convertStmt: reference copies not yet handled!" -- FIXME HERE

-- Reference allocation: not handled yet (what does the name mean?)
convertStmt (I.AllocRef itp var nm) =
  error "convertStmt: AllocRef not yet handled!" -- FIXME HERE

-- Loops --> not handled yet
convertStmt (I.Loop max_iters var start incr body) =
  error "convertStmt: loops not yet handled!" -- FIXME HERE

-- Loops --> not handled yet
convertStmt (I.Forever body) =
  error "convertStmt: loops not yet handled!" -- FIXME HERE

-- Break --> raise an IvoryBreak exception
convertStmt I.Break = return $ mkRaiseP IvoryBreak

-- Comment --> no-op
convertStmt (I.Comment _) = return $ mkReturnP $ mkLiteral ()
