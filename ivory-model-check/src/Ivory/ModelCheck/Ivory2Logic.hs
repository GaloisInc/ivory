{-# LANGUAGE GADTs, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

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

-- | The exceptions in the Ivory reachability logic
data IvoryExn = IvoryError | IvoryBreak deriving Eq

$(mkNuMatching [t| IvoryExn |])

instance Liftable IvoryExn where
  mbLift [nuP| IvoryError |] = IvoryError
  mbLift [nuP| IvoryBreak |] = IvoryBreak

instance Closable IvoryExn where
  toClosed IvoryError = $(mkClosed [| IvoryError |])
  toClosed IvoryBreak = $(mkClosed [| IvoryBreak |])

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
    i2l_vars :: Map.Map I.Var SomeILName
    -- ^ The Ivory variables that are in scope, and the Ivory
    -- reachability logic variables associated with them
  }

-- | The monad for building transition relations from Ivory expressions
newtype I2LM a =
  I2LM { runIvory2LogicM :: StateT I2LInfo (ContT ILPM Id) a }
  deriving (Functor, Applicative, Monad)

instance StateM I2LM I2LInfo where
  get = I2LM get
  set info = I2LM $ set info

-- | Helper continuation operation (FIXME: documentation)
contT :: Monad m => ((a -> m r) -> m r) -> ContT r m a
contT f = callCC $ \cc ->
 let k = runContT return . cc
 in abort =<< lift (f k)


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
withBoundName :: ((Name a -> ILPM) -> ILPM) -> I2LM (Name a)
withBoundName f = I2LM $ lift $ contT $ \k -> return $ f (runId . k)

-- | Like 'withBoundName', but convert the bound name to an expression
-- before returning it
withBoundNameExpr :: ((ILExpr a -> ILPM) -> ILPM) -> I2LM (ILExpr a)
withBoundNameExpr f = I2LM $ lift $ contT $ \k -> return $ f (runId . k)

-- | Apply a name-binding operation to the output transition relation
-- of an 'I2LM' computation, and associate the bound name with an
-- Ivory variable
withBoundVar :: I.Var -> L1Type a -> ((Name a -> ILPM) -> ILPM) -> I2LM ()
withBoundVar var l1tp f =
  do n <- withBoundName f
     info <- get
     set $ info { i2l_vars =
                    Map.insertWith
                    (\_ _ ->
                      error ("withBoundVar: duplicate binding for variable "
                             ++ show var))
                    var
                    (SomeILName l1tp n)
                    (i2l_vars info)
                }

-- | Let-bind an Ivory variable to a value, and store this binding in
-- the 'I2LInfo' transition-relation-building state. The let-binding
-- is done by building the expression @(return x) >>= k@, where @k@ is
-- the continuation of the rest of the expression being built.
withLetBoundName :: I.Var -> L1Type a -> ILExpr a -> I2LM ()
withLetBoundName var l1tp rhs =
  withBoundVar var l1tp $ \k ->
  mkOp (Op_bindP l1tp l1typeRep) (mkReturnP_tp l1tp rhs) $
  LLambda (LType_base l1tp) $ nu k

-- | Similar to 'withLetBoundName', except the given Ivory variable is
-- existentially-bound, meaning no explicit value is given for it.
withExBoundName :: I.Var -> L1Type a -> I2LM ()
withExBoundName var l1tp =
  withBoundVar var l1tp $ \k ->
  mkOp (Op_bindP l1tp l1typeRep) (mkExistsP_tp l1tp) $
  LLambda (LType_base l1tp) $ nu k


----------------------------------------------------------------------
-- Ivory state manipulation
----------------------------------------------------------------------

-- | Build an expression that is the result of a state read. This is
-- done by building the expression @(readP read_op args) >>= k@, where
-- @k@ is the continuation of the rest of the expression being built.
readArray :: L1Type a -> ILExpr Ptr -> ILExpr (Literal Word64) ->
             I2LM (ILExpr a)
readArray (L1Type_lit lit_tp) ptr ix =
  withBoundNameExpr $ \k ->
  mkOp (Op_bindP l1typeRep l1typeRep)
  (mkOp (Op_readP (ReadOp_array Elem_base)) ptr ix) $
  mkLambda $ \x -> k $ mkOp (Op_coerce litTypeRep lit_tp) x
readArray L1Type_ptr ptr ix =
  withBoundNameExpr $ \k ->
  mkOp (Op_bindP l1typeRep l1typeRep)
  (mkOp (Op_readP ReadOp_ptr_array) ptr ix) $
  mkLambda $ k


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

-- | Convert an Ivory literal to a logical expression
convertLiteral :: L1Type a -> I.Literal -> ILExpr a
convertLiteral (L1Type_lit LitType_int) (I.LitInteger i) =
  mkLiteral i
convertLiteral (L1Type_lit lit_tp@LitType_bits) (I.LitInteger i) =
  mkLiteralTp lit_tp $ fromInteger i
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
convertArithCmp :: L1Type a -> I.Type -> ArithCmp -> [I.Expr] -> I2LM (ILExpr a)
convertArithCmp l1tp_out itp acmp args =
  case (l1tp_out, convertType itp, acmp, args) of
    (L1Type_lit LitType_bool, SomeL1Type l1tp_sub@(L1Type_lit lit_tp),
     _, [ie1, ie2]) ->
      do e1 <- convertExpr l1tp_sub ie1
         e2 <- convertExpr l1tp_sub ie2
         return $ mkOp (Op_cmp lit_tp acmp) e1 e2
    (L1Type_lit LitType_bool, SomeL1Type l1tp_sub@L1Type_ptr,
     OpCmp_EQ, [ie1, ie2]) ->
      do e1 <- convertExpr l1tp_sub ie1
         e2 <- convertExpr l1tp_sub ie2
         return $ mkOp Op_ptr_eq e1 e2
    (L1Type_lit LitType_bool, SomeL1Type l1tp_sub@L1Type_ptr, _, _) ->
      error "convertArithCmp: non-equality comparison of pointers!"
    (L1Type_lit LitType_bool, SomeL1Type l1tp_sub@L1Type_prop, _, _) ->
      error "convertArithCmp: comparison of propositions!"
    (L1Type_lit LitType_bool, _, _, _) ->
      error "convertArithCmp: comparison on more or fewer than 2 arguments!"
    _ -> error "convertArithCmp: comparison at non-Boolean type!"

-- | Convert an Ivory expression to a logical expression of a specific type
convertExpr :: L1Type a -> I.Expr -> I2LM (ILExpr a)
convertExpr l1tp (I.ExpSym sym) =
  error "convertExpr: cannot (yet) handle symbols"
convertExpr l1tp (I.ExpExtern ext) =
  error "convertExpr: cannot (yet) convert external symbols"
convertExpr l1tp (I.ExpVar v) =
  liftM (mkVarExprTp $ LType_base l1tp) $ getVar l1tp v
convertExpr l1tp (I.ExpLit lit) =
  return $ convertLiteral l1tp lit
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
convertExpr l1tp (I.ExpToIx iexpr modulus) =
  error "FIXME HERE NOW"
  -- Take expr mod integer
convertExpr l1tp (I.ExpSafeCast to_itp iexpr) =
  error "FIXME HERE NOW"
  -- Never lose precision in casts
convertExpr l1tp (I.ExpOp (I.ExpEq itp) args) =
  convertArithCmp l1tp itp OpCmp_EQ args
convertExpr l1tp (I.ExpOp (I.ExpNeq itp) args) =
  do e <- convertArithCmp l1tp itp OpCmp_EQ args
     case l1tp of
       L1Type_lit LitType_bool ->
         return $ mkOp (Op_arith1 LitType_bool Op1_Neg) e
       _ -> error "convertExpr: not-equals test at non-Boolean type!"
convertExpr l1tp (I.ExpOp _ _) =
  error "FIXME HERE NOW"
-- FIXME HERE: handle more ExpOps
convertExpr l1tp (I.ExpAddrOfGlobal sym) =
  error "FIXME HERE NOW"
convertExpr l1tp (I.ExpMaxMin is_max) =
  error "FIXME HERE NOW"
convertExpr l1tp (I.ExpSizeOf itp) =
  error "FIXME HERE NOW"
convertExpr l1tp e =
  error ("convertExpr: could not convert expression: " ++ show e)
