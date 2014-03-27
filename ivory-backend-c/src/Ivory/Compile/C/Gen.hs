{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

-- | Ivory backend targeting language-c-quote.

module Ivory.Compile.C.Gen where

import qualified "language-c-quote" Language.C.Syntax as C
import Language.C.Quote.GCC

import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Proc as P

import Ivory.Compile.C.Types
import Ivory.Compile.C.Prop

import Prelude hiding (exp, abs, signum)
import qualified Prelude as P
import Data.List (foldl')

import Data.Loc (noLoc)

data Visibility = Public | Private deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Compile a top-level element.
compile :: P.Def a -> Compile
compile (P.DefProc fun) = compileUnit fun
compile (P.DefExtern e) = compileExtern e
compile (P.DefImport _) = error "Can't compile an import!"

--------------------------------------------------------------------------------
-- | Compile an extern declaration
compileExtern :: I.Extern -> Compile
compileExtern I.Extern
  { I.externSym     = sym
  , I.externRetType = ret
  , I.externArgs    = args
  }
  = putExt [cedecl| extern $ty:(toType ret) $id:(sym)
                      ($params:(map mkParam args)) ; |]
  where
  mkParam ty = [cparam| $ty:(toType ty) |]

--------------------------------------------------------------------------------
-- | Compile a struct.
compileStruct :: Visibility -> I.Struct -> Compile
compileStruct visibility def = case def of
  I.Struct n fs    ->
    (if visibility == Public then putHdrSrc else putSrc)
    [cedecl| struct $id:n { $sdecls:(map mkFieldGroup fs)
                          } __attribute__ ((__packed__)) ;
           |]
  I.Abstract _ file ->
    putHdrInc (SysInclude file)

mkFieldGroup :: I.Typed String -> C.FieldGroup
mkFieldGroup field =
  [csdecl| $ty:(toType (I.tType field)) $id:(I.tValue field) ; |]

--------------------------------------------------------------------------------
-- | Compile an external memory area reference.
compileAreaImport :: I.AreaImport -> Compile
compileAreaImport ai = putSrcInc (SysInclude (I.aiFile ai))

--------------------------------------------------------------------------------
-- | Get prototypes for memory areas.
extractAreaProto :: Visibility -> I.Area -> Compile
extractAreaProto visibility area = do
  let aty                   = toType (I.areaType area)
      ty | I.areaConst area = [cty| const $ty:aty |]
         | otherwise        = aty
  if visibility == Public
    then putHdrSrc [cedecl| extern $ty:ty $id:(I.areaSym area); |]
    else putSrc    [cedecl| static $ty:ty $id:(I.areaSym area); |]

-- | Compile a memory area definition into an extern in the header, and a
-- structure in the source.
compileArea :: Visibility -> I.Area -> Compile
compileArea visibility area = do
  let aty                   = toType (I.areaType area)
      i                     = I.areaInit area
      ty | I.areaConst area = [cty| const $ty:aty |]
         | otherwise        = aty
  case i of
    I.InitZero -> putSrc [cedecl| $ty:(type' visibility ty) $id:(I.areaSym area) ; |]
    _          -> putSrc [cedecl| $ty:(type' visibility ty)
                                 $id:(I.areaSym area) = $init:(toInit i) ; |]
  where
  type' Public ty' = [cty|$ty:ty'|]
  type' Private ty' = [cty|static $ty:ty'|]

--------------------------------------------------------------------------------
-- | Compile a definition unit.
compileUnit :: I.Proc -> Compile
compileUnit I.Proc { I.procSym      = sym
                   , I.procRetTy    = ret
                   , I.procArgs     = args
                   , I.procBody     = body
                   , I.procRequires = requires
                   , I.procEnsures  = ensures
                   }
  = do let ens  = map I.getEnsure ensures
       let bd   = concatMap (toBody ens) body
       let reqs = map (toRequire . I.getRequire) requires
       putSrc [cedecl| $ty:(toType ret) ($id:sym) ($params:(toArgs args))
                         { $items:reqs
                           $items:bd
                         } |]

--------------------------------------------------------------------------------
-- | Get the prototypes.
extractProto :: Visibility -> I.Proc -> Compile
extractProto visibility I.Proc { I.procSym   = sym
                               , I.procRetTy = ret
                               , I.procArgs  = args
                               }
  = if visibility == Public
       then putHdrSrc
         [cedecl| $ty:(toType ret) ($id:sym) ($params:(toArgs args)); |]
       else putSrc
         [cedecl| static $ty:(toType ret) ($id:sym) ($params:(toArgs args)); |]

--------------------------------------------------------------------------------

-- | Argument conversion.
toArgs :: [I.Typed I.Var] -> [C.Param]
toArgs = foldl' go [] . reverse
  where
  go acc I.Typed { I.tType  = t
                 , I.tValue = v }
    = [cparam| $ty:(toType t) $id:(toVar v) |] : acc

toParam :: C.Type -> C.Param
toParam ty = case ty of
  C.Type spec decl loc -> C.Param Nothing spec decl loc
  _                    -> error "toParam: unexpected anti-quote"

-- | Type conversion outside of an assignment context.  This converts arrays to
-- arrays, and carrays to pointers.
toType :: I.Type -> C.Type
toType  = toTypeCxt $ \ t -> case t of
  I.TyCArray t'  -> [cty| $ty:(toType t') *          |]
  I.TyArr len t' -> [cty| $ty:(toType t')[$uint:len] |]
  _              -> [cty| $ty:(toType t) *           |]


-- | Type conversion in the context of an assignment.
-- This converts all arrays/carrays to pointers.
toTypeAssign :: I.Type -> C.Type
toTypeAssign  = toTypeCxt $ \ t -> case t of
  I.TyCArray t'  -> [cty| $ty:(toTypeAssign t') * |]
  I.TyArr _   t' -> [cty| $ty:(toTypeAssign t') * |]
  _              -> [cty| $ty:(toTypeAssign t)  * |]

--------------------------------------------------------------------------------
-- | C type conversion, with a special case for references and pointers.
toTypeCxt :: (I.Type -> C.Type) -> I.Type -> C.Type
toTypeCxt arrCase = convert
  where
  convert ty = case ty of
    I.TyVoid              -> [cty| void |]
    I.TyChar              -> [cty| char |]
    I.TyInt i             -> intSize i
    I.TyWord w            -> wordSize w
    I.TyBool              -> [cty| typename bool |]
    I.TyFloat             -> [cty| float |]
    I.TyDouble            -> [cty| double |]
    I.TyStruct nm         -> [cty| struct $id:nm |]
    I.TyConstRef t        -> [cty| const $ty:(arrCase t) |]
    -- Reference is a guaranted non-NULL pointer.
    I.TyRef t             -> arrCase t
    I.TyPtr t             -> arrCase t
    I.TyArr len t         -> [cty| $ty:(convert t)[$uint:len] |]
    I.TyCArray t          -> [cty| $ty:(convert t) * |]
    I.TyProc retTy argTys ->
      [cty| $ty:(convert retTy) (*)
            ($params:(map (toParam . convert) argTys)) |]
    I.TyOpaque            -> error "Opaque type is not implementable."

intSize :: I.IntSize -> C.Type
intSize I.Int8  = [cty| typename int8_t  |]
intSize I.Int16 = [cty| typename int16_t |]
intSize I.Int32 = [cty| typename int32_t |]
intSize I.Int64 = [cty| typename int64_t |]

wordSize :: I.WordSize -> C.Type
wordSize I.Word8  = [cty| typename uint8_t  |]
wordSize I.Word16 = [cty| typename uint16_t |]
wordSize I.Word32 = [cty| typename uint32_t |]
wordSize I.Word64 = [cty| typename uint64_t |]

--------------------------------------------------------------------------------
-- | Call Symbols
toName :: I.Name -> String
toName name = case name of
  I.NameSym s   -> s
  I.NameVar var -> toVar var

-- | Variable name mangling.
toVar :: I.Var -> String
toVar var = case var of
  I.VarName n     -> "n_" ++ n
  I.VarInternal n -> "i_" ++ n
  I.VarLitName n  -> n

--------------------------------------------------------------------------------

-- | Translate statements.
toBody :: [I.Cond] -> I.Stmt -> [C.BlockItem]
toBody ens stmt =
  let toBody' = toBody ens in
  case stmt of
    I.Assign t v exp       ->
      [C.BlockDecl [cdecl| $ty:(toTypeAssign t) $id:(toVar v)
                         = $exp:(toExpr t exp); |]]
    I.IfTE exp blk0 blk1   ->
      let ifBd   = concatMap toBody' blk0 in
      let elseBd = concatMap toBody' blk1 in
      if null elseBd
        then [C.BlockStm [cstm| if($exp:(toExpr I.TyBool exp)) {
                                  $items:ifBd } |]]
        else [C.BlockStm [cstm| if($exp:(toExpr I.TyBool exp)) {
                                  $items:ifBd }
                                else { $items:elseBd } |]]
    I.Return exp           ->
           map (toEnsure $ I.tValue exp) ens
        ++ [C.BlockStm [cstm| return $exp:(typedRet exp); |]]
    I.ReturnVoid           -> [C.BlockStm [cstm| return; |]]

    -- t is the referenced type.  Should only be able to deref a stored value.
    -- We replicate some of the type deconstruction in parsing expressions to
    -- optimize away *& constructions on dereferncing structs and indexes into
    -- arrays.
    I.Deref t var exp      -> [C.BlockDecl
      [cdecl| $ty:(toType t) $id:(toVar var) = $exp:deref; |]]
      where
      deref = case t of
                I.TyArr _ _  -> [cexp| * $exp:e |]
                I.TyCArray _ -> [cexp| * $exp:e |]
                _            -> [cexp|   $exp:e |]
      e = case exp of
            I.ExpLabel t' e' field ->
              [cexp| $exp:(toExpr (I.TyRef t') e') -> $id:field |]
            I.ExpIndex t' e' ti i  ->
              [cexp| $exp:(toExpr (I.TyRef t') e') [$exp:(toExpr ti i)] |]
            _                      -> [cexp| * $exp:(toExpr (I.TyRef t) exp) |]

    I.Local t var inits    -> [C.BlockDecl
      [cdecl| $ty:(toType t) $id:(toVar var) = $init:(toInit inits); |]]
    -- Can't do a static check since we have local let bindings.
    I.RefCopy t vto vfrom  ->
      [C.BlockStm [cstm| if( $exp:toRef != $exp:fromRef) {
         memcpy( $exp:toRef, $exp:fromRef, sizeof($ty:(toType t)) ); }
                           else { COMPILER_ASSERTS(false); }
      |]]
      where
      toRef   = toExpr (I.TyRef t) vto
      fromRef = toExpr (I.TyRef t) vfrom

    -- Should only be a reference (not a pointer).
    I.AllocRef t l r       -> [C.BlockDecl
        [cdecl| $ty:(toType (I.TyRef rty)) $id:(toVar l) = $exp:rhs; |]]
      where
      name      = toName r
      (rhs,rty) = case t of
        I.TyArr _ t'  -> ([cexp| $id:name    |], t')
        I.TyCArray t' -> ([cexp| $id:name    |], t')
        _             -> ([cexp| &($id:name) |], t)
    I.Assert exp           -> [C.BlockStm
      [cstm| ASSERTS($exp:(toExpr I.TyBool exp)); |]]
    I.CompilerAssert exp   -> [C.BlockStm
      [cstm| COMPILER_ASSERTS($exp:(toExpr I.TyBool exp)); |]]
    I.Assume exp           -> [C.BlockStm
      [cstm| ASSUMES($exp:(toExpr I.TyBool exp)); |]]
    I.Call t mVar sym args ->
      case mVar of
        Nothing  -> -- Just call the fuction.
          [C.BlockStm [cstm| $id:(toName sym)($args:(map go args)); |]]
        Just var -> -- Call it and assign it a value.
          [C.BlockDecl [cdecl| $ty:(toType t) $id:(toVar var) =
                                 $id:(toName sym)($args:(map go args));
                      |]]
        where
        go I.Typed { I.tType = t'
                   , I.tValue = v }
          = toExpr t' v
    -- Assume that ty is a signed and sufficiently big (int).
    I.Loop var start incr blk ->
      let loopBd =  concatMap toBody' blk in
      [C.BlockStm [cstm| for( $ty:(toType ty) $id:(toVar var)
                                = $exp:(toExpr ty start);
                              $exp:test;
                              $exp:incExp ) {
                       $items:loopBd } |]]
      where
      ty = I.TyInt I.Int32
      (test,incExp)  = toIncr incr
      ix = toVar var
      toIncr (I.IncrTo to) =
        ( [cexp| $id:ix <= $exp:(toExpr ty to) |]
        , [cexp| $id:ix++ |] )
      toIncr (I.DecrTo to) =
        ( [cexp| $id:ix >= $exp:(toExpr ty to) |]
        , [cexp| $id:ix-- |] )

    I.Forever blk ->
      let foreverBd =  concatMap toBody' blk
          foreverDecl = C.BlockDecl
            [cdecl| int forever_loop __attribute__((unused)); |]
          loop = C.BlockStm [cstm| for( forever_loop = 0
                                      ; FOREVER
                                      ; FOREVER_INC ) { $items:foreverBd } |]
          decAndLoop = [ foreverDecl, loop ]
      in  [ C.BlockStm [cstm| { $items:decAndLoop } |] ]

    I.Break                       -> [C.BlockStm [cstm| break; |]]
    I.Store t ptr exp             -> [C.BlockStm
      [cstm| * $exp:(toExpr (I.TyRef t) ptr) = $exp:(toExpr t exp); |]]

    I.Comment c                   -> [C.BlockStm (C.Comment c noLoc)]
-- | Return statement.
typedRet :: I.Typed I.Expr -> C.Exp
typedRet I.Typed { I.tType  = t
                 , I.tValue = exp }
    = [cexp| $exp:(toExpr t exp) |]

--------------------------------------------------------------------------------

toInit :: I.Init -> C.Initializer
toInit i = case i of
  I.InitZero      -> [cinit|{$inits:([])}|] -- {}
  I.InitExpr ty e -> [cinit|$exp:(toExpr ty e)|]
  I.InitArray is  -> [cinit|{$inits:([ toInit j | j <- is ])}|]
  I.InitStruct fs ->
    C.CompoundInitializer [ (Just (fieldDes f), toInit j) | (f,j) <- fs ] noLoc

fieldDes :: String -> C.Designation
fieldDes n = C.Designation [ C.MemberDesignator (C.Id n noLoc) noLoc ] noLoc

--------------------------------------------------------------------------------

-- | Translate an expression.
toExpr :: I.Type -> I.Expr -> C.Exp
----------------------------------------
toExpr _ (I.ExpVar var)  = [cexp| $id:(toVar var) |]
----------------------------------------
toExpr t (I.ExpLit lit)  =
  case lit of
    -- XXX hack: should make type-correct literals.
    I.LitInteger i -> [cexp| ($ty:(toType t))$id:fromInt |]
      where fromInt = case t of
                        I.TyWord _  -> show i ++ "U"
                        I.TyInt  _  -> show i
                        I.TyFloat   -> show (fromIntegral i :: Float) ++ "F"
                        I.TyDouble  -> show (fromIntegral i :: Double)
                        _           -> error ("Nonint type " ++ (show t) ++
                                              " of literal " ++ (show i) )
    I.LitChar c    -> [cexp| $char:c |]
    I.LitBool b    -> [cexp| $id:(if b then "true" else "false") |]
    I.LitNull      -> [cexp| NULL |]
    I.LitString s  -> [cexp| $string:s |]
    I.LitFloat f   -> [cexp| $id:(show f ++ "f") |]
    I.LitDouble d  -> [cexp| $id:(show d) |]
----------------------------------------
toExpr t (I.ExpOp op args) = [cexp| ($ty:(toType t)) $exp:(toExpOp t op args) |]
----------------------------------------
toExpr _ (I.ExpSym sym) = [cexp| $id:sym |]
----------------------------------------
toExpr t (I.ExpLabel t' e field) = case t of
  I.TyRef (I.TyArr _ _)       -> getField
  I.TyRef (I.TyCArray _)      -> getField
  I.TyConstRef (I.TyArr _ _)  -> getField
  I.TyConstRef (I.TyCArray _) -> getField
  _                           ->
    [cexp| &($exp:(toExpr (I.TyRef t') e) -> $id:field) |]
  where getField = [cexp| ($exp:(toExpr  t' e) -> $id:field) |]
----------------------------------------
toExpr t (I.ExpIndex at a ti i) = case t of
  I.TyRef (I.TyArr _ _)       -> expIdx I.TyRef
  I.TyRef (I.TyCArray _)      -> expIdx I.TyRef
  I.TyConstRef (I.TyArr _ _)  -> expIdx I.TyConstRef
  I.TyConstRef (I.TyCArray _) -> expIdx I.TyConstRef
  _                           ->
    [cexp| &($exp:(toExpr (I.TyRef at) a) [$exp:(toExpr ti i)]) |]
  where
  expIdx constr =
    [cexp| ($exp:(toExpr (constr at) a) [$exp:(toExpr ti i)]) |]
----------------------------------------
toExpr tTo (I.ExpSafeCast tFrom e) =
  [cexp| ($ty:(toType tTo))$exp:(toExpr tFrom e) |]
----------------------------------------
toExpr tTo (I.ExpToIx e maxSz) =
  [cexp| $exp:(toExpr tTo e ) % $exp:maxSz |]
----------------------------------------
toExpr tTo (I.ExpAddrOfGlobal sym) = case tTo of
  I.TyRef (I.TyArr _ _)       -> [cexp| $id:sym |]
  I.TyRef (I.TyCArray _)      -> [cexp| $id:sym |]
  I.TyConstRef (I.TyArr _ _)  -> [cexp| $id:sym |]
  I.TyConstRef (I.TyCArray _) -> [cexp| $id:sym |]
  _                           -> [cexp| & $id:sym |]
----------------------------------------
toExpr ty (I.ExpMaxMin b) = [cexp| $id:macro |]
  where
  macro = case b of
    True  -> case ty of
      I.TyInt sz -> case sz of
        I.Int8     -> "INT8_MAX"
        I.Int16    -> "INT16_MAX"
        I.Int32    -> "INT32_MAX"
        I.Int64    -> "INT64_MAX"
      I.TyWord sz -> case sz of
        I.Word8     -> "UINT8_MAX"
        I.Word16    -> "UINT16_MAX"
        I.Word32    -> "UINT32_MAX"
        I.Word64    -> "UINT64_MAX"
      _           -> err
    False -> case ty of
      I.TyInt sz -> case sz of
        I.Int8     -> "INT8_MIN"
        I.Int16    -> "INT16_MIN"
        I.Int32    -> "INT32_MIN"
        I.Int64    -> "INT64_MIN"
      _          -> err
  err = error $ "unexpected type " ++ show ty ++ " in ExpMaxMin."
----------------------------------------


exp0 :: [C.Exp] -> C.Exp
exp0 = flip (!!) 0

exp1 :: [C.Exp] -> C.Exp
exp1 = flip (!!) 1

exp2 :: [C.Exp] -> C.Exp
exp2 = flip (!!) 2

mkArgs :: I.Type -> [I.Expr] -> [C.Exp]
mkArgs ty = map (toExpr ty)

toExpOp :: I.Type -> I.ExpOp -> [I.Expr] -> C.Exp
toExpOp ty op args = case op of
  -- eq instance
  I.ExpEq ety  -> let xs = mkArgs ety args in
                  [cexp| $exp:(exp0 xs) == $exp:(exp1 xs) |]
  I.ExpNeq ety -> let xs = mkArgs ety args in
                  [cexp| $exp:(exp0 xs) != $exp:(exp1 xs) |]
  -- conditional expressions
  I.ExpCond    -> let b  = toExpr I.TyBool (head args) in
                  let xs = mkArgs ty (tail args) in
                  [cexp| $exp:b ? $exp:(exp0 xs) : $exp:(exp1 xs) |]
  -- ord instance
  I.ExpGt orEq ety
    | orEq      -> let xs = mkArgs ety args in
                   [cexp| $exp:(exp0 xs) >= $exp:(exp1 xs) |]
    | otherwise -> let xs = mkArgs ety args in
                   [cexp| $exp:(exp0 xs) > $exp:(exp1 xs) |]
  I.ExpLt orEq ety
    | orEq      -> let xs = mkArgs ety args in
                   [cexp| $exp:(exp0 xs) <= $exp:(exp1 xs) |]
    | otherwise -> let xs = mkArgs ety args in
                   [cexp| $exp:(exp0 xs) < $exp:(exp1 xs) |]
  -- boolean operations
  I.ExpNot      -> let xs = mkArgs ty args in
                   [cexp| !($exp:(exp0 xs)) |]
  I.ExpAnd      -> let xs = mkArgs ty args in
                   [cexp| $exp:(exp0 xs) && $exp:(exp1 xs) |]
  I.ExpOr       -> let xs = mkArgs ty args in
                   [cexp| $exp:(exp0 xs) || $exp:(exp1 xs) |]
  -- num instance
  I.ExpMul      -> let xs = mkArgs ty args in
                   [cexp| $exp:(exp0 xs) * $exp:(exp1 xs) |]
  I.ExpAdd      -> let xs = mkArgs ty args in
                   [cexp| $exp:(exp0 xs) + $exp:(exp1 xs) |]
  I.ExpSub      -> let xs = mkArgs ty args in
                   [cexp| $exp:(exp0 xs) - $exp:(exp1 xs) |]
  I.ExpNegate   -> let xs = mkArgs ty args in
                   [cexp| - ($exp:(exp0 xs)) |]
  I.ExpAbs      -> let xs = mkArgs ty args in
                   [cexp| $id:(absSym ty)($exp:(exp0 xs)) |]
  I.ExpSignum   -> let xs = mkArgs ty args in
                   [cexp| $id:(signumSym ty)($exp:(exp0 xs)) |]

  -- integral/fractional instance
  I.ExpDiv      -> let xs = mkArgs ty args in
                   [cexp| $exp:(exp0 xs) / $exp:(exp1 xs) |]
  I.ExpMod      -> toMod ty args
  I.ExpRecip    -> let xs = mkArgs ty args in
                   [cexp| 1 / $exp:(exp0 xs) |]

  -- floating instance
  I.ExpFExp       -> floatingUnary ty "exp"  args
  I.ExpFSqrt      -> floatingUnary ty "sqrt" args
  I.ExpFLog       -> floatingUnary ty "log"  args
  I.ExpFPow       -> floatingBinary ty "pow" args
  I.ExpFLogBase   -> toLogBase ty args
  I.ExpFSin       -> floatingUnary ty "sin"   args
  I.ExpFCos       -> floatingUnary ty "cos"   args
  I.ExpFTan       -> floatingUnary ty "tan"   args
  I.ExpFAsin      -> floatingUnary ty "asin"  args
  I.ExpFAcos      -> floatingUnary ty "acos"  args
  I.ExpFAtan      -> floatingUnary ty "atan"  args
  I.ExpFSinh      -> floatingUnary ty "sinh"  args
  I.ExpFCosh      -> floatingUnary ty "cosh"  args
  I.ExpFTanh      -> floatingUnary ty "tanh"  args
  I.ExpFAsinh     -> floatingUnary ty "asinh" args
  I.ExpFAcosh     -> floatingUnary ty "acosh" args
  I.ExpFAtanh     -> floatingUnary ty "atanh" args

  -- float operations
  -- XXX this needs to add a dependency on <math.h>
  I.ExpIsNan ety -> let xs = mkArgs ety args in
                    [cexp| ($ty:(toType I.TyBool)) (isnan($exp:(exp0 xs))) |]
  -- isinf returns -1 for negative infinity and 1 for positive infinity.
  I.ExpIsInf ety -> let xs = mkArgs ety args in
                    [cexp| ($ty:(toType I.TyBool)) (isinf($exp:(exp0 xs))) |]
  I.ExpRoundF    -> floatingBinary ty "round" args
  I.ExpCeilF     -> floatingBinary ty "ceil" args
  I.ExpFloorF    -> floatingBinary ty "floor" args

  -- -- float casting
  -- I.ExpToFloat ety   -> let xs = mkArgs ety args in
  --                       [cexp| ($ty:(toType ty))($exp:(exp0 xs)) |]
  -- I.ExpFromFloat ety ->
  --   [cexp| ($ty:(toType ty))($exp:(floatingUnary ety "trunc" args)) |]

  -- bit operations
  I.ExpBitAnd        -> let xs = mkArgs ty args in
                        [cexp| $exp:(exp0 xs) & $exp:(exp1 xs) |]
  I.ExpBitOr         -> let xs = mkArgs ty args in
                        [cexp| $exp:(exp0 xs) | $exp:(exp1 xs) |]
  I.ExpBitXor        -> let xs = mkArgs ty args in
                        [cexp| $exp:(exp0 xs) ^ $exp:(exp1 xs) |]
  I.ExpBitComplement -> let xs = mkArgs ty args in
                        [cexp| ~($exp:(exp0 xs)) |]
  I.ExpBitShiftL     -> let xs = mkArgs ty args in
                        [cexp| $exp:(exp0 xs) << $exp:(exp1 xs) |]
  I.ExpBitShiftR     -> let xs = mkArgs ty args in
                        [cexp| $exp:(exp0 xs) >> $exp:(exp1 xs) |]

floatingSym :: I.Type -> String ->  String
floatingSym t sym = case t of
  I.TyFloat -> sym ++ "f"
  I.TyDouble -> sym
  _ -> error "Can't make floatingSym out of non-float"

floatingBinary :: I.Type -> String -> [I.Expr] -> C.Exp
floatingBinary ty name args =
  let xs = mkArgs ty args in
  [cexp| $id:(floatingSym ty name)($exp:(exp0 xs), $exp:(exp1 xs)) |]

floatingUnary :: I.Type -> String -> [I.Expr] -> C.Exp
floatingUnary ty name args =
  let xs = mkArgs ty args in
  [cexp| $id:(floatingSym ty name)($exp:(exp0 xs)) |]

toLogBase :: I.Type -> [I.Expr] -> C.Exp
toLogBase ty args = [cexp| $exp:(logC $ exp0 xs) / $exp:(logC $ exp1 xs) |]
  where
  xs = mkArgs ty args
  logC e = [cexp| $id:(floatingSym ty "log")($exp:e) |]

-- XXX Not sure aobut this, as there's currently no way to perform mod on
-- float/double in the frontend.
toMod :: I.Type -> [I.Expr] -> C.Exp
toMod ty args = case ty of
  I.TyFloat  -> [cexp| fmodf($exp:x', $exp:y') |]
  I.TyDouble -> [cexp| fmod ($exp:x', $exp:y') |]
  _          -> [cexp| $exp:x' % $exp:y' |]
  where
  args' = mkArgs ty args
  x' = exp0 args'
  y' = exp1 args'

-- | Emit the function name for a call to abs.  This doesn't include any symbol
-- for unsigned things, as they should be optimized out by the front end.
absSym :: I.Type -> String
absSym ty = case ty of
  I.TyFloat  -> "fabsf"
  I.TyDouble -> "fabs"
  I.TyInt i  -> "abs_i" ++ iType i
  I.TyChar   -> "abs_char"
  _          -> error ("abs " ++ "unimplemented for type " ++ show ty)
  where
  iType i = case i of
    I.Int8  -> "8"
    I.Int16 -> "16"
    I.Int32 -> "32"
    I.Int64 -> "64"

-- | Emit the function name for a call to signum.
signumSym :: I.Type -> String
signumSym ty = case ty of
  I.TyFloat  -> "signum_float"
  I.TyDouble -> "signum_double"
  I.TyInt i  -> "signum_i" ++ showInt i
  I.TyWord w -> "signum_w" ++ showWord w
  I.TyChar   -> "signum_char"
  _          -> error ("signum " ++ "unimplemented for type " ++ show ty)
----------------------------------------

showInt :: I.IntSize -> String
showInt I.Int8  = show (8 :: Int)
showInt I.Int16 = show (16 :: Int)
showInt I.Int32 = show (32 :: Int)
showInt I.Int64 = show (64 :: Int)

showWord :: I.WordSize -> String
showWord I.Word8  = show (8 :: Int)
showWord I.Word16 = show (16 :: Int)
showWord I.Word32 = show (16 :: Int)
showWord I.Word64 = show (32 :: Int)

--------------------------------------------------------------------------------

toRequire :: I.Cond -> C.BlockItem
toRequire = toAssertion id "REQUIRES"

-- | Takes the return expression, the condition, and returns a 'BlockItem'.
toEnsure :: I.Expr -> I.Cond -> C.BlockItem
toEnsure retE = toAssertion (ensTrans retE) "ENSURES"

toAssertion :: (I.Expr -> I.Expr) -> String -> I.Cond -> C.BlockItem
toAssertion trans call cond = C.BlockStm $
  case cond of
    I.CondBool e          ->
      [cstm| $id:call($exp:(toExpr I.TyBool (trans e))); |]
    I.CondDeref t e var c ->
      let res = (toBody []) (I.Deref t var (trans e)) in
      let c1  = toAssertion trans call c in
      [cstm| { $items:res $item:c1 } |]

--------------------------------------------------------------------------------
