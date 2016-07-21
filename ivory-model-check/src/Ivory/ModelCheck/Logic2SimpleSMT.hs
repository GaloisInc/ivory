{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances,
    TypeOperators, DataKinds, EmptyCase, NamedFieldPuns,
    TemplateHaskell, QuasiQuotes, ViewPatterns, RankNTypes, KindSignatures #-}

module Ivory.ModelCheck.Logic2SimpleSMT where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Bits
import Data.Typeable
import Numeric
import Numeric.Natural
import Text.PrettyPrint
import Data.Ratio((%), numerator, denominator)
import Text.Read (readMaybe)
import Data.List

import MonadLib

import Data.Binding.Hobbits

import qualified SimpleSMT as SMT

import Ivory.ModelCheck.Logic

import Debug.Trace


----------------------------------------------------------------------
-- SMT expression types
----------------------------------------------------------------------

-- The representation of sorts in SimpleSMT
newtype SMTSort a = SMTSort { unSMTSort :: SMT.SExpr }
                  deriving Show

-- | Get the 'SMTSort' for an 'L1Type'
l1type_to_sort :: L1Type a -> SMTSort a
l1type_to_sort (L1Type_lit LitType_unit) =
  -- NOTE: we use the type of 1-bit vectors for the unit type, for lack of an
  -- appropriate unit type...
  SMTSort $ SMT.tBits 1
l1type_to_sort (L1Type_lit LitType_bool) = SMTSort SMT.tBool
l1type_to_sort (L1Type_lit LitType_int) = SMTSort SMT.tInt
l1type_to_sort (L1Type_lit LitType_rat) = SMTSort SMT.tReal
l1type_to_sort (L1Type_lit (LitType_bits :: LitType a)) =
  SMTSort $ SMT.tBits $ toInteger $ finiteBitSize (zeroBits :: a)
l1type_to_sort L1Type_prop = SMTSort SMT.tBool
l1type_to_sort L1Type_ptr =
  -- NOTE: we represent pointers as integers
  SMTSort SMT.tInt

-- | Get the input and output sorts for a first-order function type
l1funType_to_sorts :: L1FunType a -> (MapList SMTSort (ArgTypes a),
                                      SMTSort (RetType a))
l1funType_to_sorts (L1FunType_base l1tp@(L1Type_lit _)) =
  (Nil, l1type_to_sort l1tp)
l1funType_to_sorts (L1FunType_base l1tp@L1Type_ptr) =
  (Nil, l1type_to_sort l1tp)
l1funType_to_sorts (L1FunType_base l1tp@L1Type_prop) =
  (Nil, l1type_to_sort l1tp)
l1funType_to_sorts (L1FunType_cons l1tp@(L1Type_lit _) ftp) =
  let (arg_tps, ret_tp) = l1funType_to_sorts ftp in
  (Cons (l1type_to_sort l1tp) arg_tps, ret_tp)
l1funType_to_sorts (L1FunType_cons l1tp@L1Type_ptr ftp) =
  let (arg_tps, ret_tp) = l1funType_to_sorts ftp in
  (Cons (l1type_to_sort l1tp) arg_tps, ret_tp)
l1funType_to_sorts (L1FunType_cons l1tp@L1Type_prop ftp) =
  let (arg_tps, ret_tp) = l1funType_to_sorts ftp in
  (Cons (l1type_to_sort l1tp) arg_tps, ret_tp)

-- | Get the input and output sorts for a first-order function type as
-- s-expressions
l1funType_to_sorts_sexpr :: L1FunType a -> ([SMT.SExpr], SMT.SExpr)
l1funType_to_sorts_sexpr (L1FunType_base l1tp) =
  ([], unSMTSort $ l1type_to_sort l1tp)
l1funType_to_sorts_sexpr (L1FunType_cons l1tp ftp) =
  let (arg_tps, ret_tp) = l1funType_to_sorts_sexpr ftp in
  (unSMTSort (l1type_to_sort l1tp) : arg_tps, ret_tp)

-- | Return a base name for each type, to be used for variables
l1type_base_name :: L1Type a -> String
l1type_base_name (L1Type_lit LitType_unit) = "u"
l1type_base_name (L1Type_lit LitType_bool) = "b"
l1type_base_name (L1Type_lit LitType_int) = "i"
l1type_base_name (L1Type_lit LitType_rat) = "r"
l1type_base_name (L1Type_lit LitType_bits) = "bv"
l1type_base_name L1Type_prop = "phi"
l1type_base_name L1Type_ptr = "ptr"


----------------------------------------------------------------------
-- Typed SMT expressions
----------------------------------------------------------------------

-- | Typed SMT expressions are just s-expressions with an associated type
newtype SMTExpr a = SMTExpr { unSMTExpr :: SMT.SExpr }
                  deriving Show

-- | Pretty-print an s-expression
ppSExpr :: SMT.SExpr -> String
ppSExpr sexpr = render $ helper sexpr where
  helper :: SMT.SExpr -> Doc
  helper (SMT.Atom str) = text str
  helper (SMT.List sexprs) =
    text "(" <> nest 2 (fsep $ map helper sexprs) <> text ")"

-- | Pretty-print an 'SMTExpr'
ppExpr :: SMTExpr a -> String
ppExpr (SMTExpr sexpr) = ppSExpr sexpr

-- | Lift a unary function on 'SExpr's to one on 'SMTExpr's. NOTE: be careful
-- that you do not ruin type-safety with this...
unarySMT :: (SMT.SExpr -> SMT.SExpr) -> SMTExpr a -> SMTExpr b
unarySMT f (SMTExpr sexpr) = SMTExpr $ f sexpr

-- | Lift a binary function on 'SExpr's to one on 'SMTExpr's. NOTE: be careful
-- that you do not ruin type-safety with this...
binarySMT :: (SMT.SExpr -> SMT.SExpr -> SMT.SExpr) ->
             SMTExpr a -> SMTExpr b -> SMTExpr c
binarySMT f (SMTExpr sexpr1) (SMTExpr sexpr2) = SMTExpr $ f sexpr1 sexpr2

-- | Turn any literal into an 'SMTExpr'
smtLiteral :: LitType a -> a -> SMTExpr (Literal a)
smtLiteral LitType_unit () = SMTExpr $ SMT.bvBin 1 0
smtLiteral LitType_bool b = SMTExpr $ SMT.bool b
smtLiteral LitType_int i = SMTExpr $ SMT.int i
smtLiteral LitType_rat r = SMTExpr $ SMT.real r
smtLiteral (LitType_bits :: LitType a) bv =
  SMTExpr $ SMT.bvHex (finiteBitSize (zeroBits :: a)) (toInteger bv)

-- | The 'SMTExpr' for the null pointer
smtNullPtr :: SMTExpr Ptr
smtNullPtr = SMTExpr $ SMT.int 0

-- | Build a zero object of a given type
smtZeroOfType :: L1Type a -> SMTExpr a
smtZeroOfType (L1Type_lit lit_tp@LitType_unit) = smtLiteral lit_tp ()
smtZeroOfType (L1Type_lit lit_tp@LitType_bool) = smtLiteral lit_tp False
smtZeroOfType (L1Type_lit lit_tp@LitType_int) = smtLiteral lit_tp 0
smtZeroOfType (L1Type_lit lit_tp@LitType_rat) = smtLiteral lit_tp 0
smtZeroOfType (L1Type_lit lit_tp@LitType_bits) = smtLiteral lit_tp 0
smtZeroOfType L1Type_ptr = smtNullPtr
smtZeroOfType L1Type_prop = smtFalse

-- | Build a "1" object of a given type, if possible
smtOneOfType :: LitType a -> SMTExpr (Literal a)
smtOneOfType (lit_tp@LitType_unit) = smtLiteral lit_tp ()
smtOneOfType (lit_tp@LitType_bool) = smtLiteral lit_tp True
smtOneOfType (lit_tp@LitType_int) = smtLiteral lit_tp 1
smtOneOfType (lit_tp@LitType_rat) = smtLiteral lit_tp 1
smtOneOfType (lit_tp@LitType_bits) = smtLiteral lit_tp 1

-- | The 'SMTExpr' for a global variable pointer. NOTE: global variables are
-- represented as *negative* numbers.
smtGlobalVar :: Natural -> SMTExpr Ptr
smtGlobalVar n = SMTExpr $ SMT.int (- (toInteger n))

-- | The 'SMTExpr' for the next pointer
smtNextPtr :: SMTExpr Ptr -> SMTExpr Ptr
smtNextPtr (SMTExpr sexpr) = SMTExpr $ SMT.add sexpr (SMT.int 1)

-- | The absolute value function on 'SMTExpr's
smtAbs :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a)
smtAbs _ _ = error "SMT absolute value function not (yet?) supported"

-- | The signum function on 'SMTExpr's
smtSignum :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a)
smtSignum _ _ = error "SMT signum function not (yet?) supported"

-- | The negation function on 'SMTExpr's
smtNeg :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a)
smtNeg LitType_unit = id
smtNeg LitType_bool = unarySMT SMT.not
smtNeg LitType_int = unarySMT SMT.neg
smtNeg LitType_rat = unarySMT SMT.neg
smtNeg LitType_bits = unarySMT SMT.bvNeg

-- | The bit complementation function on 'SMTExpr's
smtComplement :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a)
smtComplement LitType_bits = unarySMT SMT.bvNot
smtComplement _ =
  error "SMT complementation function called on non-bitvector value!"

-- | The increment-by-1 function on 'SMTExpr's
smtIncr :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a)
smtIncr lit_tp e = smtAdd lit_tp e (smtOneOfType lit_tp)

-- | The decrement-by-1 function on 'SMTExpr's
smtDecr :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a)
smtDecr lit_tp e = smtAdd lit_tp e (smtOneOfType lit_tp)

-- | Apply an 'ArithOp1' to an 'SMTExpr'
smtArithOp1 :: ArithOp1 -> LitType a -> SMTExpr (Literal a) ->
               SMTExpr (Literal a)
smtArithOp1 Op1_Abs = smtAbs
smtArithOp1 Op1_Signum = smtSignum
smtArithOp1 Op1_Neg = smtNeg
smtArithOp1 Op1_Complement = smtComplement
smtArithOp1 Op1_Incr = smtIncr
smtArithOp1 Op1_Decr = smtDecr

-- | The binary addition function on 'SMTExpr's
smtAdd :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a) ->
          SMTExpr (Literal a)
smtAdd LitType_unit = \_ -> id
smtAdd LitType_bool = binarySMT SMT.or
smtAdd LitType_int = binarySMT SMT.add
smtAdd LitType_rat = binarySMT SMT.add
smtAdd LitType_bits = binarySMT SMT.bvAdd

-- | The binary subtraction function on 'SMTExpr's
smtSub :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a) ->
          SMTExpr (Literal a)
smtSub LitType_unit = \_ -> id
smtSub LitType_bool = error "SMT conversion: subtraction on Booleans!"
smtSub LitType_int = binarySMT SMT.sub
smtSub LitType_rat = binarySMT SMT.sub
smtSub LitType_bits = binarySMT SMT.bvSub

-- | The binary multiplication function on 'SMTExpr's
smtMult :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a) ->
           SMTExpr (Literal a)
smtMult LitType_unit = \_ -> id
smtMult LitType_bool = binarySMT SMT.and
smtMult LitType_int = binarySMT SMT.mul
smtMult LitType_rat = binarySMT SMT.mul
smtMult LitType_bits = binarySMT SMT.bvMul

-- | The binary division function on 'SMTExpr's
smtDiv :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a) ->
          SMTExpr (Literal a)
smtDiv LitType_unit = \_ -> id
smtDiv LitType_bool = error "SMT conversion: division on Booleans!"
smtDiv LitType_int = binarySMT SMT.div
smtDiv LitType_rat = binarySMT SMT.realDiv
smtDiv (LitType_bits :: LitType bv)
  | isSigned (0 :: bv) = binarySMT SMT.bvSDiv
smtDiv (LitType_bits :: LitType bv) = binarySMT SMT.bvUDiv

-- | The binary modulo function on 'SMTExpr's
smtMod :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a) ->
          SMTExpr (Literal a)
smtMod LitType_unit = \_ -> id
smtMod LitType_bool = error "SMT conversion: modulus on Booleans!"
smtMod LitType_int = binarySMT SMT.mod
smtMod LitType_rat = error "SMT convertion: modulus on rationals!"
smtMod (LitType_bits :: LitType bv)
  | isSigned (0 :: bv) = binarySMT SMT.bvSRem
smtMod (LitType_bits :: LitType bv) = binarySMT SMT.bvURem

-- | The bitwise and function on 'SMTExpr's
smtBitAnd :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a) ->
             SMTExpr (Literal a)
smtBitAnd LitType_bits = binarySMT SMT.bvAnd
smtBitAnd _ = error "SMT bitwise and function called on non-bitvector values!"

-- | The bitwise and function on 'SMTExpr's
smtBitOr :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a) ->
            SMTExpr (Literal a)
smtBitOr LitType_bits = binarySMT SMT.bvOr
smtBitOr _ = error "SMT bitwise or function called on non-bitvector values!"

-- | The bitwise xor function on 'SMTExpr's
smtBitXor :: LitType a -> SMTExpr (Literal a) -> SMTExpr (Literal a) ->
             SMTExpr (Literal a)
smtBitXor LitType_bits = binarySMT SMT.bvXOr
smtBitXor _ = error "SMT bitwise xor function called on non-bitvector values!"

-- | Apply an 'ArithOp2' to an 'SMTExpr'
smtArithOp2 :: ArithOp2 -> LitType a -> SMTExpr (Literal a) ->
               SMTExpr (Literal a) -> SMTExpr (Literal a)
smtArithOp2 Op2_Add = smtAdd
smtArithOp2 Op2_Sub = smtSub
smtArithOp2 Op2_Mult = smtMult
smtArithOp2 Op2_Div = smtDiv
smtArithOp2 Op2_Mod = smtMod
smtArithOp2 Op2_BitAnd = smtBitAnd
smtArithOp2 Op2_BitOr = smtBitOr
smtArithOp2 Op2_BitXor = smtBitXor

-- | The equality function on 'SMTExpr's with Boolean type
smtEqBool :: SMTExpr a -> SMTExpr a -> SMTExpr (Literal Bool)
smtEqBool (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.eq sexpr1 sexpr2

-- | The less-than function on 'SMTExpr's
smtLt :: L1Type a -> SMTExpr a -> SMTExpr a -> SMTExpr (Literal Bool)
smtLt (L1Type_lit LitType_unit) _ _ =
  error "SMT less-than function called on unit values!"
smtLt (L1Type_lit LitType_bool) _ _ =
  error "SMT less-than function called on Boolean values!"
smtLt (L1Type_lit LitType_int) (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.lt sexpr1 sexpr2
smtLt (L1Type_lit LitType_rat) (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.lt sexpr1 sexpr2
smtLt (L1Type_lit (LitType_bits :: LitType bv)) (SMTExpr sexpr1) (SMTExpr sexpr2)
  | isSigned (0 :: bv) = SMTExpr $ SMT.bvSLt sexpr1 sexpr2
smtLt (L1Type_lit LitType_bits) (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.bvULt sexpr1 sexpr2
smtLt L1Type_ptr (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.lt sexpr1 sexpr2
smtLt L1Type_prop _ _ =
  error "SMT less-than function called on propositional values!"

-- | The less-than-or-equal-to function on 'SMTExpr's
smtLe :: L1Type a -> SMTExpr a -> SMTExpr a -> SMTExpr (Literal Bool)
smtLe (L1Type_lit LitType_unit) _ _ =
  error "SMT less-than function called on unit values!"
smtLe (L1Type_lit LitType_bool) _ _ =
  error "SMT less-than function called on Boolean values!"
smtLe (L1Type_lit LitType_int) (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.leq sexpr1 sexpr2
smtLe (L1Type_lit LitType_rat) (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.leq sexpr1 sexpr2
smtLe (L1Type_lit (LitType_bits :: LitType bv)) (SMTExpr sexpr1) (SMTExpr sexpr2)
  | isSigned (0 :: bv) = SMTExpr $ SMT.bvSLeq sexpr1 sexpr2
smtLe (L1Type_lit LitType_bits) (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.bvULeq sexpr1 sexpr2
smtLe L1Type_ptr (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.leq sexpr1 sexpr2
smtLe L1Type_prop _ _ =
  error "SMT less-than-or-equal-to function called on propositional values!"

-- | Apply an 'ArithCmp' to two 'SMTExpr's
smtArithCmp :: ArithCmp -> L1Type a -> SMTExpr a -> SMTExpr a ->
               SMTExpr (Literal Bool)
smtArithCmp OpCmp_EQ = \_ -> smtEqBool
smtArithCmp OpCmp_LT = smtLt
smtArithCmp OpCmp_LE = smtLe

-- | Apply a coercion function from one 'LitType' to another to an 'SMTExpr'
smtCoerce :: LitType f -> LitType t -> SMTExpr (Literal f) ->
             SMTExpr (Literal t)
smtCoerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) (SMTExpr sexpr)
  | finiteBitSize (0 :: t) == finiteBitSize (0 :: f) =
    SMTExpr sexpr
smtCoerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) (SMTExpr sexpr)
  | isSigned (0 :: f) && finiteBitSize (0 :: t) > finiteBitSize (0 :: f) =
    SMTExpr $
    SMT.signExtend (toInteger $
                    finiteBitSize (0 :: t) - finiteBitSize (0 :: f)) sexpr
smtCoerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) (SMTExpr sexpr)
  | finiteBitSize (0 :: t) > finiteBitSize (0 :: f) =
    SMTExpr $
    SMT.zeroExtend (toInteger $
                    finiteBitSize (0 :: t) - finiteBitSize (0 :: f)) sexpr
smtCoerce (LitType_bits :: LitType f) (LitType_bits :: LitType t) (SMTExpr sexpr) =
  SMTExpr $ SMT.extract sexpr (toInteger $ finiteBitSize (0 :: t) - 1) 0
smtCoerce LitType_int (LitType_bits :: LitType t) (SMTExpr _) =
  error "smtCoerce: integer to bit-vector conversion not (yet?) supported"
  --AST <$> Z3.mkInt2bv (finiteBitSize (0 :: t)) sexpr
smtCoerce (LitType_bits :: LitType f) LitType_int (SMTExpr _) =
  error "smtCoerce: bit-vector to integer conversion not (yet?) supported"
  --AST <$> Z3.mkBv2int sexpr (isSigned (0 :: f))
smtCoerce LitType_int LitType_int e = e
smtCoerce LitType_bool LitType_bool e = e
smtCoerce LitType_unit LitType_unit e = e
smtCoerce LitType_bool lit_tp@LitType_bits (SMTExpr sexpr) =
  -- Conversion from Bool -> bit-vector: (ite sexpr #b1 #b0)
  SMTExpr $ SMT.ite sexpr (unSMTExpr $ smtLiteral lit_tp 1)
  (unSMTExpr $ smtLiteral lit_tp 0)
smtCoerce lit_tp@LitType_bits LitType_bool (SMTExpr sexpr) =
  -- Conversion from bit-vector -> Bool: (= (0th bit of sexpr) #b1)
  SMTExpr $ SMT.eq (SMT.extract sexpr 0 0) (SMT.bvBin 1 0)
smtCoerce lit_tp_from lit_tp_to _ =
  error ("smtCoerce: coercion not supported from " ++ show lit_tp_from
         ++ " to " ++ show lit_tp_to)

-- | Build an if-then-else expression
smtIfThenElse :: SMTExpr (Literal Bool) -> SMTExpr a -> SMTExpr a -> SMTExpr a
smtIfThenElse (SMTExpr sexpr_cond) (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.ite sexpr_cond sexpr1 sexpr2

-- | The "true" 'SMTExpr'
smtTrue :: SMTExpr Prop
smtTrue = SMTExpr $ SMT.bool True

-- | The "false" 'SMTExpr'
smtFalse :: SMTExpr Prop
smtFalse = SMTExpr $ SMT.bool False

-- | Build a conjunction
smtAnd :: SMTExpr Prop -> SMTExpr Prop -> SMTExpr Prop
smtAnd = binarySMT SMT.and

-- | Build a disjunction
smtOr :: SMTExpr Prop -> SMTExpr Prop -> SMTExpr Prop
smtOr = binarySMT SMT.or

-- | Build a negation
smtNot :: SMTExpr Prop -> SMTExpr Prop
smtNot = unarySMT SMT.not

-- | The equality function on 'SMTExpr's
smtEq :: SMTExpr a -> SMTExpr a -> SMTExpr Prop
smtEq (SMTExpr sexpr1) (SMTExpr sexpr2) =
  SMTExpr $ SMT.eq sexpr1 sexpr2

-- | Convert a Boolean 'SMTExpr' to a propositional one
smtIsTrue :: SMTExpr (Literal Bool) -> SMTExpr Prop
smtIsTrue (SMTExpr sexpr1) = SMTExpr sexpr1

-- | Fully apply an 'SMTExpr' to arguments for it
smtApply :: SMTExpr a -> LTypeArgs a args ret -> MapList SMTExpr args ->
            SMTExpr ret
smtApply f (LTypeArgs_base _) _ = f
smtApply f (LTypeArgs_pm _) _ = f
smtApply (SMTExpr f_sexpr) tp_args args =
  SMTExpr $ SMT.List (f_sexpr : arg_sexprs tp_args args)
  where
    arg_sexprs :: LTypeArgs a args ret -> MapList SMTExpr args -> [SMT.SExpr]
    arg_sexprs (LTypeArgs_base _) _ = []
    arg_sexprs (LTypeArgs_pm _) _ = []
    arg_sexprs (LTypeArgs_fun _ tp_args') args' =
      unSMTExpr (ml_first args') : arg_sexprs tp_args' (ml_rest args')

-- | Assert a formula
smtAssert :: SMTExpr Prop -> SMTm ctx ()
smtAssert expr =
  smtDebug 2 ("SMT: asserting proposition:\n" ++ ppExpr expr) >>
  put [expr]

-- | Build a universal quantifier around a computation of an 'SMTExpr'
smtForall :: L1Type a -> SMTm (ctx ':> a) (SMTExpr Prop) ->
             SMTm ctx (SMTExpr Prop)
smtForall l1tp body_m =
  do (var_str, body_expr) <- withUVar l1tp body_m
     let sort = l1type_to_sort l1tp
     return $ SMTExpr $
       SMT.List [SMT.Atom "forall",
                 SMT.List [SMT.List [SMT.Atom var_str, unSMTSort sort]],
                 unSMTExpr body_expr]

-- | Build an existential quantifier around a computation of an 'SMTExpr'
smtExists :: L1Type a -> SMTm (ctx ':> a) (SMTExpr Prop) ->
             SMTm ctx (SMTExpr Prop)
smtExists l1tp body_m =
     snd <$> withEVar l1tp body_m

-- | Build a let-binding around a computation of an 'SMTExpr'
smtLet :: L1Type a -> SMTExpr a -> SMTm (ctx ':> a) (SMTExpr Prop) ->
          SMTm ctx (SMTExpr Prop)
smtLet l1tp rhs body_m =
    do (var_expr, body_expr) <- withEVar l1tp body_m
       smtAssert $ smtEq var_expr rhs
       return body_expr


----------------------------------------------------------------------
-- SMT variable contexts
----------------------------------------------------------------------

-- | The type of SMT contexts, that say how to translate free variables into
-- SMT. Specifically, an SMT context of type @'SMTCtx' ctx@ indicates, for each
-- variable type listed in @ctx@, whether it is an existential or universal
-- variable, as well as what its 'String' name and type are.
data SMTCtx ctx where
  SMTCtx_EVar :: SMTCtx ctx -> L1Type a -> SMTExpr a -> SMTCtx (ctx ':> a)
  -- ^ Add an existential variable to the context, which is associated with a
  -- constant symbol, given as an 'SMTExpr'
  SMTCtx_UVar :: SMTCtx ctx -> L1Type a -> String -> SMTCtx (ctx ':> a)
  -- ^ Add a universal variable to the context, given by name
  SMTCtx_FVar :: SMTCtx ctx -> L1FunType (a -> b) -> String ->
                 SMTCtx (ctx ':> (a -> b))
  -- ^ Add an existential function variable to the context
  SMTCtx_nil :: SMTCtx 'RNil
  -- ^ The empty SMT context

-- | Look up a variable in an 'SMTCtx' and return it, fully applied to the given
-- arguments
smtVarLookup :: SMTCtx ctx -> Member ctx a -> LTypeArgs a args ret ->
                MapList SMTExpr args -> SMTExpr ret
smtVarLookup (SMTCtx_EVar _ _ var_expr) Member_Base tp_args args =
  smtApply var_expr tp_args args
smtVarLookup (SMTCtx_UVar _ _ var_str) Member_Base tp_args args =
  smtApply (SMTExpr $ SMT.Atom var_str) tp_args args
smtVarLookup (SMTCtx_FVar _ _ fun_str) Member_Base tp_args args =
  smtApply (SMTExpr $ SMT.Atom fun_str) tp_args args
smtVarLookup (SMTCtx_EVar ctx' _ _) (Member_Step memb) tp_args args =
  smtVarLookup ctx' memb tp_args args
smtVarLookup (SMTCtx_UVar ctx' _ _) (Member_Step memb) tp_args args =
  smtVarLookup ctx' memb tp_args args
smtVarLookup (SMTCtx_FVar ctx' _ _) (Member_Step memb) tp_args args =
  smtVarLookup ctx' memb tp_args args

-- | Build an 'SMTExpr' for a variable
smtVar :: Member ctx a -> LTypeArgs a args ret ->
          MapList SMTExpr args -> SMTm ctx (SMTExpr ret)
smtVar memb tp_args args =
  do ctx <- ask
     return $ smtVarLookup ctx memb tp_args args


----------------------------------------------------------------------
-- Monad for interacting with SMT
----------------------------------------------------------------------

-- | State information used in constructing SMT terms
data SMTInfo =
  SMTInfo
  {
    -- | Index used to create fresh constants and function symbols
    fresh_id :: Integer,
    -- | The debugging level
    smt_debug_level :: Int,
    -- | The SMT solver being used
    smt_solver :: SMT.Solver
  }

-- | Build an 'SMTInfo'
mkSMTInfo :: Int -> SMT.Solver -> SMTInfo
mkSMTInfo smt_debug_level smt_solver =
  SMTInfo { fresh_id = 1, smt_debug_level, smt_solver }

-- | Monad for building SMT expressions relative to a given variable
-- context. The monad uses the state information in a 'SMTInfo', reads an
-- 'SMTCtx' for the current variable context, and also outputs a set of
-- assertions, given as type 'SMTExpr', to pass to the current solver.
newtype SMTm ctx a =
  SMTm { runSMTm :: ReaderT (SMTCtx ctx)
                    (StateT (SMTInfo, [SMTExpr Prop]) IO) a }
  deriving (Functor, Applicative, Monad)

instance ReaderM (SMTm ctx) (SMTCtx ctx) where
  ask = SMTm $ ask

instance RunReaderM (SMTm ctx) (SMTCtx ctx) where
  local ctx (SMTm m) = SMTm $ local ctx m

localCtx :: SMTCtx ctx -> SMTm ctx a -> SMTm ctx' a
localCtx ctx (SMTm m) = SMTm $ lift $ runReaderT ctx m

instance StateM (SMTm ctx) SMTInfo where
  get = SMTm $ (\(info, _) -> info) <$> get
  set info = SMTm $ sets_ $ \(_, props) -> (info, props)

getSolver :: SMTm ctx SMT.Solver
getSolver = smt_solver <$> get

instance WriterM (SMTm ctx) [SMTExpr Prop] where
  put props =
    SMTm $ do (info, props') <- get
              set (info, props' ++ props)

instance RunWriterM (SMTm ctx) [SMTExpr Prop] where
  collect (SMTm m) =
    SMTm $ do (info, orig_props) <- get
              set (info, [])
              ret <- m
              (info', props') <- get
              set (info', orig_props)
              return (ret, props')

instance BaseM (SMTm ctx) IO where
  inBase m = SMTm $ lift $ lift m

-- | Conditionally perform a computation if the debug level is at least the
-- indicated level
smtIfDebug :: Int -> SMTm ctx () -> SMTm ctx ()
smtIfDebug level m =
  do cur_level <- smt_debug_level <$> get
     if cur_level >= level then m else return ()

-- | Conditionally print a string to stdout if the debug level of the given
-- solver is at least the indicated level
smtDebug :: Int -> String -> SMTm ctx ()
smtDebug level str =
  smtIfDebug level $ inBase (putStrLn str)

instance RunM (SMTm ctx) a (SMT.Solver -> Int ->
                            MapRList L1FunType ctx -> IO a) where
  runM m solver debug_level tps =
    liftM fst $ runStateT (mkSMTInfo debug_level solver, []) $
    runReaderT SMTCtx_nil $
    runSMTm (mkSMTCtx tps >>= \ctx -> localCtx ctx m)


----------------------------------------------------------------------
-- Monadic operations on SMT contexts
----------------------------------------------------------------------

-- | Make a fresh variable name with base name @str@
mkFreshSymbol :: String -> SMTm ctx String
mkFreshSymbol basename =
  do info <- get
     let i = fresh_id info
     set $ info { fresh_id = i+1 }
     return $ basename ++ show i

-- | Make a fresh variable name for a function type
mkFreshFunSymbol :: L1FunType a -> SMTm ctx String
mkFreshFunSymbol ftp =
  mkFreshSymbol ("f_" ++ l1type_base_name (funType_ret_type ftp))

-- | Make a fresh constant as an 'SMTExpr' from a first-order type
mkFreshConstant :: L1Type a -> SMTm ctx (SMTExpr a)
mkFreshConstant l1tp =
  do symb <- mkFreshSymbol (l1type_base_name l1tp)
     solver <- getSolver
     SMTExpr <$> inBase (SMT.declare solver symb
                         (unSMTSort $ l1type_to_sort l1tp))

-- | Make a fresh function from a first-order function type
mkFreshFun :: L1FunType a -> SMTm ctx String
mkFreshFun ftp =
  do symb <- mkFreshFunSymbol ftp
     let (arg_sorts, ret_sort) = l1funType_to_sorts_sexpr ftp
     solver <- getSolver
     _ <- inBase (SMT.declareFun solver symb arg_sorts ret_sort)
     return symb

{-
-- | Assert a "dummy formula" for an SMTExpr, by creating a fresh dummy variable
-- and asserting the formula dummy \/ ast = 0
assertDummyFormula :: L1Type a -> SMTExpr a -> SMTm ctx ()
assertDummyFormula l1tp expr =
  do sym <- mkFreshSymbol "dummy"
     dummy <- SMTExpr <$> declare solver sym (l1type_to_sort L1Type_prop)
     smtAssert $ smtOr dummy (smtEq expr (smtZeroOfType l1tp))

-- | Make a fresh constant as an 'SMTExpr' from a first-order type, and assert a
-- "dummy formula" to ensure that it ends up in the model
mkFreshConstant :: L1Type a -> SMTm ctx (SMTExpr a)
mkFreshConstant l1tp = do
  ast <- mkFreshConstantRaw l1tp
  --assertDummyFormula l1tp ast
  return ast

-- | Assert a "dummy formula" for a function f, by creating a fresh dummy
-- variable and asserting the formula dummy \/ f 0 ... 0 = 0
assertDummyFormulaFun :: L1FunType a -> SMTExpr a -> SMTm ctx ()
assertDummyFormulaFun ftp f = helper ftp (unSMTExpr f) [] where
  helper :: L1FunType a -> SExpr -> [SExpr] -> SMTm ctx ()
  helper (L1FunType_base l1tp) f_sexpr args =
    assertDummyFormula l1tp (SMTExpr $ List (fdecl : reverse args))
  helper (L1FunType_cons l1tp ftp') f_sexpr args =
    helper ftp' f_sexpr (unSMTExpr (smtZeroOfType l1tp) : args)

-- | Make a fresh function declaration as a 'SMT.FuncDecl' from a first-order
-- function type, and assert a "dummy formula" to ensure it ends up in the model
mkFreshFun :: L1FunType a -> SMTm ctx (SMTExpr a)
mkFreshFun ftp = do
  fdecl <- mkFreshFuncDeclRaw ftp
  --assertDummyFormulaFDecl ftp fdecl
  return fdecl
-}

-- | Run an 'SMTm' computation in a context extended with a universal variable
withUVar :: L1Type a -> SMTm (ctx ':> a) b -> SMTm ctx (String, b)
withUVar l1tp m =
  do ctx <- ask
     symb <- mkFreshSymbol (l1type_base_name l1tp)
     ret <- localCtx (SMTCtx_UVar ctx l1tp symb) m
     return (symb, ret)

-- | Run an 'SMTm' computation in the context extended with an existential
-- variable, modeled with a fresh constant. NOTE: this is not allowed inside the
-- context of a universal, as this would require a fresh constant for each of
-- the (potentially infinitely many) possible instantiations for this universal
-- variable. Return both the computation result and the fresh constant.
withEVar :: L1Type a -> SMTm (ctx ':> a) b -> SMTm ctx (SMTExpr a, b)
withEVar l1tp m =
  do ctx <- ask
     errorIfUniversal ctx
     const_expr <- mkFreshConstant l1tp
     ret <- localCtx (SMTCtx_EVar ctx l1tp const_expr) m
     return (const_expr, ret)
       where
         errorIfUniversal :: SMTCtx ctx' -> SMTm ctx ()
         errorIfUniversal (SMTCtx_UVar _ _ _) =
           error "withEVar: attempt to build an existential inside a forall!"
         errorIfUniversal (SMTCtx_EVar ctx _ _) =
           errorIfUniversal ctx
         errorIfUniversal (SMTCtx_FVar ctx _ _) =
           errorIfUniversal ctx
         errorIfUniversal SMTCtx_nil = return ()

-- | Build an existential 'SMTCtx' from a list of first-order types
mkSMTCtx :: MapRList L1FunType ctx -> SMTm c (SMTCtx ctx)
mkSMTCtx (tps :>: L1FunType_base l1tp) =
  do ctx <- mkSMTCtx tps
     const_expr <- mkFreshConstant l1tp
     return $ SMTCtx_EVar ctx l1tp const_expr
mkSMTCtx (tps :>: ftp@(L1FunType_cons _ _)) =
  liftM3 SMTCtx_FVar (mkSMTCtx tps) (return ftp) (mkFreshFun ftp)
mkSMTCtx MNil = return SMTCtx_nil


----------------------------------------------------------------------
-- Converting from our reachability logic into SMT
----------------------------------------------------------------------

-- | Helper type for an 'SMTExpr' inside a 'SMTm' computation
newtype SMTm_Expr ctx a = SMTm_Expr { unSMTm_Expr :: SMTm ctx (SMTExpr a) }

-- | Helper function for extracting an 'SMTExpr' from a typed argument passed to
-- 'interpOpB' or 'interpVarB' in a 'BindingApply'
extractL1Arg :: L1Type a -> BindingApply SMTm_Expr ctx a -> SMTm ctx (SMTExpr a)
extractL1Arg l1tp arg =
  unSMTm_Expr $ elimL1BindingApplyF l1tp $ unBindingApply arg

-- | Extract a list of SMT expressions from a list of typed arguments passed to
-- 'interpVarB' in 'BindingApply's
extractVarArgs :: LTypeArgs a args ret ->
                  MapList (BindingApply SMTm_Expr ctx) args ->
                  SMTm ctx (MapList SMTExpr args)
extractVarArgs (LTypeArgs_base _) _ = return Nil
extractVarArgs (LTypeArgs_pm _) _ = return Nil
extractVarArgs (LTypeArgs_fun (LType_base l1tp) tp_args) args =
  do expr <- extractL1Arg l1tp (ml_first args)
     exprs <- extractVarArgs tp_args (ml_rest args)
     return $ Cons expr exprs
extractVarArgs (LTypeArgs_fun _ _) _ =
  error "extractVarArgs: converting a higher-order variable to SMT!"

-- | Helper function for extracting an 'SMTExpr' from an argument of type
-- @'Literal' a@ passed to 'interpOpB' or 'interpVarB'
extractLitArg :: BindingApply SMTm_Expr ctx (Literal a) ->
                 SMTm ctx (SMTExpr (Literal a))
extractLitArg arg = unSMTm_Expr $ unBindingApply arg

-- | Helper function for extracting a 'SMTExpr' from an argument of type 'Ptr'
-- passed to 'interpOpB' or 'interpVarB'
extractPtrArg :: BindingApply SMTm_Expr ctx Ptr -> SMTm ctx (SMTExpr Ptr)
extractPtrArg arg = unSMTm_Expr $ unBindingApply arg

-- | Helper function for extracting a 'SMTExpr' from an argument of type 'Prop'
-- passed to 'interpOpB' or 'interpVarB'
extractPropArg :: BindingApply SMTm_Expr ctx Prop -> SMTm ctx (SMTExpr Prop)
extractPropArg arg = unSMTm_Expr $ unBindingApply arg

-- Instance for converting LExprs to SMT expressions
instance LBindingExprAlgebra tag SMTm_Expr where

  -- Interpret a variable, given as a Member in the current context, into SMT
  interpVarB _ tp_args memb args =
    SMTm_Expr $ do smt_args <- extractVarArgs tp_args args
                   smtVar memb tp_args smt_args

  -- Interpret literals into SMT
  interpOpB (Op_Literal lit_tp x) _ =
    SMTm_Expr $ return $ smtLiteral lit_tp x

  -- Interpret the arithmetic operations into SMT
  interpOpB (Op_arith1 lit_tp aop) (ml_first -> arg1) =
    SMTm_Expr $ do ast1 <- extractLitArg arg1
                   return $ smtArithOp1 aop lit_tp ast1
  interpOpB (Op_arith2 lit_tp aop) (ml_12 -> (arg1, arg2)) =
    SMTm_Expr $ do ast1 <- extractLitArg arg1
                   ast2 <- extractLitArg arg2
                   return $ smtArithOp2 aop lit_tp ast1 ast2
  interpOpB (Op_coerce lit_tp_from lit_tp_to) (ml_first -> arg1) =
    SMTm_Expr $ do ast1 <- extractLitArg arg1
                   return $ smtCoerce lit_tp_from lit_tp_to ast1
  interpOpB (Op_cmp lit_tp acmp) (ml_12 -> (arg1, arg2)) =
    SMTm_Expr $ do ast1 <- extractLitArg arg1
                   ast2 <- extractLitArg arg2
                   return $ smtArithCmp acmp (L1Type_lit lit_tp) ast1 ast2
  interpOpB (Op_cond l1tp@(L1Type_lit _)) (ml_123 -> (arg1, arg2, arg3)) =
    SMTm_Expr $ do ast1 <- extractLitArg arg1
                   ast2 <- extractL1Arg l1tp arg2
                   ast3 <- extractL1Arg l1tp arg3
                   return $ smtIfThenElse ast1 ast2 ast3
  interpOpB (Op_cond l1tp@L1Type_ptr) (ml_123 -> (arg1, arg2, arg3)) =
    SMTm_Expr $ do ast1 <- extractLitArg arg1
                   ast2 <- extractL1Arg l1tp arg2
                   ast3 <- extractL1Arg l1tp arg3
                   return $ smtIfThenElse ast1 ast2 ast3
  interpOpB (Op_cond l1tp@L1Type_prop) (ml_123 -> (arg1, arg2, arg3)) =
    SMTm_Expr $ do ast1 <- extractLitArg arg1
                   ast2 <- extractL1Arg l1tp arg2
                   ast3 <- extractL1Arg l1tp arg3
                   return $ smtIfThenElse ast1 ast2 ast3
  interpOpB Op_null_ptr _ = SMTm_Expr $ return smtNullPtr
  interpOpB (Op_global_var n) _ = SMTm_Expr $ return $ smtGlobalVar n
  interpOpB Op_next_ptr (ml_first -> arg1) =
    SMTm_Expr $ do ast1 <- extractPtrArg arg1
                   return $ smtNextPtr ast1
  interpOpB (Op_ptr_cmp acmp) (ml_12 -> (arg1, arg2)) =
    SMTm_Expr $ do ast1 <- extractPtrArg arg1
                   ast2 <- extractPtrArg arg2
                   return $ smtArithCmp acmp l1typeRep ast1 ast2

  -- Interpret the first-order propositional operations into SMT
  interpOpB Op_true _ = SMTm_Expr $ return smtTrue
  interpOpB Op_false _ = SMTm_Expr $ return smtFalse
  interpOpB Op_and (ml_12 -> (arg1, arg2)) =
    SMTm_Expr $ do ast1 <- extractPropArg arg1
                   ast2 <- extractPropArg arg2
                   return $ smtAnd ast1 ast2
  interpOpB Op_or (ml_12 -> (arg1, arg2)) =
    SMTm_Expr $ do ast1 <- extractPropArg arg1
                   ast2 <- extractPropArg arg2
                   return $ smtOr ast1 ast2
  interpOpB Op_not (ml_first -> arg1) =
    SMTm_Expr $ do ast1 <- extractPropArg arg1
                   return $ smtNot ast1
  interpOpB (Op_eq l1tp) (ml_12 -> (arg1, arg2)) =
    SMTm_Expr $ do ast1 <- extractL1Arg l1tp arg1
                   ast2 <- extractL1Arg l1tp arg2
                   return $ smtEq ast1 ast2
  interpOpB Op_istrue (ml_first -> arg1) =
    -- NOTE: Op_istrue is the identity when converted to SMT
    SMTm_Expr $ do ast1 <- extractLitArg arg1
                   return $ smtIsTrue ast1

  -- Interpret the quantifiers into SMT
  interpOpB (Op_forall l1tp) (ml_first -> (BindingApply (SMTm_Expr body_m))) =
    SMTm_Expr $ smtForall l1tp body_m
  interpOpB (Op_exists l1tp) (ml_first -> (BindingApply (SMTm_Expr body_m))) =
    SMTm_Expr $ smtExists l1tp body_m
  interpOpB (Op_let l1tp) (ml_12 -> (rhs, BindingApply (SMTm_Expr body_m))) =
    SMTm_Expr $ do rhs_ast <- extractL1Arg l1tp rhs
                   smtLet l1tp rhs_ast body_m

  -- Signal an error if we try to convert any predicate monad operations to SMT
  interpOpB (Op_returnP _) _ = error "Cannot convert returnP to SMT"
  interpOpB (Op_bindP _ _) _ =
    error "Cannot convert bindP to SMT!"
  interpOpB (Op_readP _) _ =
    error "Cannot convert readP to SMT!"
  interpOpB (Op_updateP _) _ =
    error "Cannot convert updateP to SMT!"
  interpOpB (Op_raiseP _) _ =
    error "Cannot convert raiseP to SMT!"
  interpOpB (Op_catchP _) _ =
    error "Cannot convert catchP to SMT!"
  interpOpB Op_assumeP _ =
    error "Cannot convert assumeP to SMT!"
  interpOpB (Op_existsP _) _ =
    error "Cannot convert existsP to SMT!"
  interpOpB Op_falseP _ =
    error "Cannot convert falseP to SMT!"
  interpOpB Op_orP _ =
    error "Cannot convert orP to SMT!"

-- | Top-level call to convert an 'LProp' into a SMT expression
lprop_to_smt_expr :: LProp tag -> SMTm 'RNil (SMTExpr Prop)
lprop_to_smt_expr prop =
  unSMTm_Expr $ interpExprB Proxy prop

-- | Top-level call to convert an 'LProp'-in-binding into a SMT expression
mb_lprop_to_smt_expr :: Mb ctx (LProp tag) -> SMTm ctx (SMTExpr Prop)
mb_lprop_to_smt_expr prop =
  unSMTm_Expr $ interpMbExprB Proxy prop


----------------------------------------------------------------------
-- Extensions to SimpleSMT for getting models
----------------------------------------------------------------------

-- | Convert an s-expression to a value (this is lifted verbatim from SimpleSMT,
-- but is not exposed as a public API there...)
sexprToVal :: SMT.SExpr -> SMT.Value
sexprToVal expr =
  case expr of
    SMT.Atom "true"                    -> SMT.Bool True
    SMT.Atom "false"                   -> SMT.Bool False
    SMT.Atom ('#' : 'b' : ds)
      | Just n <- binLit ds         -> SMT.Bits (length ds) n
    SMT.Atom ('#' : 'x' : ds)
      | [(n,[])] <- readHex ds      -> SMT.Bits (4 * length ds) n
    SMT.Atom txt
      | Just n <- readMaybe txt     -> SMT.Int n
    SMT.List [ SMT.Atom "-", x ]
      | SMT.Int a <- sexprToVal x    -> SMT.Int (negate a)
    SMT.List [ SMT.Atom "/", x, y ]
      | SMT.Int a <- sexprToVal x
      , SMT.Int b <- sexprToVal y    -> SMT.Real (a % b)
    _ -> SMT.Other expr

  where
  binLit cs = do ds <- mapM binDigit cs
                 return $ sum $ zipWith (*) (reverse ds) powers2
  powers2   = 1 : map (2 *) powers2
  binDigit '0' = Just 0
  binDigit '1' = Just 1
  binDigit _   = Nothing

-- | Interpretations of functions in SMT models
data FunInterp =
  FunInterp
  {
    fun_name :: String,
    -- ^ The name of the function
    fun_args :: [(String, SMT.SExpr)],
    -- ^ The arguments of the function and their types
    fun_ret_type :: SMT.SExpr,
    -- ^ The return type of the function
    fun_cases :: [([SMT.Value], SMT.Value)],
    -- ^ The input-output cases for the function, mapping sequences of concrete
    -- values for the arguments to concrete outputs
    fun_else_value :: SMT.Value
    -- ^ The default value for the function if none of the cases match
  }

-- | A model is a list of function interpretations
newtype Model = Model { modelInterps :: [FunInterp] }

-- | Look up a function interpretation in a 'Model'
modelLookup :: Model -> String -> Maybe FunInterp
modelLookup (Model finterps) name =
  find ((==) name . fun_name) finterps

-- | Test whether an s-expression is constant relative to a given set of
-- variable names, i.e., whether it does not contain any of those variables
is_constant_sexpr :: [String] -> SMT.SExpr -> Bool
is_constant_sexpr var_names (SMT.Atom str) =
  notElem str var_names
is_constant_sexpr var_names (SMT.List sexprs) =
  and $ map (is_constant_sexpr var_names) sexprs

-- | Parse an s-expression for the body of a function into a list of cases and a
-- default value
parseFunBody :: Model -> [String] -> SMT.SExpr ->
                ([([SMT.Value], SMT.Value)], SMT.Value)

-- Special case: empty variable list -> a constant function
parseFunBody _ [] sexpr = ([], sexprToVal sexpr)

-- Special case: constant expression
parseFunBody _ var_names sexpr
  | is_constant_sexpr var_names sexpr = ([], sexprToVal sexpr)

-- Parse an if-then-else expression
parseFunBody model var_names sexpr@(SMT.List [SMT.Atom "ite", sexpr_cond,
                                              sexpr_then, sexpr_else]) =
  let arg_val_map = parse_ite_cond sexpr_cond
      arg_vals =
        map (\var -> case lookup var arg_val_map of
                Just val -> val
                Nothing ->
                  error ("parseFunBody: variable " ++ var ++
                         " not in if condition"))
        var_names
      then_val = constant_sexpr_to_val sexpr_then
      (cases, else_val) = parseFunBody model var_names sexpr_else in
  ((arg_vals, then_val) : cases, else_val)
  where
    -- Parse the condition of an if-expression
    parse_ite_cond :: SMT.SExpr -> [(String, SMT.Value)]
    parse_ite_cond (SMT.List [SMT.Atom "=", SMT.Atom var, val_sexpr])
      | elem var var_names = [(var, constant_sexpr_to_val val_sexpr)]
    parse_ite_cond (SMT.List [SMT.Atom "=", val_sexpr, SMT.Atom var])
      | elem var var_names = [(var, constant_sexpr_to_val val_sexpr)]
    parse_ite_cond (SMT.List (SMT.Atom "and" : sexprs)) =
      foldl' (unionBy
              (\(var1,_) (var2,_) ->
                if var1 == var2 then
                  error ("parseFunBody: variable " ++ var1 ++
                         " occurs multiple times in an if-expression condition")
                else False))
      [] (map parse_ite_cond sexprs)
    parse_ite_cond sexpr =
      error ("parseFunBody: could not parse if-then-else expression:\n"
             ++ ppSExpr sexpr)

    -- Parse an s-expression into a value, making sure that it is constant
    constant_sexpr_to_val :: SMT.SExpr -> SMT.Value
    constant_sexpr_to_val val_sexpr =
      if is_constant_sexpr var_names val_sexpr then
        sexprToVal val_sexpr
      else
        error ("parseFunBody: expected a value, found:\n" ++ ppSExpr val_sexpr)

-- Parse a nested set of function applications, where each variable only occurs
-- as the sole argument of a unary function
parseFunBody model var_names sexpr =
  let important_vals = get_important_arg_vals [] sexpr
      (real_var_envs, default_var_envs) = build_var_envs important_vals in
  case nub (map (\env -> eval_helper env sexpr) default_var_envs) of
    [] -> error "parseFunBody: No default values found! (Should be impossible...)"
    [else_val] ->
      (map
       (\env ->
         (map (\var -> case lookup var env of
                  Just (Just val) -> val
                  Just Nothing ->
                    error
                    ("parseFunBody: unexpected default value assigned to variable "
                     ++ var ++ " (Should be impossible...)")
                  Nothing ->
                    error ("parseFunBody: variable " ++ var
                           ++ " does not occur in s-expression:\n"
                           ++ ppSExpr sexpr))
          var_names
         ,
         eval_helper env sexpr))
       real_var_envs
      ,
      else_val)
    _ -> error ("parseFunBody: multiple distinct default values for function body:\n"
                ++ ppSExpr sexpr)
  where
    -- Get the finitely-many values for the arguments that match an if-then-else
    -- case of a function that is applied to them, and add them to the input
    -- list of argument values
    get_important_arg_vals :: [(String, [SMT.Value])] -> SMT.SExpr ->
                              [(String, [SMT.Value])]
    get_important_arg_vals arg_vals (SMT.Atom var) =
      if elem var var_names then
        error ("parseFunBody: variable " ++ var ++
               " is not the argument of a unary defined function in the model")
      else arg_vals
    get_important_arg_vals arg_vals (SMT.List [SMT.Atom fun_name, SMT.Atom var])
      | Just finterp <- modelLookup model fun_name
      , elem var var_names
      = insert_arg_vals var
        (map (\fun_case ->
               case fst fun_case of
                 [arg_val] -> arg_val
                 _ -> error ("parseFunBody: function " ++ fun_name
                             ++ " applied to wrong number of arguments!")) $
         fun_cases finterp)
        arg_vals
    get_important_arg_vals arg_vals (SMT.List (SMT.Atom fun_name : args))
      | Just _ <- modelLookup model fun_name
      = foldl' get_important_arg_vals arg_vals args
    get_important_arg_vals _ sexpr =
      error ("parseFunBody: cannot handle expression:\n" ++ ppSExpr sexpr)

    -- Insert a list of values for an argument into a list of argument values
    insert_arg_vals :: String -> [SMT.Value] -> [(String, [SMT.Value])] ->
                       [(String, [SMT.Value])]
    insert_arg_vals var vals [] = [(var, vals)]
    insert_arg_vals var vals ((var', vals') : arg_vals) =
      if var == var' then
        (var, vals ++ vals') : arg_vals
      else
        (var', vals') : insert_arg_vals var vals arg_vals

    -- Build two sets of var-to-optional-value environment mappings, one where
    -- all variables have a value and one where at least one variable has a
    -- "default" value (represented with Nothing)
    build_var_envs :: [(String, [SMT.Value])] ->
                      ([[(String, Maybe SMT.Value)]],
                       [[(String, Maybe SMT.Value)]])
    build_var_envs [] = ([[]], [[]])
    build_var_envs ((var, vals) : var_vals') =
      let (real_var_envs, default_var_envs) = build_var_envs var_vals' in
      ([ (var, Just val) : env | val <- vals, env <- real_var_envs ]
      ,
       map (\env -> (var, Nothing) : env) real_var_envs ++
       map (\env -> (var, Nothing) : env) default_var_envs ++
       [ (var, Just val) : env | val <- vals, env <- default_var_envs ])

    -- Evaluate an s-expression with a mapping from variable names to optional
    -- values, where Nothing represents a default value
    eval_helper :: [(String, Maybe SMT.Value)] -> SMT.SExpr -> SMT.Value
    eval_helper env sexpr =
      case eval_helper' env sexpr of
        Just v -> v
        Nothing ->
          error ("parseFunBody: could not find default value for expression:\n"
                 ++ ppSExpr sexpr)

    eval_helper' :: [(String, Maybe SMT.Value)] -> SMT.SExpr -> Maybe SMT.Value
    eval_helper' env (SMT.Atom var) =
      case lookup var env of
        Just res -> res
        Nothing -> error ("parseFunBody: unexpected variable: " ++ var)
    eval_helper' env (SMT.List (SMT.Atom fun_name : args)) =
      case modelLookup model fun_name of
        Nothing -> error ("parseFunBody: unexpected function: " ++ fun_name)
        Just finterp ->
          case lookup (map (eval_helper env) args) (fun_cases finterp) of
            Just v -> Just v
            Nothing -> Just $ fun_else_value finterp
    eval_helper' _ sexpr =
      error ("parseFunBody: cannot handle expression:\n" ++ ppSExpr sexpr)

-- | Parse a function interpretation from an s-expression using the model so far
parseFunInterp :: Model -> SMT.SExpr -> FunInterp
parseFunInterp model f_sexpr@(SMT.List
                              [SMT.Atom "define-fun", SMT.Atom fun_name,
                               SMT.List arg_sexprs, fun_ret_type, body]) =
  let parsed_args = map parse_arg_sexpr arg_sexprs
      (fun_cases, fun_else_value) =
        parseFunBody model (map fst parsed_args) body in
  FunInterp { fun_name, fun_args = parsed_args, fun_ret_type,
              fun_cases, fun_else_value } where

    -- Parse an (argument name, argument type) pair
    parse_arg_sexpr :: SMT.SExpr -> (String, SMT.SExpr)
    parse_arg_sexpr (SMT.List [SMT.Atom arg_name, arg_tp]) = (arg_name, arg_tp)
    parse_arg_sexpr sexpr =
      error ("parseFunInterp: Could not parse function argument:\n" ++ ppSExpr sexpr
             ++ "\n in function interpretation:\n"
             ++ ppSExpr f_sexpr)

-- | Get the current model (only valid after a 'Sat' result)
getModel :: SMTm ctx Model
getModel =
  do solver <- getSolver
     model_sexpr <- inBase $ SMT.command solver (SMT.List [SMT.Atom "get-model"])
     smtDebug 3 ("Model:\n" ++ ppSExpr model_sexpr)
     case model_sexpr of
       SMT.List (SMT.Atom "model" : fun_decls) ->
         return $
         foldl' (\model fun_decl ->
                  Model (parseFunInterp model fun_decl : modelInterps model))
         (Model []) fun_decls
       resp ->
         error ("getModel: unexpected response from solver:\n" ++ ppSExpr resp)


----------------------------------------------------------------------
-- Top-level interface
----------------------------------------------------------------------

-- | Convert an 'SMT.Value' into an 'SMTValue'
value_to_smt_value :: L1Type a -> SMT.Value -> SMTValue a
value_to_smt_value (L1Type_lit LitType_unit) _ =
  Literal ()
value_to_smt_value (L1Type_lit LitType_bool) (SMT.Bool b) =
  Literal b
value_to_smt_value (L1Type_lit LitType_bool) v =
  error ("value_to_smt_value: Boolean expected, found: " ++ show v)
value_to_smt_value (L1Type_lit LitType_int) (SMT.Int i) =
  Literal i
value_to_smt_value (L1Type_lit LitType_int) v =
  error ("value_to_smt_value: integer expected, found: " ++ show v)
value_to_smt_value (L1Type_lit LitType_rat) (SMT.Real r) =
  Literal r
value_to_smt_value (L1Type_lit LitType_rat) v =
  error ("value_to_smt_value: rational expected, found: " ++ show v)
value_to_smt_value (L1Type_lit LitType_bits) (SMT.Bits _ i) =
  Literal $ fromInteger i
value_to_smt_value (L1Type_lit LitType_bits) v =
  error ("value_to_smt_value: bit-vector expected, found: " ++ show v)
value_to_smt_value L1Type_ptr (SMT.Int i) =
  Ptr i
value_to_smt_value L1Type_ptr v =
  error ("value_to_smt_value: pointer (integer) expected, found: " ++ show v)
value_to_smt_value L1Type_prop (SMT.Bool b) =
  b
value_to_smt_value L1Type_prop v =
  error ("value_to_smt_value: proposition (Boolean) expected, found: " ++ show v)

-- | Convert a list of 'SMT.Value's into a left-hand side for a 'FinFun'
values_to_smt_values :: MapList L1Type as -> [SMT.Value] ->
                        TupleType (SMTValues as)
values_to_smt_values Nil [] = ()
values_to_smt_values (Cons l1tp l1tps) (v : vs) =
  (value_to_smt_value l1tp v, values_to_smt_values l1tps vs)
values_to_smt_values _ _ =
  error "values_to_smt_values: wrong number of arguments!"

-- | Convert a 'FunInterp' into an 'SMTValue' for a function
fun_value_to_smt_value :: L1FunType a -> FunInterp -> SMTValueFun a
fun_value_to_smt_value (ftp :: L1FunType a) finterp =
  FinFun
  {
    finfunDef = conv_ret_value (fun_else_value finterp)
  , finfunMap =
      map (\fun_case ->
            (values_to_smt_values (funType_arg_types ftp) (fst fun_case),
             conv_ret_value (snd fun_case))) $
      fun_cases finterp
  }
  where
    conv_ret_value :: SMT.Value -> SMTValue (RetType a)
    conv_ret_value v = value_to_smt_value (funType_ret_type ftp) v

-- | Get the values of all the existential values in an 'SMTCtx'
evalSMTCtx :: Model -> SMTCtx ctx -> SMTm ctx' (MapRList MaybeSMTValue ctx)
evalSMTCtx _ SMTCtx_nil = return MNil
evalSMTCtx model (SMTCtx_EVar ctx l1tp var_expr) =
  do solver <- getSolver
     v <- value_to_smt_value l1tp <$>
       (inBase $ SMT.getExpr solver (unSMTExpr var_expr))
     vs <- evalSMTCtx model ctx
     return (vs :>: MaybeSMTValue (Just v))
evalSMTCtx model (SMTCtx_FVar ctx ftp fun_str) =
  do vs <- evalSMTCtx model ctx
     let maybe_v =
           case modelLookup model fun_str of
             Just finterp -> Just $ fun_value_to_smt_value ftp finterp
             Nothing -> Nothing
     return (vs :>: MaybeSMTValue maybe_v)
evalSMTCtx _ (SMTCtx_UVar _ _ _) =
  error "evalSMTCtx: formula has a top-level universal variable!"

-- | Dummy type to indicate the SimpleSMT solver in the 'SMTSolver'
-- class. Includes a debugging level.
data SimpleSolver = SimpleSolver { smtDebugLevel :: Int,
                                   smtSolverPath :: String,
                                   smtSolverArgs :: [String],
                                   smtTimeout_ms :: Maybe Integer }

-- | Default 'SMTSolver' for SMT
defaultZ3Solver :: SimpleSolver
defaultZ3Solver = SimpleSolver { smtDebugLevel = 0,
                                 smtSolverPath = "z3",
                                 smtSolverArgs = ["-smt2", "-in"],
                                 smtTimeout_ms = Just 2000 }

-- | Run an 'SMTm' computation using an 'SMTSolver'
runWithSolver :: SimpleSolver -> MapRList L1FunType ctx -> SMTm ctx a -> IO a
runWithSolver solver tps m =
  do maybe_logger <-
       -- Build a logger that logs everything iff the debug level > 2
       if smtDebugLevel solver > 2 then Just <$> SMT.newLogger 0
       else return Nothing
     smt_solver <-
       SMT.newSolver (smtSolverPath solver) (smtSolverArgs solver) maybe_logger
     -- Set the :timeout value if a timeout value is given
     case smtTimeout_ms solver of
       Nothing -> return ()
       Just timeout ->
         do res <- SMT.setOptionMaybe smt_solver ":timeout" (show timeout)
            if not res then
              putStrLn "Warning: the :timeout option is not supported"
              else return ()
     runM m smt_solver (smtDebugLevel solver) tps

instance SMTSolver SimpleSolver where
  smtGetDebugLevel solver = smtDebugLevel solver
  smtSetDebugLevel level solver = solver { smtDebugLevel = level }
  smtSolve solver const_tps props =
    runWithSolver solver const_tps $
    do smt_solver <- getSolver
       (converted_asts, collected_asts) <-
         collect $ mapM mb_lprop_to_smt_expr props
       let smt_props = collected_asts ++ converted_asts
       smtIfDebug 1 $
         do inBase $ putStrLn "Performing SMT query:"
            inBase $ putStrLn ""
            mapM_ (inBase . putStrLn . ppExpr) smt_props
            inBase $ putStrLn ""
       mapM_ (inBase . SMT.assert smt_solver . unSMTExpr) smt_props
       res <- inBase $ SMT.check smt_solver
       case res of
         SMT.Sat ->
           do ctx <- ask
              smtDebug 1 "SMT: query is satisfiable!"
              model <- getModel
              vals <- evalSMTCtx model ctx
              return (SMT_sat vals)
         SMT.Unsat ->
           smtDebug 1 "SMT: query is unsatisfiable!" >>
           return SMT_unsat
         SMT.Unknown ->
           do smtDebug 1 "SMT: error running query"
              return $ SMT_unknown "(error unknown)"
