{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Ivory expression QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.QQ.ExprQQ where

import           Prelude hiding (exp, init)
import qualified Prelude as P

import Ivory.Language.CSyntax.QQ.Types

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T
import           Language.Haskell.TH.Quote()
import           Language.Haskell.TH.Lift()

import qualified Ivory.Language as I

import Ivory.Language.CSyntax.ParseAST

--------------------------------------------------------------------------------
-- Expressions

fromLit :: Literal -> T.Exp
fromLit lit = case lit of
  LitInteger int -> LitE (IntegerL int)

fromOpExp :: DerefVarEnv -> ExpOp -> [Exp] -> T.Exp
fromOpExp env op args = case op of
  EqOp            -> mkInfix '(I.==?)
  NeqOp           -> mkInfix '(I./=?)
  CondOp          -> mkTert  '(I.?)

  GtOp g          -> mkInfix $ if g then '(I.>=?) else '(I.>?)
  LtOp g          -> mkInfix $ if g then '(I.<=?) else '(I.<?)

  NotOp           -> mkUn 'I.iNot
  AndOp           -> mkInfix '(I..&&)
  OrOp            -> mkInfix '(I..||)

  MulOp           -> mkInfix '(*)
  AddOp           -> mkInfix '(+)
  SubOp           -> mkInfix '(-)
  NegateOp        -> mkInfix 'negate
  AbsOp           -> mkUn 'abs
  SignumOp        -> mkUn 'signum

  DivOp           -> mkInfix 'I.iDiv -- truncate toward 0 (Haskell's 'quot')
  ModOp           -> mkInfix '(I..%)

  FExpOp          -> mkUn 'P.exp
  FSqrtOp         -> mkUn 'sqrt
  FLogOp          -> mkUn 'log
  FPowOp          -> mkInfix '(**)
  FSinOp          -> mkUn 'sin
  FTanOp          -> mkUn 'tan
  FCosOp          -> mkUn 'cos
  FAsinOp         -> mkUn 'asin
  FAtanOp         -> mkUn 'atan
  FAcosOp         -> mkUn 'acos
  FSinhOp         -> mkUn 'sinh
  FTanhOp         -> mkUn 'tanh
  FCoshOp         -> mkUn 'cosh
  FAsinhOp        -> mkUn 'asinh
  FAtanhOp        -> mkUn 'atanh
  FAcoshOp        -> mkUn 'acosh

  IsNanOp         -> mkUn 'I.isnan
  IsInfOp         -> mkUn 'I.isinf
  RoundFOp        -> mkUn 'I.roundF
  CeilFOp         -> mkUn 'I.ceilF
  FloorFOp        -> mkUn 'I.floorF

  BitAndOp        -> mkInfix '(I..&)
  BitOrOp         -> mkInfix '(I..|)
  BitXorOp        -> mkInfix '(I..^)
  BitComplementOp -> mkUn 'I.iComplement
  BitShiftLOp     -> mkUn 'I.iShiftL
  BitShiftROp     -> mkUn 'I.iShiftR

  where
  getArg i = toExp env (args !! i)
  mkArg    = Just . getArg
  mkInfix op' = InfixE (mkArg 0) (VarE op') (mkArg 1)
  mkTert  op' =
    InfixE (mkArg 0) (VarE op') (Just $ TupE [getArg 1, getArg 2])
  mkUn op' = AppE (VarE op') (getArg 0)

toExp :: DerefVarEnv -> Exp -> T.Exp
toExp env exp = case exp of
  ExpLit lit
    -> fromLit lit
  ExpVar v
    -> VarE (mkName v)
  ExpDeref refVar
    -> lookupV (RefExp refVar)
  ExpOp op args
    -> fromOpExp env op args
  ExpArrIx arr ix
    -> lookupV (ArrIxExp arr ix)
  ExpAnti str
    -> VarE (mkName str)
  where
  lookupV de = VarE (lookupDerefVar de env)

-----------------------------------------
-- Dereference expression environment

type DerefVarEnv = [(DerefExp, Name)]

-- Returns the fresh variable that is the do-block binding from the dereference
-- statement.
lookupDerefVar :: DerefExp -> DerefVarEnv -> Name
lookupDerefVar refVar env =
  case lookup refVar env of
    Nothing -> error "Internal error in lookupDerefVar"
    Just rv -> rv
