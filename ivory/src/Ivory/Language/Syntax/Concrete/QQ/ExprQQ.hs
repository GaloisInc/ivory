{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}

--
-- Ivory expression QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.ExprQQ
  ( toExp
  , fromConstDef
  , toArray
  , toStruct
  , toAddrOf
  ) where

import           Prelude hiding (exp, init)
import qualified Prelude as P

import           Data.String (IsString(..))

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T
import           Language.Haskell.TH.Quote()
import           Language.Haskell.TH.Lift()

import qualified Ivory.Language.IIntegral as I
import qualified Ivory.Language.Bits      as I
import qualified  Ivory.Language.Float    as I
import qualified  Ivory.Language.Ref      as I
import qualified  Ivory.Language.MemArea  as I
import qualified  Ivory.Language.IBool    as I
import qualified  Ivory.Language.Array    as I
import qualified  Ivory.Language.Struct   as I
import qualified  Ivory.Language.CArray   as I
import qualified  Ivory.Language.Cast     as I
import qualified  Ivory.Language.Ptr      as I
import qualified  Ivory.Language.SizeOf   as I

import Ivory.Language.Syntax.Concrete.ParseAST

import Ivory.Language.Syntax.Concrete.QQ.Common
import Ivory.Language.Syntax.Concrete.QQ.TypeQQ
import Ivory.Language.Syntax.Concrete.Location

--------------------------------------------------------------------------------
-- Expressions

-- | Top-level constant definition.
fromConstDef :: ConstDef -> Q [Dec]
fromConstDef def = case def of
#if __GLASGOW_HASKELL__ >= 709
  ConstDef sym e mtype srcloc -> do
    n <- newName sym
    let def = ValD (VarP n) (NormalB $ toExp [] e) []
    case mtype of
      Nothing -> return [def]
      Just ty -> do tyQ <- runToQ (fromType ty)
                    -- Ignore possible type variables---should be any for a
                    -- top-level constant.
                    ln <- lnPragma srcloc
                    return (ln ++ [SigD n (fst tyQ), def])
#else
  ConstDef sym e mtype _srcloc -> do
    n <- newName sym
    let d = ValD (VarP n) (NormalB $ toExp [] e) []
    case mtype of
      Nothing -> return [d]
      Just ty -> do tyQ <- runToQ (fromType ty)
                    -- Ignore possible type variables---should be any for a
                    -- top-level constant.
                    return [SigD n (fst tyQ), d]
#endif

fromLit :: Literal -> T.Exp
fromLit lit = case lit of
  LitInteger int -> LitE (IntegerL int)
  LitFloat   f   -> LitE (RationalL f)
  LitString  str -> AppE (VarE 'fromString) (LitE (StringL str))

fromOpExp :: VarEnv -> ExpOp -> [Exp] -> T.Exp
fromOpExp env op args = case op of
  EqOp             -> mkInfix '(I.==?)
  NeqOp            -> mkInfix '(I./=?)
  CondOp           -> mkTert  '(I.?)

  GtOp g           -> mkInfix $ if g then '(I.>=?) else '(I.>?)
  LtOp g           -> mkInfix $ if g then '(I.<=?) else '(I.<?)

  NotOp            -> mkUn 'I.iNot
  AndOp            -> mkInfix '(I..&&)
  OrOp             -> mkInfix '(I..||)

  MulOp            -> mkInfix '(*)
  AddOp            -> mkInfix '(+)
  SubOp            -> mkInfix '(-)
  NegateOp         -> mkUn 'negate
  AbsOp            -> mkUn 'abs
  SignumOp         -> mkUn 'signum

  DivOp            -> mkInfix 'I.iDiv -- truncate toward 0 (Haskell's 'quot')
  ModOp            -> mkInfix '(I..%)

  FExpOp           -> mkUn 'P.exp
  FSqrtOp          -> mkUn 'sqrt
  FLogOp           -> mkUn 'log
  FPowOp           -> mkInfix '(**)
  FSinOp           -> mkUn 'sin
  FTanOp           -> mkUn 'tan
  FCosOp           -> mkUn 'cos
  FAsinOp          -> mkUn 'asin
  FAtanOp          -> mkUn 'atan
  FAcosOp          -> mkUn 'acos
  FSinhOp          -> mkUn 'sinh
  FTanhOp          -> mkUn 'tanh
  FCoshOp          -> mkUn 'cosh
  FAsinhOp         -> mkUn 'asinh
  FAtanhOp         -> mkUn 'atanh
  FAcoshOp         -> mkUn 'acosh

  IsNanOp          -> mkUn 'I.isnan
  IsInfOp          -> mkUn 'I.isinf
  RoundFOp         -> mkUn 'I.roundF
  CeilFOp          -> mkUn 'I.ceilF
  FloorFOp         -> mkUn 'I.floorF

  BitAndOp         -> mkInfix '(I..&)
  BitOrOp          -> mkInfix '(I..|)
  BitXorOp         -> mkInfix '(I..^)
  BitComplementOp  -> mkUn  'I.iComplement
  BitShiftLOp      -> mkBin 'I.iShiftL
  BitShiftROp      -> mkBin 'I.iShiftR

  ConstRefOp       -> mkUn 'I.constRef

  SafeCast         -> mkUn  'I.safeCast
  BitCast          -> mkUn  'I.bitCast
  CastWith         -> mkBin 'I.castWith
  TwosCompCast     -> mkUn  'I.twosComplementCast
  TwosCompRep      -> mkUn  'I.twosComplementRep

  ToIx             -> mkUn  'I.toIx
  FromIx           -> mkUn  'I.fromIx
  ToCArray         -> mkUn  'I.toCArray
  ArrayLen         -> mkUn  'I.arrayLen
  SizeOf           -> mkUn  'I.sizeOf
  NullPtr          -> mkUn  'I.nullPtr
  RefToPtr         -> mkUn  'I.refToPtr
  IxSize           -> mkUn  'I.ixSize

  where
  getArg i    = toExp env (args !! i)
  mkArg       = Just . getArg
  mkInfix op' = InfixE (mkArg 0) (VarE op') (mkArg 1)
  mkTert  op' = InfixE (mkArg 0) (VarE op') (Just $ TupE [getArg 1, getArg 2])
  mkUn    op' = AppE (VarE op') (getArg 0)
  mkBin   op' = AppE (AppE (VarE op') (getArg 0)) (getArg 1)

toExp :: VarEnv -> Exp -> T.Exp
toExp env exp = case exp of
  ExpLit lit
    -> fromLit lit
  ExpVar v
    -> VarE (mkName v)
  ExpRet
    -> VarE (mkName "return")
  ExpOp op args
    -> fromOpExp env op args
  IvoryMacroExp (v,args)
    -> callit (mkVar v) (map (toExp env) args)
  ExpDeref e
    -> VarE $ lookupDerefVar (expToArea e) env
  ExpArray e0 e1
    -> toArray (toExp env e0) (toExp env e1)
  ExpStruct e0 e1
    -> toStruct (toExp env e0) (toExp env e1)
  -- Must be a call that returns a value
  ExpCall sym args
    -> VarE $ lookupVar (expToCall sym args) env
  ExpAddrOf v
    -> toAddrOf $ VarE $ mkName v
  LocExp e
    -> toExp env (unLoc e)

--------------------------------------------------------------------------------
-- These are shared by toExp above and fromArea in BindExp.

toArray :: T.Exp -> T.Exp -> T.Exp
toArray e0 e1 = InfixE (Just e0) (VarE '(I.!)) (Just e1)

toStruct :: T.Exp -> T.Exp -> T.Exp
toStruct e0 e1 = InfixE (Just e0) (VarE '(I.~>)) (Just e1)

toAddrOf :: T.Exp -> T.Exp
toAddrOf = AppE (VarE 'I.addrOf)

--------------------------------------------------------------------------------
