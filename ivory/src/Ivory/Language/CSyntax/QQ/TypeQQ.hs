{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Ivory types QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.QQ.TypeQQ where

import           Prelude hiding (exp, init)
import qualified Prelude as P

import Control.Monad

import Ivory.Language.CSyntax.QQ.Common

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language as I

import Ivory.Language.CSyntax.ParseAST

--------------------------------------------------------------------------------

-- We use a state monad over the Q monad to keep track of type variables that we
-- need to quantify later.
type TTyVar a = QStM Name a

--------------------------------------------------------------------------------

fromIntSz :: IntSize -> Name
fromIntSz sz = case sz of
  Int8  -> ''I.Sint8
  Int16 -> ''I.Sint16
  Int32 -> ''I.Sint32
  Int64 -> ''I.Sint64

fromWordSz :: WordSize -> Name
fromWordSz sz = case sz of
  Word8  -> ''I.Uint8
  Word16 -> ''I.Uint16
  Word32 -> ''I.Uint32
  Word64 -> ''I.Uint64

fromMemArea :: MemArea -> TTyVar T.Type
fromMemArea ma = case ma of
  Global  -> liftPromote 'I.Global
  Stack   -> do s <- liftPromote 'I.Stack
                n <- new
                return (AppT s n)
  PolyMem -> new
  where
  new :: TTyVar T.Type
  new = do n <- liftQ (newName "s")
           insert n
           return (VarT n)

storedTy :: Type -> TTyVar T.Type
storedTy ty = do
  ty' <- fromType ty
  s   <- liftPromote 'I.Stored
  let stored = return (AppT s ty')
  case ty of
    TyVoid       -> stored
    TyInt{}      -> stored
    TyWord{}     -> stored
    TyBool       -> stored
    TyChar       -> stored
    TyFloat      -> stored
    TyDouble     -> stored
    _            -> return ty'

fromRef :: MemArea -> Type -> TTyVar T.Type
fromRef mem ty = do
  ty'     <- storedTy ty
  ma      <- fromMemArea mem
  return $ AppT (AppT (ConT ''I.Ref) ma) ty'

fromArray :: Type -> Integer -> TTyVar T.Type
fromArray ty sz = do
  let szTy = (LitT (NumTyLit sz))
  ty'    <- storedTy ty
  arr    <- liftPromote 'I.Array
  return $ AppT (AppT arr szTy) ty'

fromStruct :: Name -> TTyVar T.Type
fromStruct nm = error "from struct QQ not defined"

fromType :: Type -> TTyVar T.Type
fromType ty = case ty of
  TyVoid       -> c ''()
  TyInt sz     -> c (fromIntSz sz)
  TyWord sz    -> c (fromWordSz sz)
  TyBool       -> c ''I.IBool
  TyChar       -> c ''I.IChar
  TyFloat      -> c ''I.IFloat
  TyDouble     -> c ''I.IDouble
  TyRef qma qt -> fromRef qma qt
  TyArr ty' sz -> fromArray ty' sz
  TyStruct nm  -> fromStruct (mkName nm)
  where
  c = liftQ . conT

-- | Create a procedure type.
fromProcType :: ProcDef -> Q Dec
fromProcType (ProcDef retTy procName args _) = do
  arr                 <- promotedT '(I.:->)
  (ret,retTyVars)     <- runToQ (fromType retTy)
  (argVars,argTyVars) <- fromArgs
  let def = AppT (ConT ''I.Def) (AppT (AppT arr argVars) ret)
  return $ SigD (mkName procName)
                (ForallT (map PlainTV (argTyVars ++ retTyVars)) [] def)
  where
  fromArgs :: Q (T.Type, [Name])
  fromArgs = runToQ $ foldM go PromotedNilT
                               (fst $ unzip args)
  -- Grab the type variables to quantify them.
  go :: T.Type -> Type -> TTyVar T.Type
  go acc ty = do
    ty' <- fromType ty
    return (AppT (AppT PromotedConsT ty') acc)

--------------------------------------------------------------------------------
-- Helpers

liftPromote :: Name -> TTyVar T.Type
liftPromote = liftQ . promotedT
