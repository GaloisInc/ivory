{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language as I

import Ivory.Language.CSyntax.ParseAST

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

fromMemArea :: MemArea -> Q T.Type
fromMemArea ma = case ma of
   Stack   -> promotedT 'Stack
   Global  -> promotedT 'Global
   PolyMem -> newName "s" >>= return . VarT

fromType :: Type -> Q T.Type
fromType ty = case ty of
  TyVoid       -> conT ''()
  TyInt sz     -> conT (fromIntSz sz)
  TyWord sz    -> conT (fromWordSz sz)
  TyBool       -> conT ''I.IBool
  TyChar       -> conT ''I.IChar
  TyFloat      -> conT ''I.IFloat
  TyDouble     -> conT ''I.IDouble
  TyRef qma qt -> do t  <- fromType qt
                     ma <- fromMemArea qma
                     return $ AppT (AppT (ConT ''I.Ref) ma) t

-- | Create a procedure type.
fromProcType :: ProcDef -> Q Dec
fromProcType (ProcDef retTy procName args _) = do
  arr         <- promotedT '(I.:->)
  ret         <- fromType retTy
  (vars,theargs) <- fromArgs
  let def = AppT (ConT ''I.Def) (AppT (AppT arr theargs) ret)
  return $ SigD (mkName procName)
                (ForallT (map PlainTV vars) [] def)
  where
  fromArgs = foldM go ([], PromotedNilT)
                      (fst $ unzip args)
  -- Grab the type variables to quantify them.
  go (tyVars, acc) ty = do
    ty' <- fromType ty
    let aptTy = AppT acc ty'
    return $ case ty' of
      VarT a  -> (a : tyVars, aptTy)
      _       -> (tyVars    , aptTy)
