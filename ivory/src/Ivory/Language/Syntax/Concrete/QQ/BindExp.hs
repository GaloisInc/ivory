{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Binding expressions that are Ivory statements.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.BindExp
  ( fromExp
  , fromArea
  , fromExpStmt
  , fromAreaStmt
  ) where

import Prelude hiding (exp)

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T

import qualified Ivory.Language.Proc     as I
import qualified Ivory.Language.Ref      as I

import           Ivory.Language.Syntax.Concrete.QQ.Common
import           Ivory.Language.Syntax.Concrete.ParseAST
import           Ivory.Language.Syntax.Concrete.QQ.ExprQQ

--------------------------------------------------------------------------------

-- Insert statements

-- Collect up expressions that turn into Ivory statements. This call comes from
-- a the use of an expression in a statement.
--
-- This isn't heavily optimized (for the sake of simplicity): we might duplicate
--dereference statements for e_1 and e_2 in the same statement.
fromExp :: Insert a -> Exp -> QStM a T.Exp
fromExp f exp = do
  env <- (mkBinds f) exp
  return (toExp env exp)

mkBinds :: Insert a -> Exp -> QStM a VarEnv
mkBinds f exp = do
  -- collectBindExps gets all subexpressions that contain Ivory statements (area
  -- dereferences, function calls), except the array index in array areas (which
  -- are processed in fromArea) and the function args.
  envs <- mapM (insertBind f) (collectBindExps exp)
  return (concat envs)

-- For each binding, (1) insert a statement and (2) return a map, mapping to the
-- fresh variable associated with the key so the expression can lookup the
-- binding variable from the monadic statement.
insertBind :: Insert a -> Key -> QStM a VarEnv
insertBind f key = do
  b <- fromBind f key
  i@(_, nm) <- fresh
  f key nm b
  return [i]
  where
  fresh = do nm <- liftQ (freshVar key)
             return (key, nm)

fromBind :: Insert a -> Key -> QStM a T.Exp
fromBind f key
  | isArea key
  = fromArea f (keyToArea key)
  | isCall key
  = fromCall f (keyToCall key)
  | otherwise
  = error "impossible in fromBind"

fromCall :: Insert a -> Call -> QStM a T.Exp
fromCall f (Call sym args) = do
  es <- mapM (fromExp f) args
  let call = AppE (VarE 'I.call) (mkVar sym)
  return $ callit call es

-- Base names for call bindings
callToVar :: Call -> String
callToVar (Call sym _) = sym

-- Base names for dereference variables
areaToVar :: Area -> String
areaToVar area = case area of
  AreaVar v               -> map (\c -> if c == '.' then '_' else c) v
  AddrOf v                -> areaToVar v
  -- Ignore the expression. Ok, since these are bases to fresh vars.
  ArrayArea area' _       -> areaToVar area'
  StructArea area0 area1  -> areaToVar area0 ++ ('_': areaToVar area1)

-- Create a TH expression for an area.
fromArea :: Insert a -> Area -> QStM a T.Exp
fromArea f area = case area of
  AreaVar v -- ref
    -> return (mkVar v)
  AddrOf area' -- ref
    -> do a <- (fromArea f) area'
          return $ toAddrOf a
  ArrayArea area' ixExp -- (arr @ ix)
    -> do ix <- (fromExp f) ixExp
          a  <- (fromArea f) area'
          return $ toArray a ix
  StructArea area0 area1 -- (area . area)
    -> do a0 <- (fromArea f) area0
          a1 <- (fromArea f) area1
          return $ toStruct a0 a1

-- We want to generate a fresh name that won't capture other user-defined
-- names, since we're inserting these variables. We'll make a name base that
-- helps us track it's usage.
freshVar :: Key -> Q Name
freshVar key
  | isArea key
  = newName $ "deref_" ++ areaToVar (keyToArea key)
  | isCall key
  = newName $ "call_" ++ callToVar (keyToCall key)
  | otherwise
  = error "Impossible in freshVar"

insertStmt :: Insert T.Stmt
insertStmt key nm exp
  | isArea key
  = insert $ BindS (VarP nm) (AppE (VarE 'I.deref) exp)
  | isCall key
  = insert $ BindS (VarP nm) exp
  | otherwise
  = error "Impossible in insertStmt"

fromExpStmt :: Exp -> QStM T.Stmt T.Exp
fromExpStmt = fromExp insertStmt

fromAreaStmt :: Area -> QStM T.Stmt T.Exp
fromAreaStmt = fromArea insertStmt
