{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- QuasiQuoter for Ivory statements.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.AreaQQ
  ( fromExp
  , fromArea
  ) where

import Prelude hiding (exp)

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T

import qualified Ivory.Language.Ref    as I
import qualified Ivory.Language.Array  as I
import qualified Ivory.Language.Struct as I

import           Ivory.Language.Syntax.Concrete.ParseAST
import           Ivory.Language.Syntax.Concrete.QQ.ExprQQ
import           Ivory.Language.Syntax.Concrete.QQ.Common

--------------------------------------------------------------------------------
-- Insert dereference statements

-- Collect up dereference expressions, which turn into Ivory statements.  We
-- only need one dereference statement for each unique dereferenced
-- equation. This call comes from a the use of an expression in a statement.
-- Note that we might needlessly duplicate dereference statements for e_1 and
-- e_2 in the same statement.
fromExp :: Exp -> TStmtM T.Exp
fromExp exp = do
  env <- mkDerefStmts exp
  return (toExp env exp)

mkDerefStmts :: Exp -> TStmtM DerefVarEnv
mkDerefStmts exp = do
  -- collectRefExps gets all subexpressions that contain an area dereference,
  -- except the array index in array areas (which are processed in fromArea).
  envs <- mapM insertDerefStmt (collectRefExps exp)
  return (concat envs)

-- For each area, (1) insert a dereference statement and (2) return a (area, nm)
-- map, where nm is the fresh variable associated with the area so the
-- expression can lookup the deref variable from the area used in a dereference.
insertDerefStmt :: Area -> TStmtM DerefVarEnv
insertDerefStmt area = do
  a <- fromArea area
  i@(_, nm) <- fresh
  insertDeref nm a
  return [i]
  where
  fresh = do nm <- liftQ (freshDeref area)
             return (area, nm)
  -- We want to generate a fresh name that won't capture other user-defined
  -- names, since we're inserting these variables. We'll make a name base that
  -- helps us track it's usage.
  freshDeref = newName . ("deref_" ++) . areaToVar
  insertDeref nm exp = insert $ BindS (VarP nm) (AppE (VarE 'I.deref) exp)

areaToVar :: Area -> String
areaToVar area = case area of
  AreaVar v               -> v
  -- Ignore the expression. Ok, since these are bases to fresh vars.
  ArrayArea area' _       -> areaToVar area'
  StructArea area' field  -> areaToVar area' ++ ('_':field)

--------------------------------------------------------------------------------

-- Create a TH expression for an area.
fromArea :: Area -> TStmtM T.Exp
fromArea area = case area of
  AreaVar v -- ref
    -> return $ VarE $ mkName v
  ArrayArea area' ixExp -- (arr @ ix)
    -> do ix <- fromExp ixExp
          a  <- fromArea area'
          return $ InfixE (Just a) (VarE '(I.!)) (Just ix)
  StructArea area' field -- (area . field)
    -> do a <- fromArea area'
          return $ InfixE (Just a)
                          (VarE '(I.~>))
                          (Just (mkVar field))

--------------------------------------------------------------------------------
