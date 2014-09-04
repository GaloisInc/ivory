{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Ivory pre/post conditions quasiquoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.CondQQ where

import           Prelude hiding (exp)

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH       as T
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language.Cond as I

import Ivory.Language.Syntax.Concrete.ParseAST

import Ivory.Language.Syntax.Concrete.QQ.AreaQQ
import Ivory.Language.Syntax.Concrete.QQ.Common

--------------------------------------------------------------------------------
-- Assertions, e.g.,
--
-- requires (checkStored (pid ~> pid_err) (\err -> err <? 1)) ensures (\res ->
-- checkStored (pid ~> pid_err) (\err -> err <? res))
--------------------------------------------------------------------------------

mkPrePostConds :: [PrePost] -> T.Exp -> Q T.Exp
mkPrePostConds conds procBody = do
  condFns <- mapM mkCond conds
  -- Apply conditions to the proc body in the Q monad.
  return (foldr AppE procBody condFns)

  where
  mkCond :: PrePost -> Q T.Exp
  mkCond cond = case cond of
    PreCond  exp -> appE (varE 'I.requires) (runExp exp)
    PostCond exp -> appE (varE 'I.ensures)  (lamE [varP $ mkName "return"]
                                                  (runExp exp))

  runExp :: Exp -> Q T.Exp
  runExp exp = do
    (e, derefs) <- runToQ (fromExpCond exp)
    return (foldr go e derefs)
    where
    go :: (T.Exp, T.Name) -> T.Exp -> T.Exp
    go (deref, nm) acc =
      AppE (AppE (VarE 'I.checkStored) deref) (LamE [VarP nm] acc)

--------------------------------------------------------------------------------

fromExpCond :: Exp -> QStM (T.Exp, Name) T.Exp
fromExpCond = fromExp insertDerefCond

insertDerefCond :: Insert (T.Exp, Name)
insertDerefCond nm exp = insert (exp, nm)
