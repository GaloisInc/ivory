{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Helpers for QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.Common where

import Prelude hiding (exp)

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T

import           Data.List  (nub)
import           MonadLib   (set, get)
import qualified MonadLib   as M
import           Data.Monoid
import qualified Data.DList as D
import           Control.Applicative

import Ivory.Language.Syntax.Concrete.ParseAST

--------------------------------------------------------------------------------
-- Monad for inserting values over the Q monad.

newtype QStM a b = QStM
  { unQStM :: M.StateT (D.DList a) T.Q b
  } deriving (Functor, Monad, Applicative)

instance M.StateM (QStM a) (D.DList a) where
  get = QStM M.get
  set = QStM . M.set

insert :: a -> QStM a ()
insert a = do
  st <- get
  set (D.snoc st a)

runToQ :: QStM a b -> Q (b, [a])
runToQ m = do
  (r, st) <- M.runStateT mempty (unQStM m)
  return (r, D.toList st)

liftQ :: Q b -> QStM a b
liftQ = QStM . M.lift

runToSt :: QStM a b -> Q [a]
runToSt m = snd `fmap` runToQ m

--------------------------------------------------------------------------------

-- Collect up the variables used in a dereference expression to be used in
-- making a monadic Ivory statement.
collectRefExps :: Exp -> [Area]
collectRefExps exp = nub $ case exp of
  ExpDeref area        -> [area]
  ExpOp _ args         -> concatMap collectRefExps args
  ExpLit{}             -> []
  ExpVar{}             -> []
  ExpRet{}             -> []
  IvoryMacroExp _ args -> concatMap collectRefExps args

  -- where
  -- collectAreas area = case area of
  --   AreaVar{}      -> []
  --   ArrayArea a e  -> area : (collectAreas a ++ collectRefExps e)
  --   StructArea a f -> collectAreas a

--------------------------------------------------------------------------------
-- Helpers

mkVar :: String -> T.Exp
mkVar = VarE . mkName

callit :: T.Exp -> [T.Exp] -> T.Exp
callit f args = foldl AppE f args

--------------------------------------------------------------------------------

-- We use a state monad over the Q monad to keep track of expressions in the
-- parsed language that we'll turn into statements in Ivory.
type TStmtM a = QStM T.Stmt a

--------------------------------------------------------------------------------

-- | Dereference expression environment
type DerefVarEnv = [(Area, Name)]
