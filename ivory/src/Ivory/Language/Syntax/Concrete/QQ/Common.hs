{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

--
-- Helpers for QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.Common
  ( VarEnv()
  , Insert()
  , QStM()
  , Area(..)
  , Key()
  , Call(..)
  , TStmtM()
  , getVar
  , lookupVar
  , callit
  , mkVar
  , lookupDerefVar
  , expToCall
  , expToArea
  , liftQ
  , insert
  , runToQ
  , keyToCall
  , keyToArea
  , isCall
  , isArea
  , collectBindExps
  , runToSt
#if __GLASGOW_HASKELL__ >= 709
  , lnPragma
#endif
  ) where

import Prelude ()
import Prelude.Compat hiding (exp)

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T

import           Data.List  (nub)
import           MonadLib   (set, get)
import qualified MonadLib   as M
import qualified Data.DList as D

import Ivory.Language.Syntax.Concrete.ParseAST
import Ivory.Language.Syntax.Concrete.Location

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

-- Expressions that are calls in the language.
data Call = Call FnSym [Exp]
  deriving (Show, Read, Eq)

-- Should only be called on parsed expressions that are function calls. Error
-- otherwise.
expToCall :: FnSym -> [Exp] -> Call
expToCall sym args = Call sym args

-- Expression that are areas in the language.
data Area =
    AreaVar String
  | AddrOf Area
  | ArrayArea Area Exp
  | StructArea Area Area
  deriving (Show, Read, Eq)

-- Should only be called on parsed expressions that are areas (arguments to
-- ExpDeref). Error otherwise.
expToArea :: Exp -> Area
expToArea exp = case exp of
  ExpVar v        -> AreaVar v
  ExpAddrOf v     -> AddrOf (AreaVar v)
  -- e1 below can't be an area---it's an index into the array.
  ExpArray  e0 e1 -> ArrayArea (expToArea e0) e1
  ExpStruct e0 e1 -> StructArea (expToArea e0) (expToArea e1)
  LocExp e        -> expToArea (unLoc e)
  _               -> error $ "Expression " ++ show exp ++ " instead of area."

-- Collect up the variables used in an expression that require an Ivory statement.
collectBindExps :: Exp -> [Key]
collectBindExps exp = nub $ case exp of
  ExpLit{}                -> []
  ExpVar{}                -> []
  ExpRet{}                -> []
  ExpOp _ args            -> concatMap collectBindExps args
  IvoryMacroExp (_, args) -> concatMap collectBindExps args
  -- expressions used in array indexing are extracted in processing areas.
  ExpDeref e              -> [areaToKey (expToArea e)]
  ExpArray e0 e1          -> collectBindExps e0 ++ collectBindExps e1
  ExpStruct e0 e1         -> collectBindExps e0 ++ collectBindExps e1
  ExpCall fn args         -> [callToKey (Call fn args)]
  ExpAddrOf{}             -> []
  LocExp le               -> collectBindExps (unLoc le)

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

type Key = Either Area Call

-- | Dereference expression environment
type VarEnv = [(Key, Name)]

areaToKey :: Area -> Key
areaToKey = Left

callToKey :: Call -> Key
callToKey = Right

isArea :: Key -> Bool
isArea (Left _) = True
isArea _        = False

isCall :: Key -> Bool
isCall (Right _) = True
isCall _         = False

keyToArea :: Key -> Area
keyToArea (Left area) = area
keyToArea _           = error $ "keyToArea passed a non-area"

keyToCall :: Key -> Call
keyToCall (Right call) = call
keyToCall _            = error $ "keyToCall passed a non-area"

-- Returns the fresh variable that is the do-block binding from the dereference
-- statement.
lookupDerefVar :: Area -> VarEnv -> Name
lookupDerefVar area = getVar (areaToKey area)

-- Returns the fresh variable that is the do-block binding from the dereference
-- statement.
lookupVar :: Call -> VarEnv -> Name
lookupVar call = getVar (Right call)

getVar :: Key -> VarEnv -> Name
getVar a env =
  case lookup a env of
    Nothing -> error "Internal error in getVar"
    Just rv -> rv

--------------------------------------------------------------------------------

-- | How to insert an expression, given its type, the binding variable, and the
-- TH expression.
type Insert a = Key -> Name -> T.Exp -> QStM a ()

--------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 709
lnPragma :: SrcLoc -> Q [Dec]
lnPragma srcloc =
  case srcLoclinePragma srcloc of
    Nothing         -> return []
    Just (ln, file) -> (:[]) `fmap` pragLineD ln file
#endif
