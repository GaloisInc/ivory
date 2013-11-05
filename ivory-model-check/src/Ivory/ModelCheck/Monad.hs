{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.ModelCheck.Monad
  ( runMC
  , symEnv
  , assertQueries
  , symQuery
  , symSt
  , decls
  , eqns
  , ModelCheck()
  , SymExecSt()
  , funcSym
  , updateEnv
  , addDecl
  , addQuery
  , addEqn
  , resetSt
  , mergeSt
  ) where

import           Data.Monoid
import           Control.Applicative
import           MonadLib
import qualified Data.Map.Lazy         as M

import Ivory.ModelCheck.CVC4

--------------------------------------------------------------------------------
-- Types

-- | Map from AST variables to number of time seen.
type Env = M.Map Var Int

-- | Simple assertions and assertions on return values.
data Queries = Queries
  { assertQueries :: [Expr]
  , ensureQueries :: [Expr]
  } deriving Show

-- | The program state: declarations and equations.
data ProgramSt = ProgramSt
  { decls :: [Statement]
  , eqns  :: [Expr]
  } deriving Show

-- | The full simulation state.
data SymExecSt = SymExecSt
  { funcSym  :: String
  , symEnv   :: Env
  , symSt    :: ProgramSt
  , symQuery :: Queries
  } deriving Show

newtype ModelCheck a = ModelCheck
  { unModelCheck :: StateT SymExecSt Id a
  } deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------

initEnv :: Env
initEnv = M.empty

initSymSt :: SymExecSt
initSymSt = SymExecSt { funcSym  = ""
                      , symEnv   = mempty
                      , symSt    = mempty
                      , symQuery = mempty
                      }

-- | Take an AST variable, a variable store, and returns an updated store and
-- the original variable if it's not in the store or the store variable if it
-- exists.
getEnvVar :: Var -> Env -> (Var, Env)
getEnvVar v env =
  let (mi, env') = M.insertLookupWithKey f v 0 env in
  case mi of
    Nothing -> (v, env')
    Just i -> let v' = "mc_" ++ v ++ "_" ++ show (i+1) in
              (v', env')
  where
  f _ _ old = old + 1

addDecl :: Statement -> ModelCheck ()
addDecl decl = do
  st <- get
  let ps = symSt st
  let ps' = ps { decls = decl : decls ps }
  set st { symSt = ps' }

addEqn :: Expr -> ModelCheck ()
addEqn exp = do
  st <- get
  let ps = symSt st
  let ps' = ps { eqns = exp : eqns ps }
  set st { symSt = ps' }

getProgramSt :: ModelCheck ProgramSt
getProgramSt = return . symSt =<< get

getQueries :: ModelCheck Queries
getQueries = do
  st <- get
  return (symQuery st)

setQueries :: Queries -> ModelCheck ()
setQueries q = do
  st <- get
  set st { symQuery = q }

addQuery :: Expr -> ModelCheck ()
addQuery exp = do
  q  <- getQueries
  setQueries q { assertQueries = exp : assertQueries q }

addEnsure :: Expr -> ModelCheck ()
addEnsure exp = do
  q  <- getQueries
  setQueries q { ensureQueries = exp : ensureQueries q }

updateEnv :: Var -> ModelCheck Var
updateEnv v = do
  st <- get
  let (v', env) = getEnvVar v (symEnv st)
  set st { symEnv = env }
  return v'

-- | Reset all the state except for the environment.
resetSt :: ModelCheck ()
resetSt = do
  st <- get
  set st { symSt    = mempty
         , symQuery = mempty
         }

-- | Merge at a join point by taking the disjunction of equations.  Take the
-- disjunction of equations about program state.  Append everything else (Ivory
-- AST should guarantee unique names).
mergeSt :: SymExecSt -> SymExecSt -> ModelCheck ()
mergeSt st0 st1 = do
  st <- get
  let env     = M.unions [symEnv st, symEnv st0, symEnv st1]

  let ps0     = symSt st0
  let ps1     = symSt st1

  let qs0     = symQuery st0
  let qs1     = symQuery st1

  let impls assts q   = and' assts .=> q
  let mkAssts pss qss = map (impls (eqns pss)) (assertQueries qss)
  let mkEns pss qss   = map (impls (eqns pss)) (ensureQueries qss)
  let asst0   = mkAssts ps0 qs0
  let asst1   = mkAssts ps1 qs1
  let ens0    = mkEns ps0 qs0
  let ens1    = mkEns ps1 qs1

  let query   =         symQuery st
              `mappend` Queries asst0 ens0
              `mappend` Queries asst1 ens1

  let ps      = symSt st
  let decls'  = decls ps0 ++ decls ps1 ++ decls ps
  let ps'     = ps { decls = decls' }
  set SymExecSt { funcSym  = funcSym st0
                , symEnv   = env
                , symSt    = ps'
                , symQuery = query
                }

runMC :: ModelCheck a -> (a, SymExecSt)
runMC (ModelCheck m) = runId (runStateT initSymSt m)

--------------------------------------------------------------------------------
-- Instances

instance StateM ModelCheck SymExecSt where
  get = ModelCheck get
  set = ModelCheck . set

instance Monoid Queries where
  mempty = Queries { assertQueries = []
                   , ensureQueries = []
                   }
  (Queries a0 e0) `mappend` (Queries a1 e1) =
    Queries { assertQueries = a0 ++ a1
            , ensureQueries = e0 ++ e1
            }

instance Monoid ProgramSt where
  mempty = ProgramSt { decls = []
                     , eqns  = []
                     }
  (ProgramSt d0 e0) `mappend` (ProgramSt d1 e1) =
    ProgramSt { decls = d0 ++ d1
              , eqns  = e0 ++ e1
              }

-- instance Monoid SymExecSt where
--   mempty = SymExecSt { symEnv   = mempty
--                      , symSt    = mempty
--                      , symQuery = mempty
--                      }
--   (SymExecSt e0 s0 q0) `mappend` (SymExecSt e1 s1 q1) =
--     SymExecSt { symEnv   = e0 `mappend` e1
--               , symSt    = s0 `mappend` s1
--               , symQuery = q0 `mappend` q1
--               }

--------------------------------------------------------------------------------
