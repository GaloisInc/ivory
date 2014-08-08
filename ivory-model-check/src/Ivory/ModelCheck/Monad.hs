{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.ModelCheck.Monad
  ( runMC
  , assertQueries
  , ModelCheck()
  , SymExecSt(..)
  , ProgramSt(..)
  , getState
  , setState
  , joinState
  , addType
  , declUpdateEnv
  , lookupVar
  , incReservedVar
  , addQuery
  , addInvariant
  , resetSt
  , branchSt
  )
 where

import           Prelude hiding (exp)
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Control.Applicative
import           MonadLib
import qualified Data.Map.Lazy         as M

import           Ivory.ModelCheck.CVC4 hiding (query, var)

-- XXX
--import Debug.Trace

--------------------------------------------------------------------------------
-- Types

-- | Map from AST variables to number of time seen.
type Env = M.Map Var Int

-- | Simple assertions and assertions on return values.
data Queries = Queries
  { assertQueries :: [Expr]
  , ensureQueries :: [Expr]
  } deriving Show

-- | The program state: user-defined types, declarations of variables, and
-- invariants.
data ProgramSt = ProgramSt
  { types  :: [Type]
  , decls  :: [Statement]
  , invars :: [Expr]
  }

-- | The full simulation state.
data SymExecSt = SymExecSt
  { funcSym  :: String
  , symEnv   :: Env
  , symSt    :: ProgramSt
  , symQuery :: Queries
  }

newtype ModelCheck a = ModelCheck
  { unModelCheck :: StateT SymExecSt Id a
  } deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------

-- initEnv :: Env
-- initEnv = M.empty

initSymSt :: SymExecSt
initSymSt = SymExecSt { funcSym  = ""
                      , symEnv   = mempty
                      , symSt    = mempty
                      , symQuery = mempty
                      }

mcVar :: String
mcVar = "mc_"

-- Make a program variable in a model-check variable.
constructVar :: Var -> Int -> Var
constructVar v i
  | i == 0    = v
  | otherwise = mcVar ++ show i ++ v
  where

takeInt :: Char -> Bool
takeInt c = case reads [c] :: [(Int, String)] of
              [(_, "")] -> True
              _         -> False

parseVar :: Var -> Var
parseVar v = fromMaybe v (parseMcVar v)

parseMcVar :: Var -> Maybe Var
parseMcVar v = return . dropWhile takeInt =<< stripPrefix mcVar v

-- | Take an AST variable, a variable store, and returns an updated store and
-- an evironment variable.
getEnvVar :: Var -> Env -> (Var, Env)
getEnvVar var env =
  let v = parseVar var in
  let (mi, env') = M.insertLookupWithKey f v 0 env in
  case mi of
    Nothing -> (v, env')
    Just i  -> (constructVar v (newIx i), env')
  where
  f _ _ old = newIx old
  newIx i = i+1

-- | Lookup a variable in the store.
lookupEnvVar :: Var -> Env -> Var
lookupEnvVar var env =
  let v = parseVar var in
  let mv = M.lookup v env in
  case mv of
    Nothing -> error $ "Variable " ++ v ++ " not in env:\n" ++ show env
    Just i  -> constructVar v i

addDecl :: Statement -> ModelCheck ()
addDecl decl = do
  st <- get
  let ps = symSt st
  let ps' = ps { decls = decl : decls ps }
  set st { symSt = ps' }

addType :: Type -> ModelCheck ()
addType ty = do
  st <- get
  let ps = symSt st
  if ty `elem` types ps then return ()
    else do let ps' = ps { types = ty : types ps }
            set st { symSt = ps' }

addInvariant :: Expr -> ModelCheck ()
addInvariant exp = do
  st <- get
  let ps = symSt st
  let ps' = ps { invars = exp : invars ps }
  set st { symSt = ps' }

-- getProgramSt :: ModelCheck ProgramSt
-- getProgramSt = return . symSt =<< get

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

-- addEnsure :: Expr -> ModelCheck ()
-- addEnsure exp = do
--   q  <- getQueries
--   setQueries q { ensureQueries = exp : ensureQueries q }

-- | Lookup a variable in the environment.  If it's not in there return a fresh
-- variable (and update the environment) and declare it (which is why we need
-- the type).  Otherwise, return the environment variable (and update the
-- environment).
declUpdateEnv :: Type -> Var -> ModelCheck Var
declUpdateEnv t v = do
  v' <- updateEnv v
  addDecl (varDecl v' t)
  return v'

-- | Update the environment without declaring it.
updateEnv :: Var -> ModelCheck Var
updateEnv v = do
  st <- get
  let (v', env) = getEnvVar v (symEnv st)
  set st { symEnv = env }
  return v'

-- | A special reserved variable the model-checker will use when it wants to
-- create new program variables.
--
-- XXX We assume this is in a separate namespace from ordinary program
-- variables.
reservedVar :: Var
reservedVar = "mcTmp"

-- | Increment the count of uses of 'reservedVar'.
incReservedVar :: Type -> ModelCheck Var
incReservedVar t = declUpdateEnv t reservedVar

-- | Find a variable in the store.  Throws an error if it does not exist.
lookupVar :: Var -> ModelCheck Var
lookupVar v = do
  st <- get
  return $ lookupEnvVar v (symEnv st)

-- | Reset all the state except for the environment.
resetSt :: SymExecSt -> ModelCheck ()
resetSt st =
  set st { symSt    = mempty
         , symQuery = mempty
         }

-- | Makes the invariants and queries in the current state conditional on the
-- given expression holding.
branchSt :: Expr -> ModelCheck ()
branchSt exp = do
  st <- get
  let ps = symSt st
  let invars' = implies (invars ps)
  let ps' = ps { decls  = decls ps
               , invars = invars' }
  let qs = symQuery st
  let asserts' = implies (assertQueries qs)
  let ensures' = implies (ensureQueries qs)
  let queries' = qs { assertQueries = asserts'
                    , ensureQueries = ensures' }
  let st' = SymExecSt { funcSym  = funcSym st
                      , symEnv   = symEnv st
                      , symSt    = ps'
                      , symQuery = queries'
                      }
  set st'
  where
  implies = map (exp .=>)

joinState :: SymExecSt -> ModelCheck SymExecSt
joinState st0 = do
  st1 <- get
  -- This is the right order.
  let st = st1 `mappend` st0
  set st
  return st

runMC :: ModelCheck a -> (a, SymExecSt)
runMC (ModelCheck m) = runId (runStateT initSymSt m)

--------------------------------------------------------------------------------
-- Instances

instance StateM ModelCheck SymExecSt where
  get = ModelCheck get
  set = ModelCheck . set

getState :: ModelCheck SymExecSt
getState = get

setState :: SymExecSt -> ModelCheck ()
setState = set

instance Monoid Queries where
  mempty = Queries { assertQueries = []
                   , ensureQueries = []
                   }
  (Queries a0 e0) `mappend` (Queries a1 e1) =
    Queries { assertQueries = a0 ++ a1
            , ensureQueries = e0 ++ e1
            }

instance Monoid ProgramSt where
  mempty = ProgramSt { types  = []
                     , decls  = []
                     , invars = []
                     }
  (ProgramSt t0 d0 e0) `mappend` (ProgramSt t1 d1 e1) =
    ProgramSt { types  = t0 ++ t1
              , decls  = d0 ++ d1
              , invars = e0 ++ e1
              }

instance Monoid SymExecSt where
  mempty = SymExecSt { funcSym  = ""
                     , symEnv   = mempty
                     , symSt    = mempty
                     , symQuery = mempty
                     }
  (SymExecSt f0 e0 s0 q0) `mappend` (SymExecSt f1 e1 s1 q1)
    | f0 /= f1 = error "Sym states have different function symbols."
    | otherwise =
      SymExecSt { funcSym  = f0
                , symEnv   = e0 `M.union` e1
                , symSt    = s0 `mappend` s1
                , symQuery = q0 `mappend` q1
                }

--------------------------------------------------------------------------------
