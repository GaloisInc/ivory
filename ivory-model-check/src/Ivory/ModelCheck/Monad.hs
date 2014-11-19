{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.ModelCheck.Monad
  ( runMC
  , assertQueries
  , ModelCheck()
  , IsDefined(..)
  , SymExecSt(..)
  , SymOpts(..)
  , ProgramSt(..)
  , getState
  , setState
  , inBranch
  , askOpts
  , setSrcLoc
  , addProc
  , lookupProc
  , getRefs
  , getStructs
  , addStruct
  , withLocalRefs
  , updateStRef
  , addType
  , declUpdateEnv
  , lookupVar
  , incReservedVar
  , addQuery
  , addEnsure
  , addInvariant
  , askInline
  )
 where

import           Prelude hiding (exp)
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.Int
import           Data.Word
import           Control.Applicative
import           MonadLib
import qualified Data.Map.Lazy         as M

import Ivory.Language.Syntax.Concrete.Location
import qualified Ivory.Language.Syntax as I
import           Ivory.ModelCheck.CVC4 hiding (query, var)
import qualified Ivory.Opts.Overflow   as I

-- XXX
-- import Debug.Trace

--------------------------------------------------------------------------------
-- Types

-- | Map from AST variables to number of time seen.
type Env = M.Map Var Int

-- | Simple assertions and assertions on return values.
data Queries = Queries
  { assertQueries :: [Located Expr]
  , ensureQueries :: [Located Expr]
  } deriving Show

-- | The program state: user-defined types, declarations of variables, and
-- invariants.
data ProgramSt = ProgramSt
  { types  :: [(String, [(Var,Type)])]
  , decls  :: [Statement]
  , invars :: [Located Expr]
  , srcloc :: SrcLoc
  }

data IsDefined = Defined | Imported

-- | The full simulation state.
data SymExecSt = SymExecSt
  { funcSym  :: String
  , symEnv   :: Env
  , symSt    :: ProgramSt
  , symQuery :: Queries
  , symProcs :: M.Map I.Sym (IsDefined, I.Proc)
  , symStructs :: M.Map I.Sym I.Struct
  , symRefs  :: M.Map (I.Type, Var) [Var]
    -- ^ To track assignment to Refs during inlined calls
  , symCond  :: Expr
    -- ^ The current branch condition
  }

data SymOpts = SymOpts
  { inlineCalls :: Bool
  -- , moduleEnv   :: ModuleEnv
  }

-- type ModuleEnv = M.Map I.ModuleName I.Module

newtype ModelCheck a = ModelCheck (StateT SymExecSt (ReaderT SymOpts Id) a)
  -- { unModelCheck ::
  -- }
    deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------

-- initEnv :: Env
-- initEnv = M.empty

initSymSt :: SymExecSt
initSymSt = SymExecSt { funcSym  = ""
                      , symEnv   = mempty
                      , symSt    = mempty
                      , symQuery = mempty
                      , symProcs = overflowProcs
                      , symStructs = mempty
                      , symRefs  = mempty
                      , symCond  = true
                      }

mcVar :: String
mcVar = "mc_"

-- Make a program variable in a model-check variable.
constructVar :: Var -> Int -> Var
constructVar v i
  | i == 0    = v
  | otherwise = mcVar ++ show i ++ v

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
  f _ _ = newIx
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

addType :: String -> [(Var,Type)] -> ModelCheck ()
addType ty fs = do
  st <- get
  let ps = symSt st
  unless (isJust $ lookup ty $ types ps) $ do
    let ps' = ps { types = (ty, fs) : types ps }
    set st { symSt = ps' }

addInvariant :: Expr -> ModelCheck ()
addInvariant T   = return ()
addInvariant exp = do
  st  <- get
  loc <- getSrcLoc
  let b = symCond st
  let ps = symSt st
  let ps' = ps { invars = (b .=> exp) `at` loc : invars ps }
  set st { symSt = ps' }

-- getProgramSt :: ModelCheck ProgramSt
-- getProgramSt = return . symSt =<< get

getRefs :: ModelCheck (M.Map (I.Type, Var) [Var])
getRefs = do
  st <- get
  return (symRefs st)

getStructs :: ModelCheck (M.Map I.Sym I.Struct)
getStructs = do
  st <- get
  return (symStructs st)

addStruct :: I.Struct -> ModelCheck ()
addStruct s = do
  st <- get
  let nm = case s of
             I.Struct n _ -> n
             I.Abstract n _ -> n
  let st' = st { symStructs = M.insert nm s (symStructs st) }
  set st'
  

updateStRef :: I.Type -> Var -> Var -> ModelCheck ()
updateStRef t v v' =
  sets_ (\st -> st { symRefs = M.insert (t,v) [v'] (symRefs st) })

withLocalRefs :: ModelCheck a -> ModelCheck a
withLocalRefs m = do
  st <- get
  let refs = symRefs st
  -- sets_ (\s -> s { symRefs = mempty })
  a <- m
  sets_ (\s -> s { symRefs = refs })
  return a

getSrcLoc :: ModelCheck SrcLoc
getSrcLoc = do
  st <- get
  return (srcloc $ symSt st)

setSrcLoc :: SrcLoc -> ModelCheck ()
setSrcLoc loc = do
  st <- get
  let symSt' = (symSt st) { srcloc = loc }
  set st { symSt = symSt' }

getQueries :: ModelCheck Queries
getQueries = do
  st <- get
  return (symQuery st)

setQueries :: Queries -> ModelCheck ()
setQueries q = do
  st <- get
  set st { symQuery = q }

addQuery :: Expr -> ModelCheck ()
addQuery T = return ()
addQuery exp = do
  st  <- getState
  loc <- getSrcLoc
  let b = symCond st
  q   <- getQueries
  setQueries q { assertQueries = (b .=> exp) `at` loc : assertQueries q }

addEnsure :: Expr -> ModelCheck ()
addEnsure T = return ()
addEnsure exp = do
  st  <- getState
  loc <- getSrcLoc
  let b = symCond st
  q   <- getQueries
  setQueries q { ensureQueries = (b .=> exp) `at` loc : ensureQueries q }

addProc :: IsDefined -> I.Proc -> ModelCheck ()
addProc d p = do
  st <- get
  set st { symProcs = M.insert (I.procSym p) (d,p) (symProcs st) }

lookupProc :: I.Sym -> ModelCheck (IsDefined, I.Proc)
lookupProc nm = do
  st <- get
  case M.lookup nm (symProcs st) of
    Nothing -> error $ "couldn't find proc: " ++ show nm
    Just p  -> return p

askInline :: ModelCheck Bool
askInline = asks inlineCalls

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

inBranch :: Expr -> ModelCheck a -> ModelCheck a
inBranch b doThis = do
  b' <- symCond <$> getState
  sets_ (\s -> s { symCond = b })
  res <- doThis
  sets_ (\s -> s { symCond = b' })
  return res
  
runMC :: SymOpts -> ModelCheck a -> (a, SymExecSt)
runMC opts (ModelCheck m) = runId (runReaderT opts (runStateT initSymSt m))

--------------------------------------------------------------------------------
-- Instances

instance StateM ModelCheck SymExecSt where
  get = ModelCheck get
  set = ModelCheck . set

getState :: ModelCheck SymExecSt
getState = get

setState :: SymExecSt -> ModelCheck ()
setState = set

instance ReaderM ModelCheck SymOpts where
  ask = ModelCheck ask

askOpts :: ModelCheck SymOpts
askOpts = ask

instance Monoid Queries where
  mempty = Queries { assertQueries = []
                   , ensureQueries = []
                   }
  (Queries a0 e0) `mappend` (Queries a1 e1) =
    Queries { assertQueries = a0 ++ a1
            , ensureQueries = e0 ++ e1
            }

instance Monoid ProgramSt where
  mempty = ProgramSt { types  = mempty
                     , decls  = []
                     , invars = []
                     , srcloc = NoLoc
                     }
  (ProgramSt t0 d0 e0 _) `mappend` (ProgramSt t1 d1 e1 _) =
    ProgramSt { types  = t0 <> t1
              , decls  = d0 ++ d1
              , invars = e0 ++ e1
              , srcloc = NoLoc    -- XXX: should be able to do better than this..
              }

--------------------------------------------------------------------------------
-- Contracts for overflow assertions

overflowProcs :: M.Map I.Sym (IsDefined, I.Proc)
overflowProcs = M.fromList . map (\(s,p) -> (s, (Imported, p))) $ concat
  [ map mkAdd is, map mkAdd ws
  , map mkSub is, map mkSub ws
  , map mkMul is, map mkMul ws
  , map mkDiv is, map mkDiv ws
  ]
  where
  is = map I.TyInt  [I.Int8,  I.Int16,  I.Int32,  I.Int64]
  ws = map I.TyWord [I.Word8, I.Word16, I.Word32, I.Word64]

mkAdd :: I.Type -> (I.Sym, I.Proc)
mkAdd t = (nm, pc)
  where
  nm = I.addBase I.<+> I.ext t
  pc = I.Proc { I.procSym = nm
              , I.procRetTy = I.TyBool
              , I.procArgs = [I.Typed t v0, I.Typed t v1]
              , I.procBody = []
              , I.procRequires = []
              , I.procEnsures = [I.Ensure $ I.CondBool $ I.ExpOp (I.ExpEq I.TyBool)
                                 [ I.ExpVar I.retval
                                 , I.ExpOp I.ExpAnd
                                  [ I.ExpOp (I.ExpLt True t)
                                    [minBound, I.ExpVar v0 + I.ExpVar v1]
                                  , I.ExpOp (I.ExpGt True t)
                                    [maxBound, I.ExpVar v0 + I.ExpVar v1]
                                  ]
                                 ]
                                ]
              }
  v0 = I.VarName "var0"
  v1 = I.VarName "var1"

mkSub :: I.Type -> (I.Sym, I.Proc)
mkSub t = (nm, pc)
  where
  nm = I.subBase I.<+> I.ext t
  pc = I.Proc { I.procSym = nm
              , I.procRetTy = I.TyBool
              , I.procArgs = [I.Typed t v0, I.Typed t v1]
              , I.procBody = []
              , I.procRequires = []
              , I.procEnsures = [I.Ensure $ I.CondBool $ I.ExpOp (I.ExpEq I.TyBool)
                                 [ I.ExpVar I.retval
                                 , I.ExpOp I.ExpAnd
                                   [ I.ExpOp (I.ExpLt True t)
                                     [minBound, I.ExpVar v0 - I.ExpVar v1]
                                   , I.ExpOp (I.ExpGt True t)
                                     [maxBound, I.ExpVar v0 - I.ExpVar v1]
                                   ]
                                 ]
                                ]
              }
  v0 = I.VarName "var0"
  v1 = I.VarName "var1"

-- NOTE: very crude approximation of multiplication overflow
mkMul :: I.Type -> (I.Sym, I.Proc)
mkMul t = (nm, pc)
  where
  nm = I.mulBase I.<+> I.ext t
  pc = I.Proc { I.procSym = nm
              , I.procRetTy = I.TyBool
              , I.procArgs = [I.Typed t v0, I.Typed t v1]
              , I.procBody = []
              , I.procRequires = []
              , I.procEnsures = [ I.Ensure $ I.CondBool $ I.ExpOp (I.ExpEq I.TyBool)
                                  [ I.ExpVar I.retval
                                  , I.ExpOp I.ExpAnd
                                    [ I.ExpOp I.ExpOr
                                      [ I.ExpOp (I.ExpNeq t) [ I.ExpVar v0, minBound ]
                                      , I.ExpOp (I.ExpNeq t) [ I.ExpVar v1, -1 ]
                                      ]
                                    , I.ExpOp I.ExpAnd
                                      [ I.ExpOp I.ExpOr
                                        [ I.ExpOp (I.ExpNeq t) [ I.ExpVar v1, minBound ]
                                        , I.ExpOp (I.ExpNeq t) [ I.ExpVar v0, -1 ]
                                        ]
                                      , I.ExpOp I.ExpOr
                                        [ I.ExpOp (I.ExpEq t) [ I.ExpVar v0, 0 ]
                                        , I.ExpOp I.ExpOr
                                          [ I.ExpOp (I.ExpEq t) [ I.ExpVar v1, 0 ]
                                          , I.ExpOp I.ExpAnd
                                            [ I.ExpOp (I.ExpLt True t)
                                              [ I.ExpVar v0, sqrtMax ]
                                            , I.ExpOp (I.ExpLt True t)
                                              [ I.ExpVar v1, sqrtMax ]
                                            ]
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
              }
  v0 = I.VarName "var0"
  v1 = I.VarName "var1"

  sqrtMax :: I.Expr
  sqrtMax = fromInteger . floor . (sqrt :: Double -> Double) . fromInteger 
          $ case t of 
              I.TyInt I.Int8    -> toInteger (maxBound :: Int8)
              I.TyInt I.Int16   -> toInteger (maxBound :: Int16)
              I.TyInt I.Int32   -> toInteger (maxBound :: Int32)
              I.TyInt I.Int64   -> toInteger (maxBound :: Int64)
              I.TyWord I.Word8  -> toInteger (maxBound :: Word8)
              I.TyWord I.Word16 -> toInteger (maxBound :: Word16)
              I.TyWord I.Word32 -> toInteger (maxBound :: Word32)
              I.TyWord I.Word64 -> toInteger (maxBound :: Word64)
              _                 -> error $ "Unexpected type for mkMul: " ++ show t


mkDiv :: I.Type -> (I.Sym, I.Proc)
mkDiv t = (nm, pc)
  where
  nm = I.divBase I.<+> I.ext t
  pc = I.Proc { I.procSym = nm
              , I.procRetTy = I.TyBool
              , I.procArgs = [I.Typed t v0, I.Typed t v1]
              , I.procBody = []
              , I.procRequires = []
              , I.procEnsures = [I.Ensure $ I.CondBool $ I.ExpOp (I.ExpEq I.TyBool)
                                 [ I.ExpVar I.retval
                                 , I.ExpOp I.ExpAnd
                                   [ I.ExpOp (I.ExpNeq t) [ I.ExpVar v1, 0 ]
                                   --NOTE: this could be omitted in the unsigned case
                                   , I.ExpOp I.ExpOr
                                     [ I.ExpOp (I.ExpNeq t) [ I.ExpVar v0, minBound ]
                                     , I.ExpOp (I.ExpNeq t) [ I.ExpVar v1, -1 ]
                                     ]
                                   ]
                                 ]
                                ]
              }
  v0 = I.VarName "var0"
  v1 = I.VarName "var1"
