{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

--
-- Ivory types QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.TypeQQ where

import           Prelude hiding (exp, init)
import qualified Prelude as P

import Data.List (nub)
import Control.Monad
import GHC.TypeLits

import Ivory.Language.Syntax.Concrete.QQ.Common

import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import qualified Language.Haskell.TH as T
import           Language.Haskell.TH.Quote()

import qualified Ivory.Language as I

import Ivory.Language.Syntax.Concrete.ParseAST

--------------------------------------------------------------------------------

-- Data type to keep track of class constraints
data Class = Int -- SingI type constraint
  deriving (Show, Read, Eq, Ord)

data TyVar = TyVar
  { tyVar       :: Name
  , constraints :: [Class]
  } deriving (Show, Eq, Ord)

-- We use a state monad over the Q monad to keep track of type variables that we
-- need to quantify later.
type TTyVar a = QStM TyVar a

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
  Global     -> liftPromote 'I.Global
  Stack      -> do s <- liftPromote 'I.Stack
                   n <- newTyVar "s" []
                   return (AppT s n)
  PolyMem mv -> case mv of
                  Nothing -> newTyVar "c" []
                  Just v  -> mkTyVar  "v" []

--------------------------------------------------------------------------------

constrTyVar :: Name -> [Class] -> TTyVar T.Type
constrTyVar n classes = do
  insert (TyVar n classes)
  return (VarT n)

newTyVar :: String -> [Class] -> TTyVar T.Type
newTyVar v classes = do
  n <- liftQ (newName v)
  constrTyVar n classes

mkTyVar :: String -> [Class] -> TTyVar T.Type
mkTyVar v classes = do
  n <- return (mkName v)
  constrTyVar n classes

--------------------------------------------------------------------------------

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

fromRef :: Name -> MemArea -> Type -> TTyVar T.Type
fromRef constr mem ty = do
  ty'    <- storedTy ty
  ma     <- fromMemArea mem
  return $ AppT (AppT (ConT constr) ma) ty'

fromArray :: Type -> Integer -> TTyVar T.Type
fromArray ty sz = do
  let szTy = LitT (NumTyLit sz)
  ty'    <- storedTy ty
  arr    <- liftPromote 'I.Array
  return $ AppT (AppT arr szTy) ty'

fromStruct :: Name -> TTyVar T.Type
fromStruct nm = error $ "from struct QQ not defined: " ++ show nm

fromType :: Type -> TTyVar T.Type
fromType ty = case ty of
  TyVoid            -> c ''()
  TyInt sz          -> c (fromIntSz sz)
  TyWord sz         -> c (fromWordSz sz)
  TyBool            -> c ''I.IBool
  TyChar            -> c ''I.IChar
  TyFloat           -> c ''I.IFloat
  TyDouble          -> c ''I.IDouble
  TyRef qma qt      -> fromRef ''I.Ref      qma qt
  TyConstRef qma qt -> fromRef ''I.ConstRef qma qt
  TyArr ty' sz      -> fromArray ty' sz
  TyStruct nm       -> fromStruct (mkName nm)
  where
  c = liftQ . conT

-- | Create a procedure type.
fromProcType :: ProcDef -> Q Dec
fromProcType (ProcDef retTy procName args _) = do
  arr                 <- promotedT '(I.:->)
  (ret,retTyVars)     <- runToQ (fromType retTy)
  (argVars,argTyVars) <- fromArgs
  let def = AppT (ConT ''I.Def) (AppT (AppT arr argVars) ret)
  -- All the type variables
  let tyVars = argTyVars ++ retTyVars
  return $ SigD (mkName procName)
                (ForallT (allTyVars tyVars)
                         -- construct their class constaints
                         (allCtxs tyVars)
                         def
                )
  where

  fromArgs :: Q (T.Type, [TyVar])
  fromArgs = runToQ $ foldM go PromotedNilT
                               (reverse $ fst $ unzip args)

  -- Grab the type variables to quantify them.
  go :: T.Type -> Type -> TTyVar T.Type
  go acc ty = do
    ty' <- fromType ty
    return (AppT (AppT PromotedConsT ty') acc)

  -- Construct the class constraints per type variable
  mkCxt :: TyVar -> [T.Pred]
  mkCxt (TyVar nm classes) =
    map (\cl -> T.ClassP (toClass cl) [T.VarT nm]) classes

  allTyVars = nub . (map (PlainTV . tyVar))
  allCtxs   = nub . (concatMap mkCxt)


--------------------------------------------------------------------------------
-- Helpers

liftPromote :: Name -> TTyVar T.Type
liftPromote = liftQ . promotedT

toClass :: Class -> Name
toClass cl = case cl of
  Int -> ''SingI
