{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

--
-- Ivory types QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.TypeQQ where

import           Prelude                                  hiding (exp, init)

import           Control.Monad
import           Data.List                                (nub)

import           Language.Haskell.TH                      hiding (Exp, Stmt,
                                                           Type)
import qualified Language.Haskell.TH                      as T
import           Language.Haskell.TH.Quote                ()

import qualified Ivory.Language.Area                      as I
import qualified Ivory.Language.Array                     as I
import qualified Ivory.Language.Float                     as I
import qualified Ivory.Language.IBool                     as I
import qualified Ivory.Language.IChar                     as I
import qualified Ivory.Language.IString                   as I
import qualified Ivory.Language.Proc                      as I
import qualified Ivory.Language.Proxy                     as I
import qualified Ivory.Language.Ref                       as I
import qualified Ivory.Language.Scope                     as I
import qualified Ivory.Language.Sint                      as I
import qualified Ivory.Language.Uint                      as I

import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.ParseAST
import           Ivory.Language.Syntax.Concrete.QQ.Common

--------------------------------------------------------------------------------
-- Haskell type synonyms

fromTypeDef :: TypeDef -> Q [Dec]
fromTypeDef td = case td of
#if __GLASGOW_HASKELL__ >= 709
  TypeDef syn ty srcloc -> do
    n      <- newName syn
    (t, _) <- runToQ (fromType ty)
    ln <- lnPragma srcloc
    return (ln ++ [TySynD n [] t])
#else
  TypeDef syn ty _srcloc -> do
    n      <- newName syn
    (t, _) <- runToQ (fromType ty)
    return [TySynD n [] t]
#endif

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

-- --------------------------------------------------------------------------------

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

fromScope :: Scope -> TTyVar T.Type
fromScope ma = case ma of
  Global     -> liftPromote 'I.Global
  Stack mv   -> do s <- liftPromote 'I.Stack
                   n <- case mv of
                      Nothing -> newTyVar "s" []
                      Just v  -> mkTyVar v []
                   return (AppT s n)
  PolyMem mv -> case mv of
                  Nothing -> newTyVar "c" []
                  Just v  -> mkTyVar  v   []

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

-- Syntactic check for base types
isBaseType :: Type -> Bool
isBaseType ty = case ty of
  TyVoid         -> True
  TyInt{}        -> True
  TyWord{}       -> True
  TyBool         -> True
  TyChar         -> True
  TyFloat        -> True
  TyDouble       -> True
  TyIx{}         -> True
  TyString       -> False
  TyStored{}     -> False
  TyStruct{}     -> False
  TyArray{}      -> False
  TyRef{}        -> False
  TyConstRef{}   -> False
  TySynonym{}    -> True
  LocTy ty'      -> isBaseType (unLoc ty')

maybeLiftStored :: Type -> Type
maybeLiftStored ty =
  if isBaseType ty
    then TyStored ty
    else ty

fromStoredTy :: Type -> TTyVar T.Type
fromStoredTy ty = do
  ty' <- fromType ty
  s   <- liftPromote 'I.Stored
  return (AppT s ty')

fromRef :: Name -> Scope -> Type -> TTyVar T.Type
fromRef constr mem area = do
  a      <- fromType (maybeLiftStored area)
  ma     <- fromScope mem
  return $ AppT (AppT (ConT constr) ma) a

fromArrayTy :: Type -> Either String Integer -> TTyVar T.Type
fromArrayTy area sz' = do
  let sz = case sz' of
             Left str -> ConT (mkName str)
             Right i  -> szTy i
  a      <- fromType (maybeLiftStored area)
  arr    <- liftPromote 'I.Array
  return $ AppT (AppT arr sz) a

fromStructTy :: String -> TTyVar T.Type
fromStructTy nm = do
  struct <- liftPromote 'I.Struct
  return  $ AppT struct (LitT (StrTyLit nm))

fromType :: Type -> TTyVar T.Type
fromType ty = case ty of
  TyVoid            -> liftCon ''()
  TyInt sz          -> liftCon (fromIntSz sz)
  TyWord sz         -> liftCon (fromWordSz sz)
  TyBool            -> liftCon ''I.IBool
  TyChar            -> liftCon ''I.IChar
  TyFloat           -> liftCon ''I.IFloat
  TyDouble          -> liftCon ''I.IDouble
  TyIx ix           -> return (AppT (ConT ''I.Ix) (szTy ix))
  TyStored area     -> fromStoredTy area
  TyStruct nm       -> fromStructTy nm
  TyArray a sz      -> fromArrayTy a sz
  TyRef qma qt      -> fromRef ''I.Ref      qma qt
  TyString          -> liftCon ''I.IString
  TyConstRef qma qt -> fromRef ''I.ConstRef qma qt
  TySynonym str     -> liftCon (mkName str)
  LocTy ty'         -> fromType (unLoc ty')

-- | Create a procedure type.
fromProcType :: Type -> String -> [(Type, a)] -> Q Dec
fromProcType retTy procName args = do
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
#if __GLASGOW_HASKELL__ >= 709
    map (\cl -> T.AppT (T.ConT $ toClass cl) (T.VarT nm)) classes
#else
    map (\cl -> T.ClassP (toClass cl) [T.VarT nm]) classes
#endif


  allTyVars = nub . (map (PlainTV . tyVar))
  allCtxs   = nub . (concatMap mkCxt)

--------------------------------------------------------------------------------
-- Helpers

liftPromote :: Name -> TTyVar T.Type
liftPromote = liftQ . promotedT

-- XXX Strings too?
toClass :: Class -> Name
toClass cl = case cl of
  Int -> ''I.ANat

-- Promote an integer to a type-level integer
szTy :: Integer -> T.Type
szTy = LitT . NumTyLit

liftCon :: Name -> TTyVar T.Type
liftCon = liftQ . conT


