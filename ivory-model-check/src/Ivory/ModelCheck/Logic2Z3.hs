{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    FlexibleInstances, TypeOperators, DataKinds, TemplateHaskell, QuasiQuotes,
    ViewPatterns #-}

module Ivory.ModelCheck.Logic2Z3 where

import Prelude ()
import Prelude.Compat hiding (exp)

import Data.Typeable
import Data.Bifunctor

import MonadLib
--import MonadLib (StateM(..),StateT(..),runStateT,
--                 ReaderM(..),RunReaderM(..),ReaderT,runReaderT,MonadT(..))
-- import Control.Monad.IO.Class (MonadIO(..))

--import Z3.Monad
import qualified Z3.Monad as Z3
import qualified Z3.Opts as Z3Opts

import Data.Binding.Hobbits
import Data.Type.RList

import Ivory.ModelCheck.Logic


----------------------------------------------------------------------
-- Typed Z3 expressions
----------------------------------------------------------------------

data Z3Expr a where
  Z3Expr_lit :: Z3.AST -> Z3Expr (Literal a)
  Z3Expr_prop :: Z3.AST -> Z3Expr Prop
  Z3Expr_fun :: (Z3Expr a -> Z3Expr b) -> Z3Expr (a -> b)


----------------------------------------------------------------------
-- Z3 variable contexts
----------------------------------------------------------------------

-- | The type of contexts of that say how to translate free variables into
-- Z3. For each type in @ctx@, a Z3 context contains either an existential
-- variable, which is a fresh Z3 constant symbol, or a universal variable, which
-- is referred to be deBruijn index, i.e., by its position in the Z3
-- context. Note that existential variables /should not/ be anything other than
-- a constant symbol (specifically, if they contain deBruijn indices, they will
-- not get translated correctly). Z3 contexts also support lifting, which is the
-- insertion of additional free universal variables into the context that are
-- not reflected in @ctx@.
data Z3Ctx ctx where
  Z3Ctx_EVar :: Z3Ctx ctx -> Z3Expr a -> Z3Ctx (ctx :> a)
  Z3Ctx_UVar :: Z3Ctx ctx -> Z3Ctx (ctx :> a)
  Z3Ctx_LiftedVar :: Z3Ctx ctx -> Z3Ctx ctx

-- | Lookup a variable in a 'Z3Ctx', returning either a 'Z3.AST', for an
-- existential variable, or a deBruijn index for a universal variable
z3ctx_lookup :: Z3Ctx ctx -> Member ctx a -> Either (Z3Expr a) Integer
z3ctx_lookup (Z3Ctx_EVar _ expr) Member_Base = Left expr
z3ctx_lookup (Z3Ctx_UVar _) Member_Base = Right 0
z3ctx_lookup (Z3Ctx_EVar ctx _) (Member_Step memb) = z3ctx_lookup ctx memb
z3ctx_lookup (Z3Ctx_UVar ctx) (Member_Step memb) =
  bimap id (+ 1) $ z3ctx_lookup ctx memb
z3ctx_lookup (Z3Ctx_LiftedVar ctx) memb =
  bimap id (+ 1) $ z3ctx_lookup ctx memb

-- | Lower a 'Z3Ctx' by moving it to a context without the last type in the
-- context. If the variable for this last type is a universal, preserve it as a
-- lifted variable in the output 'Z3Ctx'.
z3ctx_lower1 :: Z3Ctx (ctx :> a) -> Z3Ctx ctx
z3ctx_lower1 (Z3Ctx_EVar ctx _) = ctx
z3ctx_lower1 (Z3Ctx_UVar ctx) = Z3Ctx_LiftedVar ctx
z3ctx_lower1 (Z3Ctx_LiftedVar ctx) = Z3Ctx_LiftedVar $ z3ctx_lower1 ctx


----------------------------------------------------------------------
-- Monad for interacting with Z3
----------------------------------------------------------------------

-- | State information used in constructing Z3 terms
data Z3Info =
  Z3Info
  {
    fresh_id :: Integer
    -- ^ Index used to create fresh constants and function symbols
  }

-- | Monad built on top of 'Z3' to build Z3 expressions using the state
-- information in a 'Z3Info' and a 'Z3Ctx' indicating how to translate bound
-- variables.
newtype Z3m ctx a =
  Z3m { runZ3m :: ReaderT (Z3Ctx ctx) (StateT Z3Info Z3.Z3) a }
  deriving (Functor,Applicative,Monad)

instance StateM (Z3m ctx) Z3Info where
  get = Z3m get
  set x = Z3m $ set x

instance ReaderM (Z3m ctx) (Z3Ctx ctx) where
  ask = Z3m ask

instance RunReaderM (Z3m ctx) (Z3Ctx ctx) where
  local ctx m = Z3m $ local ctx (runZ3m m)

instance BaseM (Z3m ctx) Z3.Z3 where
  inBase m = Z3m $ lift $ lift m

instance RunM Z3.Z3 a (Maybe Z3.Logic -> Z3Opts.Opts -> IO a) where
  runM m = \logic opts -> Z3.evalZ3With logic opts m

instance RunM (Z3m ctx) a (Z3Ctx ctx -> Z3Info ->
                           Maybe Z3.Logic -> Z3Opts.Opts ->
                           IO (a, Z3Info)) where
  runM m = runM $ runZ3m m

-- | Lift a 'Z3m' computation into a context with a single extra free variable
liftCtx1 :: Z3m ctx b -> Z3m (ctx :> a) b
liftCtx1 m =
  do ctx <- ask
     Z3m $ lift $ runReaderT (z3ctx_lower1 ctx) (runZ3m m)

-- | Add a free existential variable to the current context
inExtCtx1_Ex :: Z3Expr a -> Z3m (ctx :> a) b -> Z3m ctx b
inExtCtx1_Ex e m =
  do ctx <- ask
     Z3m $ lift $ runReaderT (Z3Ctx_EVar ctx e) (runZ3m m)

-- | Add a free universal variable to the current context
inExtCtx1_Univ :: Z3m (ctx :> a) b -> Z3m ctx b
inExtCtx1_Univ m =
  do ctx <- ask
     Z3m $ lift $ runReaderT (Z3Ctx_UVar ctx) (runZ3m m)


----------------------------------------------------------------------
-- Converting logical types to Z3 types and sorts
----------------------------------------------------------------------

data Z3Type a where
  Z3Type_lit :: Typeable a => Z3Type (Literal a)
  Z3Type_prop :: Z3Type Prop
  Z3Type_PM :: Z3Type a -> Z3Type (PM a)
  Z3Type_fun :: Z3Type a -> Z3Type b -> Z3Type (a -> b)

z3type2sort :: Z3Type a -> Z3m ctx Z3.Sort
z3type2sort z3tp = error "Implement z3type2sort!"


----------------------------------------------------------------------
-- Expression-building operations
----------------------------------------------------------------------

-- | Build an expression for a free variable
z3var :: Z3Type (Literal a) -> Closed (Mb ctx (Name (Literal a))) ->
         Z3m ctx (Z3Expr (Literal a))
z3var z3tp n =
  case clApply $(mkClosed [| mbNameBoundP |]) n of
    [clP| Left memb |] ->
      do ctx <- ask
         sort <- z3type2sort z3tp
         case z3ctx_lookup ctx (unClosed memb) of
           Left e -> return e
           Right i ->
             do ast <- inBase $ Z3.mkBound (fromInteger i) sort
                return (Z3Expr_lit ast)
    [clP| Right closed_n |] -> noClosedNames closed_n
