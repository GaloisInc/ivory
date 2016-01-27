{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Language.Cond where

import Prelude ()
import Prelude.Compat

import Ivory.Language.Area
import Ivory.Language.IBool
import Ivory.Language.Monad
import Ivory.Language.Proc
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I


-- Effects ---------------------------------------------------------------------

-- | Emit a pre-condition.
--
-- XXX do not export
emitPreCond :: I.Require -> Ivory eff ()
emitPreCond r = emits mempty { blockRequires = [r] }

-- | Emit a post-condition.
--
-- XXX do not export
emitPostCond :: I.Ensure -> Ivory eff ()
emitPostCond e = emits mempty { blockEnsures = [e] }

-- Condition Notation ----------------------------------------------------------

newtype Cond = Cond
  { runCond :: forall eff. Ivory eff I.Cond
    -- ^ Use the naming environment from the Ivory monad.
  }

-- | Checkable a boolean expression.
check :: IBool -> Cond
check bool = Cond (return (I.CondBool (unwrapExpr bool)))

checkStored' :: forall ref s a c.
     ( CheckStored c
     , IvoryVar a
     , IvoryRef ref
     , IvoryVar (ref s ('Stored a))
     ) => (c -> Cond) -> ref s ('Stored a) -> (a -> c) -> Cond
checkStored' c ref prop = Cond $ do
  n <- freshVar "pre"
  let ty = ivoryType (Proxy :: Proxy a)
  b <- runCond $ c $ prop $ wrapVar n
  return (I.CondDeref ty (unwrapExpr ref) n b)

class CheckStored c where
  checkStored :: (IvoryVar a, IvoryRef ref, IvoryVar (ref s ('Stored a)))
              => ref s ('Stored a) -> (a -> c) -> Cond

instance CheckStored IBool where
  checkStored = checkStored' check

instance CheckStored Cond where
  checkStored = checkStored' id

-- Pre-Conditions --------------------------------------------------------------

-- | Proc bodies that have pre-conditions.  Multiple pre-conditions may be
-- provided, for which the conjunction must hold.
class Requires c where
  requires :: (WrapIvory m, IvoryType r) => c -> m r -> m r

-- XXX Do not export
requires' :: (WrapIvory m, Requires c, IvoryType r)
  => (c -> Cond) -> c -> m r -> m r
requires' chk prop b = wrap $ do
  req <- runCond $ chk $ prop
  emitPreCond (I.Require req)
  unwrap b

instance Requires IBool where
  requires = requires' check

instance Requires Cond where
  requires = requires' id

-- Post-Conditions -------------------------------------------------------------

-- | Proc bodies that have post-conditions.  Multiple post-conditions may be
-- provided, for which the conjunction must hold.
class Ensures c where
  ensures  :: (WrapIvory m, IvoryVar r) => (r -> c) -> m r -> m r
  ensures_ :: (WrapIvory m) => c -> m () -> m ()

-- XXX Do not export
ensures' :: (WrapIvory m, Ensures c, IvoryVar r)
  => (c -> Cond) -> (r -> c) -> m r -> m r
ensures' chk prop b = wrap $ do
  c <- runCond $ chk $ prop $ wrapVar I.retval
  emitPostCond (I.Ensure c)
  unwrap b

-- XXX Do not export
ensures_' :: (WrapIvory m, Ensures c)
  => (c -> Cond) -> c -> m () -> m ()
ensures_' chk prop b = wrap $ do
  c <- runCond $ chk $ prop
  emitPostCond (I.Ensure c)
  unwrap b

instance Ensures IBool where
  ensures = ensures' check
  ensures_ = ensures_' check

instance Ensures Cond where
  ensures = ensures' id
  ensures_ = ensures_' id

