{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Language.Cond where

import Ivory.Language.Area
import Ivory.Language.IBool
import Ivory.Language.Monad
import Ivory.Language.Proc
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I
import Data.Monoid(Monoid(..))

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

checkStored :: forall ref s a.
           (IvoryVar a, IvoryRef ref, IvoryVar (ref s (Stored a)))
        => ref s (Stored a) -> (a -> Cond) -> Cond
checkStored ref prop = Cond $ do
  n <- freshVar "pre"
  let ty = ivoryType (Proxy :: Proxy a)
  b <- runCond $ prop $ wrapVar n
  return (I.CondDeref ty (unwrapExpr ref) n b)

-- Pre-Conditions --------------------------------------------------------------

-- | Proc bodies that have pre-conditions.  Multiple pre-conditions may be
-- provided, for which the conjunction must hold.
class Requires c where
  requires :: IvoryType r => c -> Body r -> Body r

-- XXX Do not export
requires' :: (Requires c, IvoryType r) => (c -> Cond) -> c -> Body r -> Body r
requires' chk prop b = Body $ do
  req <- runCond $ chk $ prop
  emitPreCond (I.Require req)
  runBody b

instance Requires IBool where
  requires = requires' check

instance Requires Cond where
  requires = requires' id

-- Post-Conditions -------------------------------------------------------------

-- | Proc bodies that have post-conditions.  Multiple post-conditions may be
-- provided, for which the conjunction must hold.
class Ensures c where
  ensures :: IvoryVar r => (r -> c) -> Body r -> Body r

-- XXX Do not export
ensures' :: (Ensures c, IvoryVar r)
  => (c -> Cond) -> (r -> c) -> Body r -> Body r
ensures' chk prop b = Body $ do
  c <- runCond $ chk $ prop $ wrapVar I.retval
  emitPostCond (I.Ensure c)
  runBody b

instance Ensures IBool where
  ensures = ensures' check

instance Ensures Cond where
  ensures = ensures' id
