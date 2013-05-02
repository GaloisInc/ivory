{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

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

import Control.Monad ((<=<))

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
emitPostCond e = emits mempty { blockEnsure = Just e }

-- Conditional Notation --------------------------------------------------------

newtype Cond = Cond
  { runCond :: forall eff. Ivory eff I.Cond
    -- ^ Use the naming environment from the Ivory monad.
  }

-- | Predicates over a referenced value.
satisfy :: forall ref s a.
           (IvoryVar a, IvoryRef ref, IvoryVar (ref s (Stored a)))
        => ref s (Stored a) -> (a -> Cond) -> Cond
satisfy ref k = Cond $ do
  n <- freshVar "pre"
  let ty = ivoryType (Proxy :: Proxy a)
  b <- runCond (k (wrapVar n))
  return (I.CondDeref ty (unwrapExpr ref) n b)

-- | Check a boolean expression.
check :: IBool -> Cond
check bool = Cond (return (I.CondBool (unwrapExpr bool)))


-- Pre-Conditions --------------------------------------------------------------

-- | Proc bodies that have pre-conditions.
requires :: IvoryType r => [Cond] -> Body r -> Body r
requires rs b = Body $ do
  mapM_ (emitPreCond . I.Require <=< runCond) rs
  runBody b


-- Post-Conditions -------------------------------------------------------------

-- | Proc bodies that have post-conditions.  This function will override any
-- previously set post-condition, as each function is only allowed to have one.
-- This is a bit of a hack, and it would be nice to come up with a way to have
-- it prevent that statically.
ensures :: IvoryVar r => (r -> Cond) -> Body r -> Body r
ensures p m = Body $ do
  c <- runCond (p (wrapVar I.retval))
  emitPostCond (I.Ensure c)
  runBody m
