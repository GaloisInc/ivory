{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Monad for generating random C inputs for Ivory programs.

module Ivory.QuickCheck.Monad
  ( IvoryGen()
  , set
  , get
  , run
  , runIO
  ) where

import           MonadLib hiding (set, get, lift)
import qualified MonadLib               as M
import           Control.Applicative (Applicative(..))

import qualified System.Random          as R

--------------------------------------------------------------------------------

newtype IvoryGen a = IvoryGen (StateT R.StdGen Id a)
  deriving (Functor, Applicative, Monad)

set :: R.StdGen -> IvoryGen ()
set = IvoryGen . M.set

-- | Get the current random number, split it, use one and put back the other.
get :: IvoryGen R.StdGen
get = do
  rnd <- IvoryGen M.get
  let (r0, r1) = R.split rnd
  set r1
  return r0

run :: R.StdGen -> IvoryGen a -> (a, R.StdGen)
run rnd (IvoryGen st) = runId (runStateT rnd st)

-- | Generate a fresh random value, run the monad with it, and return the
-- result.
runIO :: IvoryGen a -> IO a
runIO igen = do
  rnd <- R.newStdGen
  let (a,_) = run rnd igen
  return a

--------------------------------------------------------------------------------
