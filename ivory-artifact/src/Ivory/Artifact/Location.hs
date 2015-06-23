{-# LANGUAGE DeriveFunctor #-}

module Ivory.Artifact.Location
  ( Located(..)
  ) where

-- | In the build, we generally want to put artifacts in one
-- of three locations: somewhere relative to the root directory
-- of the build output, somewhere relative to the sources, or somewhere
-- relative to the includes (headers).

data Located a
  = Root a
  | Src a
  | Incl a
  deriving (Show, Eq, Functor)

