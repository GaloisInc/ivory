{-# LANGUAGE DeriveFunctor #-}

module Ivory.Artifact.Location
  ( Located(..)
  , mightBeEqLocatedArtifact
  ) where

import Ivory.Artifact
-- | In the build, we generally want to put artifacts in one
-- of three locations: somewhere relative to the root directory
-- of the build output, somewhere relative to the sources, or somewhere
-- relative to the includes (headers).

data Located a
  = Root a
  | Src a
  | Incl a
  deriving (Show, Eq, Functor)

mightBeEqLocatedArtifact :: Located Artifact -> Located Artifact -> Bool
mightBeEqLocatedArtifact (Root a) (Root b) = mightBeEqArtifact a b
mightBeEqLocatedArtifact (Src a) (Src b) = mightBeEqArtifact a b
mightBeEqLocatedArtifact (Incl a) (Incl b) = mightBeEqArtifact a b
mightBeEqLocatedArtifact _ _ = False
