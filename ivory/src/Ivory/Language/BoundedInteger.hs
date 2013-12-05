{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Language.BoundedInteger where

import Text.Printf

import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

--------------------------------------------------------------------------------

-- | It is an error if a constant implicitly underflows/overflows.
boundedFromInteger :: forall a b . (Num a, IvoryType a, Bounded b, Integral b)
                   => (I.Expr -> a) -> b -> Integer -> a
boundedFromInteger constr _ i =
  if i > snd bounds
    then error $ printf "The constant %d is too large to cast to type %s." i tyStr
    else if i < fst bounds
      then error $ printf "The constant %d is too small to cast to type %s." i tyStr
         else constr (fromInteger i)
  where
  ty     = ivoryType (Proxy :: Proxy a)
  tyStr  = show ty
  bounds = (fromIntegral (minBound :: b), fromIntegral (maxBound :: b))
