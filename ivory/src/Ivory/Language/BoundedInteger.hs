{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Language.BoundedInteger where

import           Text.Printf

import           Ivory.Language.Proxy
import qualified Ivory.Language.Syntax as I
import           Ivory.Language.Type

--------------------------------------------------------------------------------

-- | It is an error if a constant implicitly underflows/overflows.
boundedFromInteger :: forall a b . (Num a, IvoryType a, Bounded b, Integral b)
                   => (I.Expr -> a) -> b -> Integer -> a
boundedFromInteger constr _ i
  | i > snd bounds
  = error $ printf "The constant %d is too large to cast to type %s." i tyStr
  | i < fst bounds
  = error $ printf "The constant %d is too small to cast to type %s." i tyStr
  | otherwise
  = constr (fromInteger i)
  where
  ty     = ivoryType (Proxy :: Proxy a)
  tyStr  = show ty
  bounds :: (Integer, Integer)
  bounds = (fromIntegral (minBound :: b), fromIntegral (maxBound :: b))
