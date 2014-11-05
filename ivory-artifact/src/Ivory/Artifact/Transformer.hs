
module Ivory.Artifact.Transformer
  ( Transformer()
  , emptyTransformer
  , transform
  , transformErr
  , runTransformer
  ) where

newtype Transformer a =
  Transformer
    { unTransformer :: a -> Either String a
    }

emptyTransformer :: Transformer a
emptyTransformer  = Transformer Right

transform :: (a -> a) -> Transformer a -> Transformer a
transform f (Transformer t) = Transformer $ \a -> do
  a' <- t a
  return (f a')

transformErr :: (a -> Either String a) -> Transformer a -> Transformer a
transformErr f (Transformer t) = Transformer $ \a -> do
  a' <- t a
  f a'

runTransformer :: Transformer a -> a -> Either String a
runTransformer  = unTransformer
