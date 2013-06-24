{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Language.Scope where

-- | Definition scopes for values.
data RefScope
  = Global            -- ^ Globally allocated
  | forall s. Stack s -- ^ Stack allocated
