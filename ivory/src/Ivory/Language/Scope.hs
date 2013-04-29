{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Ivory.Language.Scope where

-- | Definition scopes for values.
data RefScope
  = Global            -- ^ Globally allocated
  | forall s. Stack s -- ^ Stack allocated

-- | Determine if it's legal to write values from one allocation scope into
-- another.
class Writable (ref :: RefScope) (val :: RefScope)

-- -- -- It's legal to write globally allocated values to globally
-- -- -- allocated references.
-- instance Writable Global Global

-- -- It's legal to write anything to a stack-allocated scope.
-- instance Writable (Stack s) (Stack s)

-- It's legal to write anything to a stack-allocated scope.
instance Writable (Stack s) Global

-- It's legal to write anything to a stack-allocated scope.
instance Writable s s
