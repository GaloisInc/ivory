{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Stdlib.Memory
  ( resultInto
  , into
  ) where

import Ivory.Language

-- | handy shorthand for transfering members
resultInto :: IvoryStore a =>
     Ivory eff a -> Ref s (Stored a) -> Ivory eff ()
resultInto a b = store b =<< a

into :: IvoryStore a =>
     Ref s (Stored a) -> Ref s' (Stored a) -> Ivory eff ()
into a b = store b =<< deref a

