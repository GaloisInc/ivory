{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Stdlib.Memory
  ( resultInto
  , into
  , arrCopy
  ) where

import Ivory.Language
import Ivory.Stdlib.Control
import Ivory.Stdlib.Operators

-- | handy shorthand for transfering members
resultInto :: IvoryStore a =>
     Ivory eff a -> Ref s (Stored a) -> Ivory eff ()
resultInto a b = store b =<< a

into :: IvoryStore a =>
     Ref s (Stored a) -> Ref s' (Stored a) -> Ivory eff ()
into a b = store b =<< deref a

-- | Copy from array @from@ into array @to@ starting at index @start@.  If there
-- is not room to copy the entire from array, then no copying takes place.  The
-- start value being negative is considered an error.
arrCopy :: (SingI n, SingI m, IvoryStore t, GetAlloc eff ~ Scope s0)
        => Ref s1 (Array n (Stored t))
        -> Ref s2 (Array m (Stored t))
        -> Sint32
        -> Ivory eff IBool
arrCopy to from start = do
  b <- local (ival false)
  cond_ [    start <? 0
        ==> assert false
        ,   arrayLen to - start >=? arrayLen from
        ==> do arrayMap $ \ix ->
                 deref (from ! ix) >>= store (to ! mkIx ix)
               b %= const true
        ]
  return =<< deref b
  where
  mkIx ix = toIx (start + fromIx ix)
