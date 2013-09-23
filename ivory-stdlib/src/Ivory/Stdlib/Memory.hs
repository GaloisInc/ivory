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

-- XXX Belongs with Pack.hs and SafePack.hs.

-- | Copy from array @from@ (either a 'Ref' or a 'ConstRef') into array @to@
-- starting at index @start@.  If there is not room to copy the entire from
-- array, then no copying takes place.  The start value being negative is
-- considered an error.  The length of the @from@ array is returned if the
-- copying was successful and 0 otherwise.
arrCopy :: ( SingI n, SingI m, IvoryRef r
           , IvoryExpr (r s2 (Array m (Stored t)))
           , IvoryExpr (r s2 (Stored t))
           , IvoryStore t, GetAlloc eff ~ Scope s0
           )
        => Ref s1 (Array n (Stored t))
        -> r s2 (Array m (Stored t))
        -> Sint32
        -> Ivory eff Sint32
arrCopy to from start = do
  b <- local (ival $ arrayLen from)
  cond_ [    start <? 0
        ==> assert false
        ,   arrayLen to - start >=? arrayLen from
        ==> do arrayMap $ \ix ->
                 deref (from ! ix) >>= store (to ! mkIx ix)
               b %= const 0
        ]
  return =<< deref b
  where
  mkIx ix = toIx (start + fromIx ix)
