{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ivory.Stdlib.Memory
  ( resultInto
  , into
  , arrCopy
  ) where

import Ivory.Language

-- | handy shorthand for transfering members
resultInto :: IvoryStore a =>
     Ivory eff a -> Ref s (Stored a) -> Ivory eff ()
resultInto a b = store b =<< a

into :: IvoryStore a =>
     Ref s (Stored a) -> Ref s' (Stored a) -> Ivory eff ()
into a b = store b =<< deref a

-- XXX Belongs with Pack.hs and SafePack.hs.

-- | Copy from array @from@ (either a 'Ref' or a 'ConstRef') into array @to@
-- starting at index @start@.  Copying continues until either the from array is
-- fully copied or the to array is full.  The start value being negative is
-- considered an error.
arrCopy :: forall n m r s0 s1 s2 eff t.
           ( SingI n, SingI m, IvoryRef r
           , IvoryExpr (r s2 (Array m (Stored t)))
           , IvoryExpr (r s2 (Stored t))
           , IvoryStore t
           , GetAlloc eff ~ Scope s0
           )
        => Ref s1 (Array n (Stored t))
        -> r s2 (Array m (Stored t))
        -> Sint32
        -> Ivory eff ()
arrCopy to from start =
  ifte_ (start <? 0)
        (assert false)
        (arrayMap $ go)
  where
  mkIx :: Ix m -> Ix n
  mkIx ix = toIx (start + fromIx ix)

  go ix =
    ifte_
      (fromIx ix + start >=? arrayLen to)
      -- The from array is too big, so stop copying.
      (return ()) -- XXX could be break but type constraints get hairy
      -- We can copy the whole from array.
      (deref (from ! ix) >>= store (to ! mkIx ix))
