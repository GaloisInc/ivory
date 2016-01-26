{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Stdlib.Memory
  ( resultInto
  , into
  , arrayCopy
  ) where

import Ivory.Language
import Ivory.Stdlib.Control

-- | handy shorthand for transfering members
resultInto :: IvoryStore a =>
     Ivory eff a -> Ref s ('Stored a) -> Ivory eff ()
resultInto a b = store b =<< a

into :: IvoryStore a =>
     Ref s ('Stored a) -> Ref s' ('Stored a) -> Ivory eff ()
into a b = store b =<< deref a

-- XXX Belongs with Pack.hs and SafePack.hs.

-- | Copies a prefix of an array into a postfix of another array.  That is, copy
-- from array @from@ (either a 'Ref' or a 'ConstRef') into array @to@ starting
-- at index @toOffset@ in @to@.  Copying continues until either the from array
-- is fully copied, the @to@ array is full, or index @end@ in the @from@ array
-- is reached (index @end@ is not copied).  to copy the full @from@ array, let
-- @end@ equal 'arrayLen from'.
arrayCopy ::
           ( ANat n, ANat m, IvoryRef r
           , IvoryExpr (r s2 ('Array m ('Stored t)))
           , IvoryExpr (r s2 ('Stored t))
           , IvoryStore t
           )
        => Ref s1 ('Array n ('Stored t))
        -> r s2 ('Array m ('Stored t))
        -> Sint32
        -> Sint32
        -> Ivory eff ()
arrayCopy to from toOffset end = do
  assert (toOffset >=? 0 .&& toOffset <? toLen)
  assert (end      >=? 0 .&& end     <=? frLen)
  arrayMap $ go
  where
  -- The index is w.r.t. the from array.
  go ix =
    cond_
      [   -- We've reached the @end@ index: stop copying.
          (fromIx ix >=? end)
      ==> return ()
      ,   -- We've reached the end of the @to@ array: stop copying.
          (fromIx ix + toOffset >=? toLen)
      ==> return ()
      ,   true
      ==> (deref (from ! ix) >>= store (to ! mkIx ix))
      ]

  toLen = arrayLen to
  frLen = arrayLen from

  mkIx ix = toIx (toOffset + fromIx ix)

