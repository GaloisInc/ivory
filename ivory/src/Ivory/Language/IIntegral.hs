module Ivory.Language.IIntegral where

import Ivory.Language.Sint
import Ivory.Language.Type
import Ivory.Language.IBool
import Ivory.Language.Uint
import qualified Ivory.Language.Syntax.AST as I

--------------------------------------------------------------------------------

-- | Integral, without the baggage from Haskell (i.e., supertypes of 'Real' and
-- 'Enum'). Defines Euclidian division (rather than truncated division). See
-- http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.74.8522. Notable
-- properties, beyond the normal div/rem properties, are that
--
-- The remainder is always non-negative.
-- For d1 < 0, @d0 `iDiv` (-d1) == -(d0 `iDiv` d1)@.
-- For d1 < 0, @d0 .% (-d1) == d0 .% d1@.
--
-- N.B. `mod` and `rem` are equal if both args are positive, and C has no `mod`
-- operator (only `rem`). In Haskell and C, both `mod` and `rem` may return
-- negative values. Furthermore, before C99, the result of `rem` is
-- implementation-defined.
class (IvoryExpr a, IvoryOrd a , Num a) => IvoryIntegral a where

  -- | Euclidean division.
  iDiv :: a -> a -> a
  iDiv d0 d1 = (r <? 0) ? ((d1 >? 0) ? (q-1,q+1),q)
    where
    q = wrapExpr (I.ExpOp I.ExpDiv [unwrapExpr d0, unwrapExpr d1])
    r = iRem d0 d1

  -- | Euclidean remainder.
  (.%) :: a -> a -> a
  d0 .% d1 = (r <? 0) ? ((d1 >? 0) ? (r+d1,r-d1),r)
    where
    r = iRem d0 d1

-- XXX Dont' export
iRem :: IvoryIntegral a => a -> a -> a
iRem d0 d1 = wrapExpr (I.ExpOp I.ExpMod [unwrapExpr d0, unwrapExpr d1])

--------------------------------------------------------------------------------

instance IvoryIntegral Sint8
instance IvoryIntegral Sint16
instance IvoryIntegral Sint32
instance IvoryIntegral Sint64
instance IvoryIntegral Uint8
instance IvoryIntegral Uint16
instance IvoryIntegral Uint32
instance IvoryIntegral Uint64

--------------------------------------------------------------------------------
