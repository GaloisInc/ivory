{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Language.Loop (
    breakOut,
    upTo,
    downTo,
    for,
    times,
    arrayMap,
    forever
  ) where

import Ivory.Language.IBool
import Ivory.Language.Assert
import Ivory.Language.Array
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Effects as E
import qualified Ivory.Language.Syntax as AST

--------------------------------------------------------------------------------

breakOut :: (E.GetBreaks eff ~ 'E.Break) => Ivory eff ()
breakOut = emit AST.Break

-- XXX don't export.
loop :: forall eff n a. (ANat n)
     => (AST.Expr -> AST.LoopIncr)
     -> IxRep
     -> IxRep
     -> (Ix n -> Ivory (E.AllowBreak eff) a)
     -> Ivory eff ()
loop incr from to body = do
  let maxVal = ixSize (undefined :: Ix n)

  let maxSz :: IxRep
      maxSz = fromInteger maxVal
  ix        <- freshVar "ix"
  let ixVar = wrapExpr (AST.ExpVar ix)
  (_,block) <- collect (body ixVar)
  let asst v = compilerAssert (v <? maxSz .&& 0 <=? v)
  asst from
  asst to
  emit (AST.Loop maxVal ix (unwrapExpr from) (incr $ unwrapExpr to) (blockStmts block))


-- | Loop over the range of indexes @[start, start + 1 .. end]@. If
-- @start > end@, the loop body will never execute.
upTo :: ANat n
     => Ix n -> Ix n -> (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
upTo from to = loop AST.IncrTo (fromIx from) (fromIx to)

-- | Loop over the range of indexes @[end, end - 1 .. start]@. If
-- @end > start@, the loop body will never execute.
downTo :: ANat n
       => Ix n -> Ix n -> (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
downTo from to = loop AST.DecrTo (fromIx from) (fromIx to)

-- | Run the computation n times, where
-- @
--   n :: Ix m, 0 <= n <= m.
-- @
-- Indexes increment from 0 to n-1 inclusively.
--
-- Note: The definition of `for` may change so that no index value is
-- provided. (Consider using `upto` if you need to use the index.)
for :: forall eff n a. ANat n
    => Ix n -> (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
for n f = loop AST.IncrTo 0 (fromIx n - 1) f

-- | Run the computation n times, where
-- @
--   n :: Ix m, 0 <= n <= m.
-- @
-- Indexes decrement from n-1 to 0 inclusively.
--
-- Note: The definition of `times` may change so that no index value is
-- provided. (Consider using `downTo` if you need to use the index.)
times :: forall eff n a. ANat n
      => Ix n -> (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
times n = loop AST.DecrTo (fromIx n - 1) 0

arrayMap :: forall eff n a . ANat n
         => (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
arrayMap = loop AST.IncrTo 0 (fromIntegral (fromTypeNat (aNat :: NatType n)) - 1)

forever :: Ivory (E.AllowBreak eff) () -> Ivory eff ()
forever body = do
  (_, block) <- collect (body)
  emit (AST.Forever (blockStmts block))
