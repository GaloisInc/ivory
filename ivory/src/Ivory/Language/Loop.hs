{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Language.Loop where

import Ivory.Language.IIntegral
import Ivory.Language.IBool
import Ivory.Language.Assert
import Ivory.Language.Array
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Effects as E
import qualified Ivory.Language.Syntax as AST

--------------------------------------------------------------------------------

breakOut :: (E.GetBreaks eff ~ E.Break) => Ivory eff ()
breakOut = emit AST.Break

-- XXX don't export.
loop :: forall eff n a. (ANat n)
     => (AST.Expr -> AST.LoopIncr)
     -> Ix n
     -> Ix n
     -> (Ix n -> Ivory (E.AllowBreak eff) a)
     -> Ivory eff ()
loop incr fromIdx toIdx body = do
  let maxSz :: IxRep
      maxSz = fromInteger $ ixSize (undefined :: Ix n)
  let trans v = unwrapExpr $ wrapExpr v .% maxSz
  let from  = rawIxVal fromIdx
  let to    = rawIxVal toIdx
  ix        <- freshVar "ix"
  let ixVar = wrapExpr (AST.ExpVar ix)
  (_,block) <- collect (body ixVar)
  -- XXX TODO: are these still needed??
  let asst v = compilerAssert (wrapExpr v <? maxSz .&& (-1::IxRep) <=? wrapExpr v)
  asst from
  asst to
  emit (AST.Loop ix (trans from) (incr $ trans to) (blockStmts block))

upTo :: ANat n
     => Ix n -> Ix n -> (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
upTo = loop AST.IncrTo

downTo :: ANat n
       => Ix n -> Ix n -> (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
downTo = loop AST.DecrTo

-- | Run the computation n times, where
-- @
--   n :: Ix m, 0 <= n <= m.
-- @
-- Indexes increment from 0 to n-1 incluseively.
for :: forall eff n a. ANat n
    => Ix n -> (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
for n f = upTo 0 (n-1) f

-- | Run the computation n times, where
-- @
--   n :: Ix m, 0 <= n <= m.
-- @
-- Indexes decrement from n-1 to 0 inclusively.
times :: forall eff n a. ANat n
      => Ix n -> (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
times n f = downTo (n-1) 0 f

arrayMap :: forall eff n a . ANat n
         => (Ix n -> Ivory (E.AllowBreak eff) a) -> Ivory eff ()
arrayMap = upTo 0 hi
  where
  hi = fromInteger ((fromTypeNat (aNat :: NatType n)) - 1)

forever :: Ivory (E.AllowBreak eff) () -> Ivory eff ()
forever body = do
  (_, block) <- collect (body)
  emit (AST.Forever (blockStmts block))
