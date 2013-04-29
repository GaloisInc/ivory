{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Language.Loop where

import Ivory.Language.Array
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as AST

import GHC.TypeLits


-- | Break out of a loop.
breakOut :: Ivory eff ()
breakOut  = emit AST.Break

-- XXX don't export.
loop :: forall eff n a. (SingI n)
     => AST.Expr -> AST.LoopIncr -> (Ix n -> Ivory eff a) -> Ivory eff ()
loop from to body = do
  ix        <- freshVar "ix"
  let ixVar = wrapExpr (AST.ExpVar ix)
      ty    = ivoryType (Proxy :: Proxy IxRep)
  (_,block) <- collect (body ixVar)
  emit (AST.Loop ty ix from to (blockStmts block))

upTo :: SingI n => Ix n -> Ix n -> (Ix n -> Ivory eff a) -> Ivory eff ()
upTo from to = loop (unwrapExpr from) (AST.IncrTo (unwrapExpr to))

downTo :: SingI n => Ix n -> Ix n -> (Ix n -> Ivory eff a) -> Ivory eff ()
downTo from to = loop (unwrapExpr from) (AST.DecrTo (unwrapExpr to))

for :: forall eff n a. SingI n => Ix n -> (Ix n -> Ivory eff a) -> Ivory eff ()
for n = upTo 0 (n-1)

times :: forall eff n a. SingI n
      => Ix n -> (Ix n -> Ivory eff a) -> Ivory eff ()
times n = downTo (n-1) 0

arrayMap :: forall eff n a . SingI n => (Ix n -> Ivory eff a) -> Ivory eff ()
arrayMap = upTo 0 hi
  where
  hi = fromInteger ((fromTypeNat (sing :: Sing n)) - 1)

forever :: Ivory eff () -> Ivory eff ()
forever body = do
  (_, block) <- collect body
  emit (AST.Forever (blockStmts block))

