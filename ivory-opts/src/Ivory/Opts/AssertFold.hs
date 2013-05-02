{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Fold over expressions that collect up assertions about the expressions.

module Ivory.Opts.AssertFold where

import MonadLib (WriterT,put,runM,Id)
import qualified Data.DList as D
import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------

-- | Monad for accumulating expressions for asserting during evaluation.
newtype Assert a = Assert
  { unAssert :: WriterT (D.DList I.Expr) Id a }
  deriving (Functor, Monad)

-- | Takes a 'Maybe' expression e and if writes e into the 'Assert' monad.
putExpr :: Maybe I.Expr -> Assert ()
putExpr = maybe (return ()) (\e -> Assert (put $ D.singleton e))

--------------------------------------------------------------------------------

type ExpFold = I.Type -> I.Expr -> Assert ()

--------------------------------------------------------------------------------

procFold :: ExpFold -> I.Proc -> I.Proc
procFold ef p =
  let body' = stmtFold ef (D.fromList $ I.procBody p) in
  p { I.procBody = D.toList body' }

--------------------------------------------------------------------------------

stmtFold :: ExpFold -> D.DList I.Stmt -> D.DList I.Stmt
stmtFold ef ss
  -- DList is empty
  | D.foldr (\_ len -> len+1) 0 ss == (0 :: Integer) = D.empty
  | otherwise     =
  case stmt of
    I.IfTE e b0 b1 ->
      let asserts = collect I.TyBool e in
      (toAsserts asserts) `D.append`
        (I.IfTE e (toFoldBlck b0) (toFoldBlck b1)
          `D.cons` stmtFold ef stmts)
    I.Assert{}             -> go D.empty
    I.Assume{}             -> go D.empty
    I.Return (I.Typed t e) -> let asserts = collect t e in
                              go asserts
    I.ReturnVoid           -> go D.empty
    I.Deref t _ e          -> let asserts = collect t e in
                              go asserts
    I.Store t e0 e1        -> let asserts0 = collect t e0 in
                              let asserts1 = collect t e1 in
                              go (asserts0 `D.append` asserts1)
    I.Assign t _ e         -> let asserts = collect t e in
                              go asserts
    I.Call{}               -> go D.empty
    I.Loop t v e incr b    ->
      let asserts = collect t e in
      let asserts0 = fromIncr t incr in
      let assts = asserts `D.append` asserts0 in
      (I.Loop t v e incr (toFoldBlck b))
        `D.cons` ((toAsserts assts) `D.append` (stmtFold ef stmts))
    I.Break                -> go D.empty
    I.Local{}              -> go D.empty
    I.RefCopy t e0 e1      -> let asserts0 = collect t e0 in
                              let asserts1 = collect t e1 in
                              go (asserts0 `D.append` asserts1)
    I.AllocRef{}           -> go D.empty
    I.Forever b            -> I.Forever (toFoldBlck b) `D.cons`
                                stmtFold ef stmts

  where
  stmt  = D.head ss
  stmts = D.tail ss

  fromIncr t incr = case incr of
    I.IncrTo e0 -> cs e0
    I.DecrTo e0 -> cs e0
    where
    cs e0 = collect t e0

  toFoldBlck = D.toList . stmtFold ef . D.fromList
  go assts   = toAsserts assts `D.append` (stmt `D.cons` stmtFold ef stmts)
  toAsserts  = D.map I.Assert
  collect t  = snd . runM . unAssert . (ef t)
