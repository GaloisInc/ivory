--------------------------------------------------------------------------------
-- | Division by zero checks.
--------------------------------------------------------------------------------

module Ivory.Opts.DivZero
  ( divZeroFold
  ) where

import Ivory.Opts.AssertFold

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------

divZeroFold :: I.Proc -> I.Proc
divZeroFold = procFold (expFoldDefault divAssert)

--------------------------------------------------------------------------------

-- Claim that the divisor expression cannnot equal zero.  If we don't have a
-- division-causing expression, return Nothing.
divAssert :: Asserter
divAssert ty e0 = case e0 of
  I.ExpOp op args ->
    case (op,args) of
      (I.ExpDiv,[_,r])       -> ma r
      (I.ExpMod,[_,r])       -> ma r
      (I.ExpRecip,[e])       -> ma e
      (I.ExpFLog,[e])        -> ma e
      (I.ExpFLogBase,[_,r])  -> ma r
      _                      -> Nothing
  _               -> Nothing

  where
  ma x = return $ I.ExpOp (I.ExpNeq ty) [x,zeroExp]
  zeroExp = case ty of
              I.TyInt  _ -> I.ExpLit (I.LitInteger 0)
              I.TyWord _ -> I.ExpLit (I.LitInteger 0)
              I.TyFloat  -> I.ExpLit (I.LitFloat 0)
              I.TyDouble -> I.ExpLit (I.LitDouble 0)
              _          -> error $
                "ivory opts: unrecognized expression in Div by zero checking."

--------------------------------------------------------------------------------
