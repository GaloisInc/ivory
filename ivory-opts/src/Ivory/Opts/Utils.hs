--------------------------------------------------------------------------------
-- | Utilities.
--------------------------------------------------------------------------------

module Ivory.Opts.Utils

where

import qualified Ivory.Language.Syntax.AST as I
import qualified Ivory.Language.Syntax.Type as I

--------------------------------------------------------------------------------

-- | Type of the expression's arguments.
expOpType :: I.Type -> I.ExpOp -> I.Type
expOpType t0 op = case op of
  I.ExpEq        t1 -> t1
  I.ExpNeq       t1 -> t1
  I.ExpGt _      t1 -> t1
  I.ExpLt _      t1 -> t1
  I.ExpIsNan     t1 -> t1
  I.ExpIsInf     t1 -> t1
  I.ExpToFloat   t1 -> t1
  I.ExpFromFloat t1 -> t1
  _                 -> t0

--------------------------------------------------------------------------------


