--------------------------------------------------------------------------------
-- | Utilities.
--------------------------------------------------------------------------------

module Ivory.Opts.Utils

where

import           Text.PrettyPrint

import qualified Ivory.Language.Syntax.AST               as I
import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.Pretty   (pretty, prettyPrint)
import qualified Ivory.Language.Syntax.Type              as I

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
  _                 -> t0

--------------------------------------------------------------------------------
-- PrettyPrinting

mkOut :: String -> String -> (a -> Doc) -> [a] -> String
mkOut _   _    _  [] = ""
mkOut sym kind sh ls = render (nm <+> vcat (map go ls))
  where
  go x = nest 3 (text kind <> colon <+> sh x)
  nm   = text "***" <+> text "procedure/area" <+> text sym

showWithLoc :: (a -> Doc) -> Located a -> Doc
showWithLoc sh (Located loc a) =
  text (prettyPrint (pretty loc)) <> colon <+> sh a
