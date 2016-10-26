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

-- Results for a symbol, with the symbol name.
data SymResult a = SymResult String [a]
  deriving (Show, Read, Eq)

-- Results for a module, with the module name.
data ModResult a = ModResult String [SymResult a]
  deriving (Show, Read, Eq)

-- Show the errors for a module.
showModErrs :: Show a => (a -> Doc) -> ModResult a -> IO ()
showModErrs doc (ModResult m errs) =
  case errs of
    [] -> return ()
    _  ->
         putStrLn
       $ render
       $ text "***" <+> text "Module" <+> text m <> colon
      $$ nest 2 (vcat (map (showSymErrs doc) errs))
      $$ empty

-- Show the errors for a symbol (area or procedure).
showSymErrs :: (a -> Doc) -> SymResult a -> Doc
showSymErrs doc (SymResult sym errs) =
  case errs of
    [] -> empty
    _  ->
         text "***" <+> text "Symbol" <+> text sym <> colon
      $$ nest 2 (vcat (map doc errs))
      $$ empty

showWithLoc :: (a -> Doc) -> Located a -> Doc
showWithLoc sh (Located loc a) =
  case loc of
    NoLoc -> sh a
    _     -> text (prettyPrint (pretty loc)) <> colon <+> sh a
