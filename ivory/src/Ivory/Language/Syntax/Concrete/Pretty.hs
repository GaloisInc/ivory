--
-- Pretty-printing for concrete syntax.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.Pretty where

import Text.PrettyPrint

--------------------------------------------------------------------------------

class Pretty a where
  pretty :: a -> Doc

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing  = empty
  pretty (Just a) = pretty a

instance Pretty a => Pretty [a] where
  pretty = hcat . map pretty

instance Pretty Char where
  pretty = char

instance Pretty Int where
  pretty = int

defaultStyle :: Style
defaultStyle = style { lineLength = 40 }

prettyPrint :: Doc -> String
prettyPrint = renderStyle defaultStyle
