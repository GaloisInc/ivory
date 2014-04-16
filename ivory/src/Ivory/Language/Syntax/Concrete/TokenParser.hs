{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Ivory.Language.Syntax.Concrete.TokenParser where

import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as T

-- Use Haskell tokens.
T.TokenParser{..} = T.makeTokenParser haskellDef
