{-# LANGUAGE RecordWildCards #-}

module Ivory.Language.CSyntax.TokenParser where

import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as T

-- Use Haskell tokens.
T.TokenParser{..} = T.makeTokenParser haskellDef
