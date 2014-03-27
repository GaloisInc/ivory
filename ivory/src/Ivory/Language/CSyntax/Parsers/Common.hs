--
-- Common infrastructure for parsers.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.Parsers.Common where

import Text.Parsec

--------------------------------------------------------------------------------

-- | Type of Ivory parsers.
type P s = Parsec String () s

--------------------------------------------------------------------------------

noParse :: String -> String
noParse str = "<no valid parse: " ++ str ++ ">"

--------------------------------------------------------------------------------
