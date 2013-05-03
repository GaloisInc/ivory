{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--
-- TokenParser.hs --- HW quasiquoter token parser.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BitData.TokenParser where

import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as T

-- Import all the token parser definitions for our language.
T.TokenParser{..} = T.makeTokenParser haskellDef
