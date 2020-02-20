{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

--
-- DefBitRep.hs --- Template Haskell utilities.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.Language.BitData.DefBitRep where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- | Define the type instance:
--
--   type instance <fname> <x> = <rname>
--
-- for each "n" in "xs".
--
-- Used to define the set of representation types for bit lengths.
defBitRep :: Name -> Name -> [Integer] -> Q [Dec]
defBitRep fname rname xs = mapM makeInstance xs
  where
    makeInstance n = do
      let nType = LitT (NumTyLit n)
      let rType = ConT rname
      tySynInstDCompat fname Nothing [pure nType] (pure rType)
