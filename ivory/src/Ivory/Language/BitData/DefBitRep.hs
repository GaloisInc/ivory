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

-- | Define the type instance:
--
--   type instance <fname> <x> = <rname>
--
-- for each "n" in "xs".
--
-- Used to define the set of representation types for bit lengths.
defBitRep :: Name -> Name -> [Integer] -> DecsQ
defBitRep fname rname xs = return $ map go xs
  where
#if __GLASGOW_HASKELL__ >= 700
  go n = TySynInstD fname (TySynEqn [LitT (NumTyLit n)] (ConT rname))
#else
  go n = TySynInstD fname [LitT (NumTyLit n)] (ConT rname)
#endif
