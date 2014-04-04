{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Ivory QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.CSyntax.QQ   ( c ) where

import           Prelude hiding (exp, init)
import qualified Prelude as P

import Ivory.Language.CSyntax.QQ.ProcQQ

import           Language.Haskell.TH()
import           Language.Haskell.TH.Quote

import Ivory.Language.CSyntax.Parser

--------------------------------------------------------------------------------

-- | Quasiquoter for defining Ivory statements in C-like syntax.
c :: QuasiQuoter
c = QuasiQuoter
  { quoteExp  = err "quoteExp"
  , quotePat  = err "quotePat"
  , quoteDec  = decP
  , quoteType = err "quoteType"
  }
  where
  decP str = do
    procs <- ivoryCParser str
    cDefs <- mapM fromProc procs
    return (concat cDefs)
  err str = error $ str ++ " not implemented for c quasiquoter."

--------------------------------------------------------------------------------
-- Testing

{-
dump :: QuasiQuoter
dump = QuasiQuoter
  { quoteExp  = \str -> return $ LitE (StringL str)
  }
-}
