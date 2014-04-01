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

import Ivory.Language.CSyntax.QQ.StmtQQ

import           Language.Haskell.TH()
import           Language.Haskell.TH.Quote

import Ivory.Language.CSyntax.Parser

import qualified Ivory.Language.Syntax.Type as I

import Ivory.Language.CSyntax.ParseAST

--------------------------------------------------------------------------------

-- | Quasiquoter for defining Ivory statements in C-like syntax.
c :: QuasiQuoter
c = QuasiQuoter
  { quoteExp  = \str -> ivoryCParser str >>= fromProgram
  , quotePat  = err "quotePat"
  , quoteDec  = err "quotePat"
  , quoteType = err "quoteType"
  }
  where
  err str = error $ str ++ " not implemented for c quasiquoter."

--------------------------------------------------------------------------------
-- Types

fromIntSz :: IntSize -> I.IntSize
fromIntSz sz = case sz of
  Int8  -> I.Int8
  Int16 -> I.Int16
  Int32 -> I.Int32
  Int64 -> I.Int64

fromWordSz :: WordSize -> I.WordSize
fromWordSz sz = case sz of
  Word8  -> I.Word8
  Word16 -> I.Word16
  Word32 -> I.Word32
  Word64 -> I.Word64

-- fromMemArea :: 

-- fromType :: Type -> Q T.Exp
-- fromType ty = lift $ case ty of
--   TyVoid       -> I.TyVoid
--   TyInt sz     -> I.TyInt (fromIntSz sz)
--   TyWord sz    -> I.TyWord (fromWordSz sz)
--   TyBool       -> I.TyBool
--   TyChar       -> I.TyChar
--   TyFloat      -> I.TyFloat
--   TyDouble     -> I.TyDouble
--   TyRef        -> 

--------------------------------------------------------------------------------
-- Testing

{-
dump :: QuasiQuoter
dump = QuasiQuoter
  { quoteExp  = \str -> return $ LitE (StringL str)
  }
-}
