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

module Ivory.Language.Syntax.Concrete.QQ   ( ivory ) where

import           Prelude hiding (exp, init)
import qualified Prelude as P
import           Data.Char

import qualified Language.Haskell.TH       as Q
import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import           Language.Haskell.TH.Quote

import qualified Ivory.Language as I

import Ivory.Language.Syntax.Concrete.QQ.StructQQ
import Ivory.Language.Syntax.Concrete.QQ.ProcQQ

import Ivory.Language.Syntax.Concrete.ParseAST
import Ivory.Language.Syntax.Concrete.Parser

--------------------------------------------------------------------------------

-- | Quasiquoter for defining Ivory statements in C-like syntax.
ivory :: QuasiQuoter
ivory = QuasiQuoter
  { quoteExp  = err "quoteExp"
  , quotePat  = err "quotePat"
  , quoteDec  = decP
  , quoteType = err "quoteType"
  }
  where
  decP str = do
    let defs      = reverse (runParser str)
    decs         <- mapM mkDef defs
    theModule    <- ivoryMod defs
    return (concat decs ++ theModule)
  err str = error $ str ++ " not implemented for c quasiquoter."

--------------------------------------------------------------------------------

mkDef :: GlobalSym -> Q [Dec]
mkDef def = case def of
  GlobalProc   d -> fromProc d
  GlobalStruct d -> fromStruct d

-- | Define an Ivory module, one per Haskell module.
ivoryMod :: [GlobalSym] -> Q [Dec]
ivoryMod incls = do
  modTy <- mkModTy
  mi    <- modImp
  return [modTy, mi]
  where
  modImp :: Q Dec
  modImp = do
    nm <- modNameQ
    bd <- modBody
    return $ ValD (VarP $ mkName nm)
                  (NormalB bd)
                  []
  -- Module names are uppercase and have full context.  Turn Foo.Bar.FooBar into
  -- foobar for the Ivory module name.
  mkValidHsVar []      = error "Empty module name in ivoryMod!"
  mkValidHsVar fullMod = map toLower
    $ reverse (takeWhile (/= '.') (reverse fullMod))

  modNameQ = location >>= (return . mkValidHsVar . loc_module)
  modBody = do
    nm <- stringE =<< modNameQ
    let pkg   = AppE (VarE 'I.package) nm
    let doblk = map (NoBindS . ivorySymMod) incls
    return (AppE pkg (DoE doblk))

  mkModTy = do
    nm <- modNameQ
    return $ SigD (mkName nm) (ConT ''I.Module)

  -- | Include an Ivory symbol into the Ivory module.
  ivorySymMod :: GlobalSym -> Q.Exp
  ivorySymMod def = case def of
    GlobalProc   d
      -> AppE (VarE 'I.incl) (VarE $ mkName (procSym d))
    GlobalStruct d
      -> AppE (VarE 'I.defStruct)
              (SigE (ConE 'I.Proxy)
                    (AppT (ConT ''I.Proxy)
                          (LitT $ StrTyLit $ structSym d)))


--------------------------------------------------------------------------------

-- Testing

{-
dump :: QuasiQuoter
dump = QuasiQuoter
  { quoteExp  = \str -> return $ LitE (StringL str)
  }
-}
