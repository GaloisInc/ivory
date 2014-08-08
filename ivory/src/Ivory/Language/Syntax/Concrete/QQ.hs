{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

--
-- Ivory QuasiQuoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.QQ
  ( ivory
  , ivoryFile
  ) where

import           Prelude hiding (exp, init)
import           Data.Char

import qualified Language.Haskell.TH       as Q
import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import           Language.Haskell.TH.Quote

import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Proxy  as I
import qualified Ivory.Language.Module as I

import Ivory.Language.Syntax.Concrete.QQ.BitDataQQ
import Ivory.Language.Syntax.Concrete.QQ.StructQQ
import Ivory.Language.Syntax.Concrete.QQ.ProcQQ
import Ivory.Language.Syntax.Concrete.QQ.TypeQQ
import Ivory.Language.Syntax.Concrete.QQ.ExprQQ

import Ivory.Language.Syntax.Concrete.ParseAST
import Ivory.Language.Syntax.Concrete.Parser

--------------------------------------------------------------------------------
-- QuasiQuoters

-- | Quasiquoter for defining Ivory statements in C-like syntax.  No module
-- generated.
ivory :: QuasiQuoter
ivory = ivory' False

-- | Parse a file.  Use
--
-- ivoryFile|foo.ivory|]
--
-- To parse file ```foo.ivory```
-- Generates a module definition by default.
ivoryFile :: QuasiQuoter
ivoryFile = quoteFile (ivory' True)

-- | If Boolean is true, QuasiQuoter generates a module definition.
ivory' :: Bool -> QuasiQuoter
ivory' b = QuasiQuoter
  { quoteExp  = err "quoteExp"
  , quotePat  = err "quotePat"
  , quoteDec  = decP
  , quoteType = err "quoteType"
  }
  where
  decP str = do
    let defs      = reverse (runParser str)
    decs         <- mapM mkDef defs
    let ivoryDefs = filter (\case
                              GlobalTypeDef{}  -> False
                              GlobalConstDef{} -> False
                              _                -> True
                           ) defs
    theModule    <- ivoryMod ivoryDefs
    let maybeMod  = if b then theModule else []
    return (concat decs ++ maybeMod)
  err str = error $ str ++ " not implemented for c quasiquoter."

--------------------------------------------------------------------------------

mkDef :: GlobalSym -> Q [Dec]
mkDef def = case def of
  GlobalProc    d      -> fromProc d
  GlobalStruct  d      -> fromStruct d
  GlobalBitData d      -> fromBitData d
  GlobalTypeDef  tyDef  -> singList (fromTypeDef tyDef)
  GlobalConstDef tyDef  -> singList (fromConstDef tyDef)
  where
  singList x = (:[]) `fmap` x

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

  -- Produces both the string name and Haskell declaration name.
  --
  -- foo_package = package "foo_package" ...
  --
  modNameQ :: Q String
  modNameQ = location >>= (return . (++ "_package") . mkValidHsVar . loc_module)
    where
    -- Module names are uppercase and have full context.  Turn Foo.Bar.FooBar into
    -- foobar for the Ivory module name.
    mkValidHsVar :: String -> String
    mkValidHsVar ""      = error "Empty module name in ivoryMod!"
    mkValidHsVar fullMod = map toLower
      $ reverse (takeWhile (/= '.') (reverse fullMod))

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
                    (AppT (ConT ''I.Proxy) (LitT (StrTyLit (structSym d)))))
    GlobalTypeDef{}
      -> error "global typedef in ivorySymMod"
    GlobalConstDef{}
      -> error "global constDef in ivorySymMod"


--------------------------------------------------------------------------------

-- Testing

{-
dump :: QuasiQuoter
dump = QuasiQuoter
  { quoteExp  = \str -> return $ LitE (StringL str)
  }
-}
