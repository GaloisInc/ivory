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

module Ivory.Language.Syntax.Concrete.QQ   ( ivory2 ) where

import           Prelude hiding (exp, init)
import qualified Prelude as P
import           Data.Char

import qualified Language.Haskell.TH       as Q
import           Language.Haskell.TH       hiding (Stmt, Exp, Type)
import           Language.Haskell.TH.Quote

import qualified Ivory.Language as I

import Ivory.Language.Syntax.Concrete.QQ.ProcQQ
import Ivory.Language.Syntax.Concrete.ParseAST
import Ivory.Language.Syntax.Concrete.Parser

--------------------------------------------------------------------------------

-- | Quasiquoter for defining Ivory statements in C-like syntax.
ivory2 :: QuasiQuoter
ivory2 = QuasiQuoter
  { quoteExp  = err "quoteExp"
  , quotePat  = err "quotePat"
  , quoteDec  = decP
  , quoteType = err "quoteType"
  }
  where
  decP str = do
    let procs     = runParser str
    cDefs         <- mapM fromProc procs
    let procSyms  = foldr toGlobalSym [] procs
    ivoryModDefs  <- ivoryMod procSyms
    return (concat cDefs ++ ivoryModDefs)
  err str = error $ str ++ " not implemented for c quasiquoter."

--------------------------------------------------------------------------------

-- | Add a procedure symbol to the global symbols.
toGlobalSym :: ProcDef -> [GlobalSym] -> [GlobalSym]
toGlobalSym procS = (ProcSym (procSym procS) :)

-- | Include an Ivory symbol into the Ivory module.
ivorySymMod :: GlobalSym -> Q.Exp
ivorySymMod sym = case sym of
  ProcSym procName   -> AppE (VarE 'I.incl) (VarE $ mkName procName)

-- | Define an Ivory module, one per module.
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

--------------------------------------------------------------------------------

-- Testing

{-
dump :: QuasiQuoter
dump = QuasiQuoter
  { quoteExp  = \str -> return $ LitE (StringL str)
  }
-}
