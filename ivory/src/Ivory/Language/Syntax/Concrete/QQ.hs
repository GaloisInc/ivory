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
  -- ( ivory
  -- , ivoryFile
  -- )
    where

import           Prelude hiding (exp, init, const)
import           Data.Char
import           Data.Maybe
import           System.FilePath

import qualified Language.Haskell.TH        as Q
import           Language.Haskell.TH        hiding (Stmt, Exp, Type)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (addDependentFile)

import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Proxy  as I
import qualified Ivory.Language.Module as I

import Ivory.Language.Syntax.Concrete.QQ.BitDataQQ
import Ivory.Language.Syntax.Concrete.QQ.StructQQ
import Ivory.Language.Syntax.Concrete.QQ.ProcQQ
import Ivory.Language.Syntax.Concrete.QQ.TypeQQ
import Ivory.Language.Syntax.Concrete.QQ.ExprQQ

import Ivory.Language.Syntax.Concrete.ParseAST hiding (tyDef)
import Ivory.Language.Syntax.Concrete.Parser

--------------------------------------------------------------------------------
-- QuasiQuoters

-- | Quasiquoter for defining Ivory statements in C-like syntax.  No module
-- generated.
ivory :: QuasiQuoter
ivory = justDecQQ decP
  where
  decP str = do
    let defs = reverse (runParser str)
    decs    <- mapM mkDef defs
    return (concat decs)

-- | Parse a file.  Use
--
-- ivoryFile|foo.ivory|]
--
-- To parse file ```foo.ivory``` Generates a module definition by default with a
-- module name that is constructed from the filename and path such that
--
-- "dira/dirb/foobar.ivory"
--
-- has a module name
--
-- diradirbfoobar
--
-- Like `quoteFile` except we also process the filename.
ivoryFile :: QuasiQuoter
ivoryFile = justDecQQ decP
  where
  decP filePath = do
    str <- runIO (readFile filePath)
    addDependentFile filePath
    let defs      = reverse (runParser str)
    decs         <- mapM mkDef defs
    let fileName  = concat
                  $ filter (not . (any isPathSeparator))
                  $ splitDirectories
                  $ dropExtensions filePath
    theModule    <- ivoryMod (map toLower fileName)
                             (catMaybes $ map getModData defs)
    return (concat decs ++ theModule)

justDecQQ :: (String -> Q [Dec]) -> QuasiQuoter
justDecQQ decQQ = QuasiQuoter
  { quoteExp  = err "quoteExp"
  , quotePat  = err "quotePat"
  , quoteDec  = decQQ
  , quoteType = err "quoteType"
  }
  where
  err str = error $ str ++ " not implemented for c quasiquoter."

--------------------------------------------------------------------------------

-- | Filter module data from all global definitions.
getModData :: GlobalSym -> Maybe ModuleData
getModData sym = case sym of
  GlobalProc       d -> Just (ModProc d)
  GlobalStruct     d -> Just (ModStruct d)
  GlobalBitData{}    -> Nothing
  GlobalTypeDef{}    -> Nothing
  GlobalConstDef{}   -> Nothing
  GlobalInclude    i -> Just (ModInclude i)

mkDef :: GlobalSym -> Q [Dec]
mkDef def = case def of
  GlobalProc    d       -> fromProc d
  GlobalStruct  d       -> fromStruct d
  GlobalBitData d       -> fromBitData d
  GlobalTypeDef tyDef   -> singList (fromTypeDef tyDef)
  GlobalConstDef const  -> singList (fromConstDef const)
  -- No definition to make for includes.
  GlobalInclude{}       -> return []
  where
  singList x = (:[]) `fmap` x

-- | Define an Ivory module, one per Haskell module.
ivoryMod :: String -> [ModuleData] -> Q [Dec]
ivoryMod _modName []    = return []
ivoryMod  modName incls = do
  modTy <- mkModTy
  mi    <- modImp
  return [modTy, mi]
  where
  modImp :: Q Dec
  modImp = do
    bd <- modBody
    return $ ValD (VarP $ mkName modName)
                  (NormalB bd)
                  []

  modBody = do
    nm       <- stringE modName
    let pkg   = AppE (VarE 'I.package) nm
    let doblk = map (NoBindS . ivorySymMod) incls
    return (AppE pkg (DoE doblk))

  mkModTy = return $ SigD (mkName modName) (ConT ''I.Module)

  -- | Include an Ivory symbol into the Ivory module.
  ivorySymMod :: ModuleData -> Q.Exp
  ivorySymMod m = case m of
    ModProc      d
      -> AppE (VarE 'I.incl) (VarE $ mkName (procSym d))
    ModStruct    d
      -> AppE (VarE 'I.defStruct)
              (SigE (ConE 'I.Proxy)
                    (AppT (ConT ''I.Proxy) (LitT (StrTyLit (structSym d)))))
    ModInclude    incl
      -> AppE (VarE 'I.depend) (VarE $ mkName $ inclModule incl)

--------------------------------------------------------------------------------

-- | Data to put in the Ivory module.
data ModuleData =
    ModProc    ProcDef
  | ModStruct  StructDef
  | ModInclude IncludeDef
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------

-- My own `quoteFile` implementation to grab the file name to pass to module
-- construction.
-- quoteFile :: QuasiQuoter -> QuasiQuoter
-- quoteFile (QuasiQuoter { quoteExp  = qe
--                        , quotePat  = qp
--                        , quoteType = qt
--                        , quoteDec  = qd }
--           )
--   = QuasiQuoter { quoteExp  = get qe
--                 , quotePat  = get qp
--                 , quoteType = get qt
--                 , quoteDec  = get qd
--                 }
--   where
--    get :: (String -> Q a) -> String -> Q a
--    get old_quoter file_name = do file_cts <- runIO (readFile file_name)
--                                  addDependentFile file_name
--                                  old_quoter file_cts
--                                  return file_name

--------------------------------------------------------------------------------

-- Testing

{-
dump :: QuasiQuoter
dump = QuasiQuoter
  { quoteExp  = \str -> return $ LitE (StringL str)
  }
-}
