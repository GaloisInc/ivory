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
  , ivoryBlk
  )
    where

import           Prelude hiding (exp, init, const)
import           Data.Char
import           Data.Maybe
import           System.FilePath
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L

import qualified Language.Haskell.TH        as Q
import           Language.Haskell.TH        hiding (Stmt, Exp, Type)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (addDependentFile)

import qualified Ivory.Language.Const  as I
import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Proxy  as I
import qualified Ivory.Language.Module as I

import Ivory.Language.Syntax.Concrete.QQ.BitDataQQ
import Ivory.Language.Syntax.Concrete.QQ.StructQQ
import Ivory.Language.Syntax.Concrete.QQ.ProcQQ
import Ivory.Language.Syntax.Concrete.QQ.TypeQQ
import Ivory.Language.Syntax.Concrete.QQ.ExprQQ
import Ivory.Language.Syntax.Concrete.QQ.StmtQQ
import Ivory.Language.Syntax.Concrete.QQ.Common

import Ivory.Language.Syntax.Concrete.ParseAST hiding (tyDef)
import Ivory.Language.Syntax.Concrete.Lexer (scan)
import qualified Ivory.Language.Syntax.Concrete.Parser as P
import qualified Ivory.Language.Syntax.Concrete.ParseCore as P

--------------------------------------------------------------------------------

mkParser :: P.Parser a -> FilePath -> L.Text -> a
mkParser p fp txt = P.runParser (scan fp txt) p

topParser :: FilePath -> L.Text -> [GlobalSym]
topParser = mkParser P.topParser

stmtsParser :: FilePath -> L.Text -> [Stmt]
stmtsParser = mkParser P.stmtsParser

--------------------------------------------------------------------------------
-- QuasiQuoters

-- | Quasiquoter for defining Ivory statements in C-like syntax.  No module
-- generated.
ivory :: QuasiQuoter
ivory = justDecQQ decP
  where
  decP str = do
    loc <- location
    let defs = reverse (topParser (loc_filename loc) (L.pack str))
    decs    <- mapM mkDef defs
    return (concat decs)

-- | Quasiquoter for defining blocks of Ivory statements.
ivoryBlk :: QuasiQuoter
ivoryBlk = justExpQQ stmtsP
  where
  stmtsP str = do
    loc <- location
    let ss = reverse (stmtsParser (loc_filename loc) (L.pack str))
    fromProgram ss

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
    str <- runIO (L.readFile filePath)
    addDependentFile filePath
    let defs      = reverse (topParser filePath str)
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
  err str = error $ str ++ " not implemented for Ivory quasiquoter."

justExpQQ :: (String -> Q Q.Exp) -> QuasiQuoter
justExpQQ expQQ = QuasiQuoter
  { quoteExp  = expQQ
  , quotePat  = err "quotePat"
  , quoteDec  = err "quoteDec"
  , quoteType = err "quoteType"
  }
  where
  err str = error $ str ++ " not implemented for Ivory quasiquoter."

--------------------------------------------------------------------------------

-- | Filter module data from all global definitions.
getModData :: GlobalSym -> Maybe ModuleData
getModData sym = case sym of
  GlobalProc       d -> Just (ModProc d)
  GlobalInclProc   d -> Just (ModImportProc d)
  GlobalExtern     e -> Just (ModExtern e)
  GlobalStruct     d -> Just (ModStruct d)
  GlobalBitData{}    -> Nothing
  GlobalTypeDef{}    -> Nothing
  GlobalConstDef{}   -> Nothing
  GlobalInclude    d -> Just (ModInclude d)

mkDef :: GlobalSym -> Q [Dec]
mkDef def = case def of
  GlobalProc     d      -> fromProc d
  GlobalInclProc d      -> fromInclProc d
  GlobalExtern   e      -> fromExtern e
  GlobalStruct   d      -> fromStruct d
  GlobalBitData  d      -> fromBitData d
  GlobalTypeDef  tyDef  -> fromTypeDef tyDef
  GlobalConstDef const  -> fromConstDef const
  -- No definition to make for includes, source depends.
  GlobalInclude{}       -> return []

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
    ModProc   d
      -> AppE (VarE 'I.incl) (VarE $ mkName (procSym d))
    ModStruct d
      -> AppE (VarE 'I.defStruct)
              (SigE (ConE 'I.Proxy)
                    (AppT (ConT ''I.Proxy) (LitT (StrTyLit (structSym d)))))
    ModInclude incl
      -> AppE (VarE 'I.depend) (VarE $ mkName $ inclModule incl)
    ModImportProc proc
      -> AppE (VarE 'I.incl) (VarE $ mkName $ procInclSym proc)
    ModExtern ext
      -> AppE (VarE 'I.inclSym) (VarE $ mkName $ externSym ext)

--------------------------------------------------------------------------------

-- | Data to put in the Ivory module.
data ModuleData =
    ModProc       ProcDef
  | ModStruct     StructDef
  | ModInclude    IncludeDef
  | ModImportProc IncludeProc
  | ModExtern     Extern
  deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------------------

fromExtern :: Extern -> Q [Dec]
fromExtern (Extern sym file ty loc) = do
  tyQ <- runToQ (fromType ty)
  d   <- def
  let nm  = mkName sym
  let imp = ValD (VarP nm) (NormalB d) []
  ln  <- lnPragma loc
  return (ln ++ [SigD nm (fst tyQ), imp])
  where
  def = do
    let nm = AppE (VarE 'I.extern) (LitE $ StringL sym)
    return (AppE nm (LitE $ StringL file))

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
