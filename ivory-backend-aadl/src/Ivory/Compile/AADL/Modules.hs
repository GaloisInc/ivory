{-# LANGUAGE QuasiQuotes #-}

module Ivory.Compile.AADL.Modules where

import qualified Ivory.Language.Syntax.AST as I

import Prelude hiding (mod)

import Ivory.Compile.AADL.Gen
import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Types

import MonadLib (runM)

--------------------------------------------------------------------------------

-- | Compile a module.
compileModule :: I.Module -> Maybe Document
compileModule mod = case doc of
    Document _ _ [] -> Nothing
    _               -> Just $ doc { doc_name = I.modName mod }
  where
  structs = I.modStructs mod
  compRes = (snd . runM . unCompile)
  doc     = compRes $ do
    mapM_ compileStruct (I.public structs)
    mapM_ compileStruct (I.private structs)


