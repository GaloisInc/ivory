{-# LANGUAGE QuasiQuotes #-}

module Ivory.Compile.AADL.Modules where

import qualified Ivory.Language.Syntax.AST as I

import Prelude hiding (mod)

import Ivory.Compile.AADL.Gen
import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Monad

--------------------------------------------------------------------------------

-- | Compile a module.
compileModule :: [I.Module] -> I.Module -> Maybe Document
compileModule modulectx mod = case doc of
    Document _ _ [] -> Nothing
    _               -> Just doc
  where
  structs = I.modStructs mod
  doc = runCompile modulectx mod $ do
    mapM_ compileStruct (I.public structs)
    mapM_ compileStruct (I.private structs)

