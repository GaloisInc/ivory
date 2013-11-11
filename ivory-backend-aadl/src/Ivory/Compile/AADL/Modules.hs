{-# LANGUAGE QuasiQuotes #-}

module Ivory.Compile.AADL.Modules where

import qualified Ivory.Language.Syntax.AST as I

import Prelude hiding (mod)

import Ivory.Compile.AADL.Gen
import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Monad

--------------------------------------------------------------------------------

-- | Compile a module.
compileModule :: [I.Module] -> I.Module -> TypeCtxM (Maybe (Document, [Warning]))
compileModule modulectx mod = do
  (doc, warnings) <- runCompile modulectx mod $ do
    mapM_ compileStruct (I.public structs)
    mapM_ compileStruct (I.private structs)
  case doc of
    Document _ _ [] -> return Nothing
    _               -> return (Just (doc, warnings))
  where
  structs = I.modStructs mod

compileTypeCtx :: TypeCtxM a -> (a, Document)
compileTypeCtx = runTypeCtx

