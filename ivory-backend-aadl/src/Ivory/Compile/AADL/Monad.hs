{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Compile.AADL.Monad where

import MonadLib
import Data.Monoid

import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Identifier

import qualified Ivory.Language.Syntax as I

-- Collects a document of AADL types generated for use in other
-- documents.
newtype TypeCtxM a = TypeCtxM
  { unTypeCtxM :: StateT Document Id a }
  deriving (Functor, Monad)

newtype CompileM n a = CompileM
  { unCompile :: StateT Document                 -- Building compiled document
                  (ReaderT (I.Module,[I.Module]) -- Ivory module Environment
                    (StateT [(n,String)]         -- maps identifiers to names
                      (WriterT [Warning]         -- Log of warnings
                      TypeCtxM))) a }            -- AADL Type context
  deriving (Functor, Monad)


runTypeCtx :: TypeCtxM a -> (a, Document)
runTypeCtx (TypeCtxM c) = runId $ runStateT d c
  where
  d = mempty { doc_name    = "TowerArrays"
             , doc_imports = ["Data_Model"]
             }

getTypeCtxM :: TypeCtxM Document
getTypeCtxM = TypeCtxM $ get

setTypeCtxM :: Document -> TypeCtxM ()
setTypeCtxM d = TypeCtxM $ set d

getTypeCtx :: CompileM n Document
getTypeCtx = CompileM $ lift $ lift $ lift $ lift $ getTypeCtxM

setTypeCtx :: Document -> CompileM n ()
setTypeCtx d = CompileM $ lift $ lift $ lift $ lift $ setTypeCtxM d


runTypeCtx :: TypeCtxM a -> (a, Document)
runTypeCtx (TypeCtxM c) = runId
                        $ runStateT d c
  where
  d = mempty { doc_name    = "TowerArrays"
             , doc_imports = ["Data_Model"]
             }

getTypeCtxM :: TypeCtxM Document
getTypeCtxM = TypeCtxM $ get

setTypeCtxM :: Document -> TypeCtxM ()
setTypeCtxM d = TypeCtxM $ set d

getTypeCtx :: CompileM Document
getTypeCtx = Compile $ lift $ lift $ lift $ lift $ getTypeCtxM

setTypeCtx :: Document -> CompileM ()
setTypeCtx d = Compile $ lift $ lift $ lift $ lift $ setTypeCtxM d


-- Using snoc to write to lists, reverse at end of run.
runCompile :: [I.Module] -> I.Module -> CompileM n () -> TypeCtxM (Document, [Warning])
runCompile allms m (CompileM c) = do
  (((_retv,d),_scope),ws) <- runWriterT
                           $ runStateT []
                           $ runReaderT modulectx
                           $ runStateT mempty c
  return (reversed d, ws)
  where
  reversed d = Document
    { doc_name        = identifier (I.modName m)
    , doc_imports     = reverse (doc_imports d)
    , doc_definitions = reverse (doc_definitions d)
    }
  modulectx = (m, allms)

writeTypeDefinition :: DTypeDef -> CompileM n ()
writeTypeDefinition dtdef = CompileM $ do
  d <- get
  when (td `notElem` (doc_definitions d)) $
    set d { doc_definitions = td:(doc_definitions d) }
  where td = TypeDefinition dtdef

writeTypeCtxDefinition :: DTypeDef -> CompileM n ()
writeTypeCtxDefinition dtdef = do
    d <- getTypeCtx
    when (td `notElem` (doc_definitions d)) $
      setTypeCtx d { doc_definitions = td:(doc_definitions d) }
  where
  td = TypeDefinition dtdef

writeThreadDefinition :: ThreadDef -> CompileM n ()
writeThreadDefinition tdef = CompileM $ do
  d <- get
  set d { doc_definitions = td:(doc_definitions d) }
  where td = ThreadDefinition tdef

writeProcessDefinition :: ProcessDef -> CompileM n ()
writeProcessDefinition pdef = CompileM $ do
  d <- get
  set d { doc_definitions = pd:(doc_definitions d) }
  where pd = ProcessDefinition pdef

writeImport :: String -> CompileM n ()
writeImport i = CompileM $ do
  d <- get
  when (i `notElem` (doc_imports d)) $ 
    set d { doc_imports = i:(doc_imports d) }

qualTypeName :: String -> String -> CompileM n TypeName
qualTypeName q n = do
  writeImport q
  return $ QualTypeName q n

getIModContext :: CompileM n (I.Module,[I.Module])
getIModContext = CompileM $ lift ask

getIdentifierMap :: CompileM n [(n,String)]
getIdentifierMap = CompileM $ lift get

setIdentifierMap :: [(n,String)] -> CompileM n ()
setIdentifierMap s = CompileM $ lift $ set s

namedIdentifier :: n -> String -> CompileM n ()
namedIdentifier ident name = do
  m <- getIdentifierMap
  setIdentifierMap ((ident,name):m)

writeWarning :: Warning -> CompileM n ()
writeWarning w = CompileM $ put [w]

uniquenessWarning :: String -> CompileM n ()
uniquenessWarning msg = do
  writeWarning $ UniquenessWarning msg

getScope :: CompileM [(String, [String])]
getScope = Compile $ lift get

setScope :: [(String,[String])] -> CompileM ()
setScope s = Compile $ lift $ set s

innerScope :: String -> CompileM a -> CompileM a
innerScope name k = do
  s <- getScope
  setScope ((name, []):s)
  r <- k
  setScope s
  return r

existsInScope :: String -> CompileM Bool
existsInScope ident = do
  s <- getScope
  return $ or $ map (elem ident . snd) s

addToScope :: String -> CompileM ()
addToScope ident = do
  ((name,s):ss) <- getScope
  setScope ((name,(ident:s)):ss)

writeWarning :: Warning -> CompileM ()
writeWarning w = Compile $ put [w]

uniquenessWarning :: String -> CompileM ()
uniquenessWarning msg = do
  scopes <- getScope
  writeWarning $ UniquenessWarning msg (map fst scopes)


