{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Compile.AADL.Monad where

import MonadLib
import Data.Monoid

import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Identifier

import qualified Ivory.Language.Syntax as I

newtype CompileM a = Compile
  { unCompile :: StateT Document                 -- Building compiled document
                  (ReaderT (I.Module,[I.Module]) -- Ivory module Environment
                    (StateT [(String,[String])]  -- Scope name and identifiers
                      (WriterT [Warning]         -- Log of warnings
                      Id))) a }
  deriving (Functor, Monad)

data Warning
  = UniquenessWarning String [String] -- msg, scope

type Compile = CompileM ()

-- Using snoc to write to lists, reverse at end of run.
runCompile :: [I.Module] -> I.Module -> Compile -> (Document, [Warning])
runCompile allms m (Compile c) = (reversedDoc, ws)
  where
  dname = identifier (I.modName m)
  reversedDoc = Document
    { doc_name        = identifier (I.modName m)
    , doc_imports     = reverse (doc_imports d)
    , doc_definitions = reverse (doc_definitions d)
    }
  modulectx = (m, allms)
  (((_retv,d),_scope),ws) = runId
                          $ runWriterT
                          $ runStateT [(dname ++ " document",[])]
                          $ runReaderT modulectx
                          $ runStateT mempty c

writeTypeDefinition :: DTypeDef -> Compile
writeTypeDefinition dtdef = Compile $ do
  d <- get
  when (td `notElem` (doc_definitions d)) $
    set d { doc_definitions = td:(doc_definitions d) }
  where td = TypeDefinition dtdef

writeThreadDefinition :: ThreadDef -> Compile
writeThreadDefinition tdef = Compile $ do
  d <- get
  set d { doc_definitions = td:(doc_definitions d) }
  where td = ThreadDefinition tdef

writeProcessDefinition :: ProcessDef -> Compile
writeProcessDefinition pdef = Compile $ do
  d <- get
  set d { doc_definitions = pd:(doc_definitions d) }
  where pd = ProcessDefinition pdef

writeImport :: String -> Compile
writeImport i = Compile $ do
  d <- get
  when (i `notElem` (doc_imports d)) $ 
    set d { doc_imports = i:(doc_imports d) }

qualTypeName :: String -> String -> CompileM TypeName
qualTypeName q n = do
  writeImport q
  return $ QualTypeName q n

getIModContext :: CompileM (I.Module,[I.Module])
getIModContext = Compile $ lift ask

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
existsInScope identifier = do
  s <- getScope
  return $ or $ map (elem identifier . snd) s

addToScope :: String -> CompileM ()
addToScope identifier = do
  ((name,s):ss) <- getScope
  setScope ((name,(identifier:s)):ss)

writeWarning :: Warning -> CompileM ()
writeWarning w = Compile $ put [w]

uniquenessWarning :: String -> CompileM ()
uniquenessWarning msg = do
  scopes <- getScope
  writeWarning $ UniquenessWarning msg (map fst scopes)


