{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Compile.AADL.Types where

import MonadLib
import Data.Monoid

import Ivory.Compile.AADL.AST

instance Monoid Document where
  mempty = Document "" [] []
  mappend a b = Document
    { doc_name        = (doc_name a)        <> (doc_name b)
    , doc_imports     = (doc_imports a)     <> (doc_imports b)
    , doc_definitions = (doc_definitions a) <> (doc_definitions b)
    }

newtype CompileM a = Compile
  { unCompile :: WriterT Document Id a }
  deriving (Functor, Monad)

type Compile = CompileM ()

putTypeDefinition :: DType -> Compile
putTypeDefinition td = Compile (put mempty { doc_definitions = [TypeDefinition td]})

putImport :: String -> Compile
putImport i = Compile (put mempty {doc_imports = [i]})



