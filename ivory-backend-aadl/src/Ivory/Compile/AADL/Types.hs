{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Compile.AADL.Types where

import MonadLib
import Data.Monoid

import Ivory.Compile.AADL.AST

newtype CompileM a = Compile
  { unCompile :: WriterT Document Id a }
  deriving (Functor, Monad)

type Compile = CompileM ()

putTypeDefinition :: DTypeDef -> Compile
putTypeDefinition td = Compile (put mempty { doc_definitions = [TypeDefinition td]})

putImport :: String -> Compile
putImport i = Compile (put mempty {doc_imports = [i]})


