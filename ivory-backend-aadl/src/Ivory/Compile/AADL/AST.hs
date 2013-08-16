
module Ivory.Compile.AADL.AST where

import Data.Monoid

data Document =
  Document
    { doc_name        :: String
    , doc_imports     :: [String]
    , doc_definitions :: [Definition]
    } deriving (Eq, Show)

instance Monoid Document where
  mempty = Document "" [] []
  mappend a b = Document
    { doc_name        = (doc_name a)        <> (doc_name b)
    , doc_imports     = (doc_imports a)     <> (doc_imports b)
    , doc_definitions = (doc_definitions a) <> (doc_definitions b)
    }

data Definition
  = TypeDefinition DTypeDef
  | ThreadDefinition ThreadDef
  | ProcessDefinition ProcessDef
  deriving (Eq, Show)

data TypeName
  = UnqualTypeName String
  | QualTypeName String String
  | DotTypeName TypeName String
  deriving (Eq, Show)

data DTypeDef
  = DTStruct String [DTField]
  | DTArray String Int TypeName
  deriving (Eq, Show)

data DTField
  = DTField String TypeName
  deriving (Eq, Show)

data ThreadDef
  = ThreadDef String [ThreadFeature] [ThreadProperty]
  deriving (Eq, Show)

data ThreadFeature
  = ThreadFeaturePort String PortKind PortDir TypeName [ThreadProperty]
  deriving (Eq, Show)

data PortKind
  = PortKindData
  | PortKindEvent
  deriving (Eq, Show)

data PortDir
  = In
  | Out
  deriving (Eq, Show)

data ThreadProperty =
  ThreadProperty String String -- Key, Value
  deriving (Eq, Show)

data ProcessDef =
  ProcessDef String [ProcessComponent] [ProcessConnection]
  deriving (Eq, Show)

data ProcessComponent
  = ProcessComponent String String -- Name, Thread Name
  deriving (Eq, Show)

data ProcessPort
  = ProcessPort String String -- Thread Name, Feature Name
  deriving (Eq, Show)

data ProcessConnection
  = ProcessConnection ProcessPort ProcessPort -- From, To
  deriving (Eq, Show)


