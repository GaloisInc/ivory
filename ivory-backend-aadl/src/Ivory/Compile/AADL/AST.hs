
module Ivory.Compile.AADL.AST where

data Document =
  Document
    { doc_name :: String
    , doc_imports :: [String]
    , doc_definitions :: [Definition]
    }

data Definition = TypeDefinition DType

data TypeName = UnqualTypeName String
              | QualTypeName String String

data DType = DTDeclaration TypeName
           | DTImplementation TypeName String [DTField]

data DTField = DTField String TypeName

