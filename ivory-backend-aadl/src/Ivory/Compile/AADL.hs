-- Umbrella module

module Ivory.Compile.AADL
  ( compileModule
  , documentToFile
  , module Ivory.Compile.AADL.AST
  , ivoryTypesDoc
  ) where

import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Modules
import Ivory.Compile.AADL.PrettyPrint

ivoryTypesDoc :: Document
ivoryTypesDoc = Document
  { doc_name = "Ivory_Types"
  , doc_imports = [ "Data_Model" ]
  , doc_definitions =
     [ TypeDefinition $ DTStruct "ivory_dynarray" []
     ]
  }


