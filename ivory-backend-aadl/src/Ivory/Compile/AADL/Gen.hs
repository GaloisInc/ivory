-- | Ivory backend for AADL descriptions of struct types

module Ivory.Compile.AADL.Gen where

import qualified Ivory.Language.Syntax as I

import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Types

--------------------------------------------------------------------------------
-- | Compile a struct.
compileStruct :: I.Struct -> Compile
compileStruct def = case def of
  I.Struct n fs    -> do
    let tname = UnqualTypeName n
    putTypeDefinition $ DTDeclaration tname
    putTypeDefinition $ DTImplementation tname "cstruct" (map mkField fs)
  _ -> return ()

mkField :: I.Typed String -> DTField
mkField field = DTField (I.tValue field) (toType (I.tType field))

toType :: I.Type -> TypeName
toType = convert
  where
  convert ty = case ty of
    I.TyVoid              -> QualTypeName "Base_Types" "Void"
    I.TyChar              -> QualTypeName "Base_Types" "Char"
    I.TyInt i             -> intSize i
    I.TyWord w            -> wordSize w
    I.TyBool              -> QualTypeName "Base_Types" "Bool"
    I.TyFloat             -> QualTypeName "Base_Types" "Float"
    I.TyDouble            -> QualTypeName "Base_Types" "Double"
    I.TyStruct nm         -> StructTypeName nm
    I.TyConstRef t        -> RefTypeName Const         (toType t)
    I.TyRef t             -> RefTypeName Mutable       (toType t)
    I.TyPtr t             -> RefTypeName Mutable       (toType t) -- not strictly true
    I.TyArr len t         -> ArrayTypeName (Just len)  (toType t)
    I.TyCArray t          -> ArrayTypeName Nothing     (toType t)
    I.TyProc retTy argTys -> ProcTypeName (toType retTy) (map toType argTys)

intSize :: I.IntSize -> TypeName
intSize I.Int8  = QualTypeName "Base_Types" "Signed_8"
intSize I.Int16 = QualTypeName "Base_Types" "Signed_16"
intSize I.Int32 = QualTypeName "Base_Types" "Signed_32"
intSize I.Int64 = QualTypeName "Base_Types" "Signed_64"

wordSize :: I.WordSize -> TypeName
wordSize I.Word8  = QualTypeName "Base_Types" "Unsigned_8"
wordSize I.Word16 = QualTypeName "Base_Types" "Unsigned_16"
wordSize I.Word32 = QualTypeName "Base_Types" "Unsigned_32"
wordSize I.Word64 = QualTypeName "Base_Types" "Unsigned_64"

