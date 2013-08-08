-- | Ivory backend for AADL descriptions of struct types

module Ivory.Compile.AADL.Gen where

import qualified Ivory.Language.Syntax as I
import qualified Ivory.Language.Proc as P

import Ivory.Compile.AADL.AST
import Ivory.Compile.AADL.Types

import Prelude hiding (exp, abs, signum)
import qualified Prelude as P
import Control.Monad (when)
import Data.List (foldl')

import Data.Loc (noLoc)

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

-- | Type conversion outside of an assignment context.  This converts arrays to
-- arrays, and carrays to pointers.
toType :: I.Type -> TypeName
toType  = toTypeCxt $ \ t -> case t of
  I.TyCArray t'  -> undefined -- [cty| $ty:(toType t') *          |]
  I.TyArr len t' -> undefined --[cty| $ty:(toType t')[$uint:len] |]
  _              -> undefined --[cty| $ty:(toType t) *           |]


--------------------------------------------------------------------------------
-- | C type conversion, with a special case for references and pointers.
toTypeCxt :: (I.Type -> TypeName) -> I.Type -> TypeName
toTypeCxt arrCase = convert
  where
  convert ty = case ty of
    I.TyVoid              -> undefined -- [cty| void |]
    I.TyChar              -> undefined -- [cty| char |]
    I.TyInt i             -> undefined -- intSize i
    I.TyWord w            -> undefined -- wordSize w
    I.TyBool              -> undefined -- [cty| typename bool |]
    I.TyFloat             -> undefined -- [cty| float |]
    I.TyDouble            -> undefined -- [cty| double |]
    I.TyStruct nm         -> undefined -- [cty| struct $id:nm |]
    I.TyConstRef t        -> undefined -- [cty| const $ty:(arrCase t) |]
    -- Reference is a guaranted non-NULL pointer.
    I.TyRef t             -> undefined -- arrCase t
    I.TyPtr t             -> undefined -- arrCase t
    I.TyArr len t         -> undefined -- [cty| $ty:(convert t)[$uint:len] |]
    I.TyCArray t          -> undefined -- [cty| $ty:(convert t) * |]
    I.TyProc retTy argTys -> undefined --
    --  [cty| $ty:(convert retTy) (*)
    --        ($params:(map (toParam . convert) argTys)) |]

intSize :: I.IntSize -> TypeName
intSize I.Int8  = undefined -- [cty| typename int8_t  |]
intSize I.Int16 = undefined -- [cty| typename int16_t |]
intSize I.Int32 = undefined -- [cty| typename int32_t |]
intSize I.Int64 = undefined -- [cty| typename int64_t |]

wordSize :: I.WordSize -> TypeName
wordSize I.Word8  = undefined -- [cty| typename uint8_t  |]
wordSize I.Word16 = undefined -- [cty| typename uint16_t |]
wordSize I.Word32 = undefined -- [cty| typename uint32_t |]
wordSize I.Word64 = undefined -- [cty| typename uint64_t |]

