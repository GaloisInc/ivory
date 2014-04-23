{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Language.Syntax.Concrete.QQ.StructQQ
  ( fromStruct
  ) where

import qualified Ivory.Language.Area  as A
import           Ivory.Language.Proxy
import qualified Ivory.Language.Scope as S
import           Ivory.Language.SizeOf
import qualified Ivory.Language.Struct as S
import qualified Ivory.Language.Syntax.AST  as AST
import qualified Ivory.Language.Syntax.Type as AST

import           Ivory.Language.Syntax.Concrete.ParseAST

import           Data.Traversable (sequenceA)
import           Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as T
import           Language.Haskell.TH.Quote()

fromStruct :: StructDef -> Q [Dec]
fromStruct def = case def of
  StructDef n fs -> do
    let sym = mkSym n
    sizeOfDefs <- mkIvorySizeOf sym fs
    sequence (mkIvoryStruct sym def ++ sizeOfDefs ++ mkFields sym fs)

  AbstractDef n _hdr -> sequence (mkIvoryStruct (mkSym n) def)

  -- StringDef name len -> mkStringDef name len
  where
  mkSym n = litT (strTyLit n)

-- IvoryStruct -----------------------------------------------------------------

-- | Generate an @IvoryStruct@ instance.
mkIvoryStruct :: TypeQ -> StructDef -> [DecQ]
mkIvoryStruct sym def =
  [ instanceD (cxt []) (appT (conT ''S.IvoryStruct) sym) [mkStructDef def]
  ]

mkStructDef :: StructDef -> DecQ
mkStructDef def = funD 'S.structDef
  [ clause [] (normalB [| S.StructDef $astStruct |] ) []
  ]
  where
  astStruct = case def of
    StructDef n fs    -> [| AST.Struct $(stringE n)
                                         $(listE (map mkField fs)) |]
    AbstractDef n hdr -> [| AST.Abstract $(stringE n) $(stringE hdr) |]
    StringDef _ _     -> error "unexpected string definition"

  mkField f =
    [| AST.Typed
         { AST.tType  = $(mkTypeE (fieldType f))
         , AST.tValue = $(stringE (fieldName f))
         }
    |]


-- IvorySizeOf -----------------------------------------------------------------

mkIvorySizeOf :: TypeQ -> [Field] -> Q [DecQ]
mkIvorySizeOf sym fields = do
  mbs <- mapM fieldSizeOfBytes fields
  case sequenceA mbs of
    Just tys | not (null tys) -> return (mkIvorySizeOfInst sym tys)
    _                         -> return []

-- | Return a call to 'sizeOfBytes' if there's an instance for the type named in
-- the field.
fieldSizeOfBytes :: Field -> Q (Maybe T.Type)
fieldSizeOfBytes field = do
  ty          <- mkType (fieldType field)
  hasInstance <- isInstance ''IvorySizeOf [ty]
  if hasInstance
     then return (Just ty)
     else return  Nothing

mkIvorySizeOfInst :: TypeQ -> [T.Type] -> [DecQ]
mkIvorySizeOfInst sym tys =
  [ instanceD (cxt []) (appT (conT ''IvorySizeOf) struct) [mkSizeOfBytes tys]
  ]
  where
  struct = [t| A.Struct $sym |]

mkSizeOfBytes :: [T.Type] -> DecQ
mkSizeOfBytes tys = funD 'sizeOfBytes
  [ clause [wildP] (normalB (foldr1 add exprs)) []
  ]
  where
  exprs   = [ [| sizeOfBytes (Proxy :: A.AProxy $(return ty)) |] | ty <- tys ]
  add l r = [| $l + ($r :: Integer) |]

-- Field Labels ----------------------------------------------------------------

mkFields :: TypeQ -> [Field] -> [DecQ]
mkFields sym = concatMap (mkLabel sym)

mkLabel :: TypeQ -> Field -> [DecQ]
mkLabel sym f =
  [ sigD field [t| S.Label $sym $(mkType (fieldType f)) |]
  , funD field [clause [] (normalB [| S.Label $(stringE (fieldName f)) |]) []]
  ]
  where
  field = mkName (fieldName f)

mkType :: Area -> TypeQ
mkType = mkType' . toAreaCon

toAreaCon :: Area -> TypeCon
toAreaCon area = case area of
  TyStruct str
    -> TCon "Struct"
  TyArray a i
    -> TApp (TApp (TCon "Array") (TNat i)) (toAreaCon a)
  TyStored ty
    -> TApp (TCon "Stored") (toTyCon ty)

toTyCon :: Type -> TypeCon
toTyCon ty = case ty of
  TyBool   -> TCon "IBool"
  TyChar   -> TCon "IChar"
  TyFloat  -> TCon "IFloat"
  TyDouble -> TCon "IDouble"
  TyInt sz -> TCon $ case sz of
                Int8   -> "Sint8"
                Int16  -> "Sint16"
                Int32  -> "Sint32"
                Int64  -> "Sint64"
  TyWord sz -> TCon $ case sz of
                Word8   -> "Uint8"
                Word16  -> "Uint16"
                Word32  -> "Uint32"
                Word64  -> "Uint64"

mkType' :: TypeCon -> TypeQ
mkType' ty = case ty of
  TApp f x -> appT (mkType' f) (mkType' x)

  TCon "Stored" -> promotedT 'A.Stored
  TCon "Array"  -> promotedT 'A.Array
  TCon "Struct" -> promotedT 'A.Struct
  TCon "Global" -> promotedT 'S.Global
  TCon "Stack"  -> fail "struct: not sure what to do with Stack yet"

  TCon con -> do
    mb <- lookupTypeName con
    case mb of
      Just n  -> conT n
      Nothing -> fail ("Unknown type: " ++ con)

  TNat n   -> litT (numTyLit n)

  TSym s   -> litT (strTyLit s)

flattenTApp :: TypeCon -> [TypeCon]
flattenTApp ty = case ty of
  TApp l r -> flattenTApp l ++ [r]
  _          -> [ty]

-- | Turn a parsed type into its AST representation.
mkTypeE :: Area -> ExpQ
mkTypeE ty =
  appE (varE 'A.ivoryArea)
       (sigE (conE 'Proxy)
             (appT (conT ''Proxy) (mkType ty)))

-- Note: The above is equivalent to:
--
--   [| ivoryArea (Proxy :: Proxy $(mkType ty)) |]
--
-- except I can't get TH to type-check that (maybe this will
-- work in GHC 7.8?)

-- String Types ---------------------------------------------------------------

-- | Create an Ivory type for a string with a fixed capacity.
-- mkStringDef :: String -> Integer -> Q [Dec]
-- mkStringDef ty_s len = do
--   let ty_n       = mkName ty_s
--   let struct_s   = "ivory_string_" ++ ty_s
--   let struct_n   = mkName struct_s
--   let struct_t   = [t| Struct $(litT (strTyLit struct_s)) |]
--   let data_s     = struct_s ++ "_data"
--   let data_n     = mkName data_s
--   let len_s      = struct_s ++ "_len"
--   let len_n      = mkName len_s

--   let data_t     = TApp (TApp (TCon "Array") (TNat len))
--                           (TApp (TCon "Stored") (TCon "Uint8"))
--   let data_f     = Field data_s data_t
--   let len_t      = TApp (TCon "Stored")
--                           (TApp (TCon "Ix") (TNat len))
--   let len_f      = Field len_s  len_t
--   let struct_def = StructDef struct_s [data_f, len_f]

--   d1 <- fromStruct struct_def
--   d2 <- sequence $
--     [ tySynD ty_n [] struct_t
--     , instanceD (cxt []) (appT (conT ''IvoryString) struct_t)
--       [ tySynInstD ''Capacity [struct_t] (litT (numTyLit len))
--       , valD (varP 'stringDataL)   (normalB (varE data_n)) []
--       , valD (varP 'stringLengthL) (normalB (varE len_n)) []
--       ]
--     ]

--   return (d1 ++ d2)

data TypeCon
  = TApp TypeCon TypeCon
  | TCon String
  | TNat Integer
  | TSym String
    deriving (Show)
