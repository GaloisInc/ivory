{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Ivory.Language.Syntax.Concrete.QQ.StructQQ
  ( fromStruct
  ) where

import qualified Ivory.Language.Area  as A
import           Ivory.Language.Proxy
import           Ivory.Language.SizeOf
import qualified Ivory.Language.Struct as S
import qualified Ivory.Language.String as S

import qualified Ivory.Language.Syntax.AST  as AST
import qualified Ivory.Language.Syntax.Type as AST
import           Ivory.Language.Syntax.Concrete.ParseAST

import           Ivory.Language.Syntax.Concrete.QQ.Common
import           Ivory.Language.Syntax.Concrete.QQ.TypeQQ

import           Language.Haskell.TH hiding (Type)
import           Language.Haskell.TH.Quote()
import qualified Language.Haskell.TH as T

--------------------------------------------------------------------------------

fromStruct :: StructDef -> Q [Dec]
fromStruct def = case def of
  StructDef n fs -> do
    let sym = mkSym n
    sizeOfDef <- mkIvorySizeOf n fs
    sequence (mkIvoryStruct sym def ++ mkFields sym fs ++ sizeOfDef)
  AbstractDef n _hdr -> sequence (mkIvoryStruct (mkSym n) def)
  StringDef name len -> mkStringDef name len
  where
  mkSym = litT . strTyLit

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
    StructDef n fs    -> [| AST.Struct $(stringE n) $(listE (map mkField fs)) |]
    AbstractDef n hdr -> [| AST.Abstract $(stringE n) $(stringE hdr) |]
    StringDef _ _     -> error "unexpected string definition"

  mkField f =
    [| AST.Typed
         { AST.tType  = $(mkTypeE (fieldType f))
         , AST.tValue = $(stringE (fieldName f))
         }
    |]


-- IvorySizeOf -----------------------------------------------------------------

-- Create SizeOF instance for the struct.  Assumes that instances exist for the
-- fields.  Fails if this is not the case!

mkIvorySizeOf :: String -> [Field] -> Q [DecQ]
mkIvorySizeOf n fields = do
  let sym = litT (strTyLit n)
  szs <- mapM fieldSizeOfBytes fields
  if null szs then error $ "Cannot construct a struct with no fields: " ++ n
    else return (mkIvorySizeOfInst sym szs)

-- | Return a call to 'sizeOfBytes'.  May fail if no instance exists.
fieldSizeOfBytes :: Field -> Q T.Type
fieldSizeOfBytes field = mkType (fieldType field)

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
mkType a = runToQ (fromArea a) >>= (return . fst)

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
mkStringDef :: String -> Integer -> Q [Dec]
mkStringDef ty_s len = do
  let ty_n       = mkName ty_s
  let struct_s   = ivoryStringStructName ty_s
  let struct_t   = [t| A.Struct $(litT (strTyLit struct_s)) |]
  let data_s     = struct_s ++ "_data"
  let data_n     = mkName data_s
  let len_s      = struct_s ++ "_len"
  let len_n      = mkName len_s
  let data_f     = Field data_s (TyArray (TyStored (TyWord Word8)) len)
  let len_f      = Field len_s (TyStored (TyInt Int32))
  let struct_def = StructDef struct_s [data_f, len_f]

  d1 <- fromStruct struct_def
  d2 <- sequence
    [ tySynD ty_n [] struct_t
    , instanceD (cxt []) (appT (conT ''S.IvoryString) struct_t)
      [
#if __GLASGOW_HASKELL__ >= 708
        tySynInstD ''S.Capacity (tySynEqn [struct_t] (return $ szTy len))
#else
        tySynInstD ''S.Capacity [struct_t] (return $ szTy len)
#endif
      , valD (varP 'S.stringDataL)   (normalB (varE data_n)) []
      , valD (varP 'S.stringLengthL) (normalB (varE len_n)) []
      ]
    ]

  return (d1 ++ d2)

data TypeCon
  = TApp TypeCon TypeCon
  | TCon String
  | TNat Integer
  | TSym String
    deriving (Show)
