{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Ivory.Language.Syntax.Concrete.QQ.StructQQ
  ( fromStruct
  ) where

import Prelude ()
import Prelude.Compat

import qualified Ivory.Language.Area  as A
import           Ivory.Language.Proxy
import qualified Ivory.Language.Struct as S
import qualified Ivory.Language.String as S

import qualified Ivory.Language.Syntax.AST  as AST
import qualified Ivory.Language.Syntax.Type as AST
import           Ivory.Language.Syntax.Concrete.ParseAST

import           Ivory.Language.Syntax.Concrete.QQ.Common
import           Ivory.Language.Syntax.Concrete.QQ.TypeQQ

import           Language.Haskell.TH hiding (Type)
import           Language.Haskell.TH.Quote()

--------------------------------------------------------------------------------

fromStruct :: StructDef -> Q [Dec]
fromStruct def = case def of
#if __GLASGOW_HASKELL__ >= 709
  StructDef n fs srcloc -> do
    let sym = mkSym n
    defs <- sequence (mkIvoryStruct sym def ++ mkFields sym fs)
    ln <- lnPragma srcloc
    return (ln ++ defs)
  StringDef name len _srcloc -> mkStringDef name len
  AbstractDef n _hdr _srcloc -> sequence (mkIvoryStruct (mkSym n) def)
#else
  StructDef n fs _srcloc -> do
    let sym = mkSym n
    defs <- sequence (mkIvoryStruct sym def ++ mkFields sym fs)
    return defs
  StringDef name len _srcloc -> mkStringDef name len
  AbstractDef n _hdr _srcloc -> sequence (mkIvoryStruct (mkSym n) def)
#endif
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
    StructDef n fs _    -> [| AST.Struct $(stringE n) $(listE (map mkField fs)) |]
    AbstractDef n hdr _ -> [| AST.Abstract $(stringE n) $(stringE hdr) |]
    StringDef _ _ _     -> error "unexpected string definition"

  mkField f =
    [| AST.Typed
         { AST.tType  = $(mkTypeE (fieldType f))
         , AST.tValue = $(stringE (fieldName f))
         }
    |]

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

mkType :: Type -> TypeQ
mkType area = do
  ty <- runToQ $ fromType $ maybeAddStored area
  return (fst ty)

-- | Turn a parsed type into its AST representation.
mkTypeE :: Type -> ExpQ
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
  let struct_t   = [t| 'A.Struct $(litT (strTyLit struct_s)) |]
  let data_s     = struct_s ++ "_data"
  let data_n     = mkName data_s
  let len_s      = struct_s ++ "_len"
  let len_n      = mkName len_s
  let data_f     = Field data_s (TyArray (TyStored (TyWord Word8)) len) mempty
  let len_f      = Field len_s (TyStored (TyInt Int32)) mempty
  let struct_def = StructDef struct_s [data_f, len_f] mempty

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
