{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Language.Struct.Quote (
    ivory
  ) where

import Ivory.Language.Area
import Ivory.Language.Proxy
import Ivory.Language.Scope
import Ivory.Language.SizeOf
import Ivory.Language.String
import Ivory.Language.Struct
import Ivory.Language.Type
import qualified Ivory.Language.Struct.Parser as P
import qualified Ivory.Language.Syntax.AST  as AST
import qualified Ivory.Language.Syntax.Type as AST

import Data.Traversable (sequenceA)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec.Prim (parse,setPosition,getPosition)
import Text.Parsec.Pos (setSourceLine,setSourceColumn)


ivory :: QuasiQuoter
ivory  = QuasiQuoter
  { quoteExp  = const (fail "struct: unable to quote expressions")
  , quotePat  = const (fail "struct: unable to quote patterns")
  , quoteType = const (fail "struct: unable to quote types")
  , quoteDec  = quoteStructDefs
  }

parseDefs :: String -> Q [P.StructDef]
parseDefs str = do
  loc <- location
  case parse (body loc) (loc_filename loc) str of
    Right defs -> return defs
    Left err   -> fail (show err)
  where
  body loc = do
    pos <- getPosition
    let (line,col) = loc_start loc
    setPosition (setSourceLine (setSourceColumn pos col) line)
    P.parseStructDefs

quoteStructDefs :: String -> Q [Dec]
quoteStructDefs str = concat `fmap` (mapM mkDef =<< parseDefs str)

mkDef :: P.StructDef -> Q [Dec]
mkDef def = case def of
  P.StructDef n fs -> do
    let sym = mkSym n
    sizeOfDefs <- mkIvorySizeOf sym fs
    sequence (mkIvoryStruct sym def ++ sizeOfDefs ++ mkFields sym fs)

  P.AbstractDef n _hdr -> sequence (mkIvoryStruct (mkSym n) def)

  P.StringDef name len -> mkStringDef name len
  where
  mkSym n = litT (strTyLit n)


-- IvoryStruct -----------------------------------------------------------------

-- | Generate an @IvoryStruct@ instance.
mkIvoryStruct :: TypeQ -> P.StructDef -> [DecQ]
mkIvoryStruct sym def =
  [ instanceD (cxt []) (appT (conT ''IvoryStruct) sym) [mkStructDef def]
  ]

mkStructDef :: P.StructDef -> DecQ
mkStructDef def = funD 'structDef
  [ clause [] (normalB [| StructDef $astStruct |] ) []
  ]
  where
  astStruct = case def of
    P.StructDef n fs    -> [| AST.Struct $(stringE n)
                                         $(listE (map mkField fs)) |]
    P.AbstractDef n hdr -> [| AST.Abstract $(stringE n) $(stringE hdr) |]
    P.StringDef _ _     -> error "unexpected string definition"

  mkField f =
    [| AST.Typed
         { AST.tType  = $(mkTypeE (P.fieldType f))
         , AST.tValue = $(stringE (P.fieldName f))
         }
    |]


-- IvorySizeOf -----------------------------------------------------------------

mkIvorySizeOf :: TypeQ -> [P.Field] -> Q [DecQ]
mkIvorySizeOf sym fields = do
  mbs <- mapM fieldSizeOfBytes fields
  case sequenceA mbs of
    Just tys | not (null tys) -> return (mkIvorySizeOfInst sym tys)
    _                         -> return []

-- | Return a call to 'sizeOfBytes' if there's an instance for the type named in
-- the field.
fieldSizeOfBytes :: P.Field -> Q (Maybe Type)
fieldSizeOfBytes field = do
  ty          <- mkType (P.fieldType field)
  hasInstance <- isInstance ''IvorySizeOf [ty]
  if hasInstance
     then return (Just ty)
     else return  Nothing

mkIvorySizeOfInst :: TypeQ -> [Type] -> [DecQ]
mkIvorySizeOfInst sym tys =
  [ instanceD (cxt []) (appT (conT ''IvorySizeOf) struct) [mkSizeOfBytes tys]
  ]
  where
  struct = [t| Struct $sym |]

mkSizeOfBytes :: [Type] -> DecQ
mkSizeOfBytes tys = funD 'sizeOfBytes
  [ clause [wildP] (normalB (foldr1 add exprs)) []
  ]
  where
  exprs   = [ [| sizeOfBytes (Proxy :: AProxy $(return ty)) |] | ty <- tys ]
  add l r = [| $l + ($r :: Integer) |]

-- Field Labels ----------------------------------------------------------------

mkFields :: TypeQ -> [P.Field] -> [DecQ]
mkFields sym = concatMap (mkLabel sym)

mkLabel :: TypeQ -> P.Field -> [DecQ]
mkLabel sym f =
  [ sigD field [t| Label $sym $(mkType (P.fieldType f)) |]
  , funD field [clause [] (normalB [| Label $(stringE (P.fieldName f)) |]) []]
  ]
  where
  field = mkName (P.fieldName f)

mkType :: P.Type -> TypeQ
mkType ty = case ty of
  P.TApp f x -> appT (mkType f) (mkType x)

  P.TCon "Stored" -> promotedT 'Stored
  P.TCon "Array"  -> promotedT 'Array
  P.TCon "Struct" -> promotedT 'Struct
  P.TCon "Global" -> promotedT 'Global
  P.TCon "Stack"  -> fail "struct: not sure what to do with Stack yet"

  P.TCon con -> do
    mb <- lookupTypeName con
    case mb of
      Just n  -> conT n
      Nothing -> fail ("Unknown type: " ++ con)

  P.TNat n   -> litT (numTyLit n)

  P.TSym s   -> litT (strTyLit s)

flattenTApp :: P.Type -> [P.Type]
flattenTApp ty = case ty of
  P.TApp l r -> flattenTApp l ++ [r]
  _          -> [ty]

-- | Turn a parsed type into its AST representation.
mkTypeE :: P.Type -> ExpQ
mkTypeE ty =
  appE (varE 'ivoryArea)
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
  let struct_s   = "ivory_string_" ++ ty_s
  let struct_n   = mkName struct_s
  let struct_t   = [t| Struct $(litT (strTyLit struct_s)) |]
  let data_s     = struct_s ++ "_data"
  let data_n     = mkName data_s
  let len_s      = struct_s ++ "_len"
  let len_n      = mkName len_s

  let data_t     = P.TApp (P.TApp (P.TCon "Array") (P.TNat len))
                          (P.TApp (P.TCon "Stored") (P.TCon "Uint8"))
  let data_f     = P.Field data_s data_t
  let len_t      = P.TApp (P.TCon "Stored")
                          (P.TApp (P.TCon "Ix") (P.TNat len))
  let len_f      = P.Field len_s  len_t
  let struct_def = P.StructDef struct_s [data_f, len_f]

  d1 <- mkDef struct_def
  d2 <- sequence $
    [ tySynD ty_n [] struct_t
    , instanceD (cxt []) (appT (conT ''IvoryString) struct_t)
      [ tySynInstD ''Capacity [struct_t] (litT (numTyLit len))
      , valD (varP 'stringDataL)   (normalB (varE data_n)) []
      , valD (varP 'stringLengthL) (normalB (varE len_n)) []
      ]
    ]

  return (d1 ++ d2)
