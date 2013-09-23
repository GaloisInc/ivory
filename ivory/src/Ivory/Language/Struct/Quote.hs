{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Ivory.Language.Struct.Quote (
    ivory
  ) where

import Ivory.Language.Area
import Ivory.Language.Proxy
import Ivory.Language.Scope
import Ivory.Language.SizeOf
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
  P.TCon "DynArray"  -> promotedT 'DynArray
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
mkTypeE ty = case flattenTApp ty of

  [P.TCon "Struct", P.TSym name] ->
    [| AST.TyStruct $(stringE name) |]

  [P.TCon "Array", P.TNat len, e] ->
    [| AST.TyArr $(litE (integerL len)) $(mkTypeE e) |]

  [P.TCon "DynArray", e] ->
    [| AST.TyDynArray $(mkTypeE e) |]

  [P.TCon "Stored", s] ->
    [| ivoryType (Proxy :: SProxy $(mkType s)) |]

  _ ->
    fail ("mkTypeE: unhandled case: " ++ show ty)
