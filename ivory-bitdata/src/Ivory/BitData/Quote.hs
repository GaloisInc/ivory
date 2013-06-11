{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
--
-- Quote.hs --- Bit data quasiquoter.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BitData.Quote (bitdata) where

import Control.Monad (MonadPlus, liftM, msum, when, unless, mzero)
import Data.Foldable (find, foldl')
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Traversable (mapAccumL)
import MonadLib (ChoiceT, findOne, lift)
import Text.Parsec (parse, setPosition, getPosition,
                    setSourceLine, setSourceColumn)
import Text.Parsec.String (Parser)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Ivory.Language ((.|), iShiftL, safeCast)
import qualified Ivory.Language as I
import Ivory.BitData.AST
import Ivory.BitData.Parser (parseBitLiteral, parseDefs)

import qualified Ivory.BitData.Bits    as B
import qualified Ivory.BitData.BitData as B

-- | Quasiquoter for defining Ivory bit value and bit data types.
--
-- Only the declaration and expression forms are implemented.
bitdata :: QuasiQuoter
bitdata = QuasiQuoter
       { quoteDec  = bitdataQuoteDec
       , quoteExp  = bitdataQuoteExp
       , quotePat  = error "quotePat not implemented"
       , quoteType = error "quoteType not implemented"
       }

-- | Run a parser on a string, setting the source position information
-- from the location provided by the TH "Q" monad.
qParse :: Parser a -> String -> Q a
qParse parser str = do
  loc <- location
  case parse (body loc) (loc_filename loc) str of
    Right defs -> return defs
    Left err   -> fail (show err)
  where
    body loc = do
      pos <- getPosition
      let (line, col) = loc_start loc
      setPosition (setSourceLine (setSourceColumn pos col) line)
      parser

-- | The 'concatMapM' function generalizes 'concatMap' to arbitrary
-- monads.
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

-- | Combine a list of items using a "MonadPlus" Monad.  This is used
-- to lift a list of items into a "ChoiceT m a".
anyOf :: MonadPlus m => [a] -> m a
anyOf = msum . map return

----------------------------------------------------------------------
-- Expression Quasiquoter

-- | Parse a bit data literal expression (see the parser module for a
-- description of the syntax).
bitdataQuoteExp :: String -> Q Exp
bitdataQuoteExp str = do
  lit <- qParse parseBitLiteral str
  case lit of
    BitLitKnown len val ->
      let tylen = litT (numTyLit (fromIntegral len)) in
      [| B.unsafeIntToBits (val :: Int) :: B.Bits $tylen |]
    BitLitUnknown val ->
      [| B.repToBits (fromIntegral (val :: Int)) |]

----------------------------------------------------------------------
-- Declaration Quasiquoter

-- | Declaration quasiquoter for "bits".
bitdataQuoteDec :: String -> Q [Dec]
bitdataQuoteDec str = concatMapM mkDef =<< parseDefsQ str

-- | Parse a set of bit data definitions from a string.
parseDefsQ :: String -> Q [Def]
parseDefsQ = qParse parseDefs

----------------------------------------------------------------------
-- AST Annotation

-- | Convert a parser type to a Template Haskell type.
convertType :: BitTy -> Q Type
convertType (TyCon s) = do
  m <- lookupTypeName s
  case m of
    Just ty -> return $ ConT ty
    Nothing -> fail $ "undefined type: " ++ s
convertType (TyNat n) = return $ LitT $ NumTyLit n
convertType (TyApp t1 t2) = do
  t1' <- convertType t1
  t2' <- convertType t2
  return $ AppT t1' t2'

-- | Look up a type and return its size in bits.
getTyBits :: Type -> ChoiceT Q Int
getTyBits ty =
  case ty of
    ConT name
      | name == ''B.Bit -> return 1
      | otherwise       -> tyInsts ''B.BitType ty >>= decBits
    AppT (ConT name) (LitT (NumTyLit n))
      | name == ''B.Bits -> return $ fromIntegral n
      | otherwise        -> mzero
    _ -> mzero
  where
    tyInsts name t = lift (reifyInstances name [t]) >>= anyOf
    decBits (TySynInstD _ _ t) = getTyBits t
    decBits _ = mzero

-- | Return the size in bits of a type that can appear in a bitdata
-- definition.  The allowed forms are "Bit", "Bits n", or a name
-- defined by "bitdata".
tyBits :: Type -> Q Int
tyBits ty = do
  r <- findOne (getTyBits ty)
  case r of
    Just x  -> return x
    Nothing -> fail "invalid bit value base type"

-- | A field definition annotated with its TH name, type, and bit
-- size.
data THField = THField
  { thFieldName :: Name
  , thFieldType :: Type
  , thFieldLen  :: Int
  } deriving Show

-- | Annotate a field definition.
annotateField :: Field -> Q THField
annotateField (Field n t) = do
  ty <- convertType t
  len <- tyBits ty
  return $ THField (mkName n) ty len

-- | A annotated layout item containing a bit literal or a field
-- definition, and the position of the item.
data THLayoutItem =
  THLayoutConst
    { thLayoutValue :: BitLiteral
    , thLayoutPos   :: Int }
  | THLayoutField
    { thLayoutField :: THField
    , thLayoutPos   :: Int }
  deriving Show

-- | A list of annotated layout items.
type THLayout = [THLayoutItem]

-- | Return the default layout given the total bit data size and a
-- list of fields.  If there are no fields, we'll initialize with a
-- zero literal.
defaultLayout :: Int -> [THField] -> THLayout
defaultLayout len [] = [THLayoutConst (BitLitKnown len 0) 0]
defaultLayout _ fs = snd $ mapAccumL go 0 fs
  where
    go pos f
      | thFieldName f == mkName "_"
      , len <- thFieldLen f
        = (pos + len, THLayoutConst (BitLitKnown len 0) pos)
      | otherwise
        = (pos + thFieldLen f, THLayoutField f pos)

-- | Annotate a layout by looking up names in a list of fields and
-- assigning field positions.  If the layout is empty, create a
-- default layout containing all the fields.
--
-- Note that we reverse the fields and layout, since they are in the
-- AST in MSB-first order.  We assign them positions starting from bit
-- 0.
annotateLayout :: Int -> Layout -> [THField] -> THLayout
annotateLayout len [] fs = defaultLayout len (reverse fs)
annotateLayout _ ls fs = snd $ mapAccumL go 0 (reverse ls)
  where
    go pos l =
      case l of
        LayoutConst lit@(BitLitKnown len _)
          -> (pos + len, THLayoutConst lit pos)
        LayoutField name
          | Just f <- find ((== mkName name) . thFieldName) fs
            -> (pos + thFieldLen f, THLayoutField f pos)
        _ -> error "invalid bitdata layout"

-- | Return the size, in bits, of a layout item, given a list of
-- fields with type information.
layoutItemSize :: [THField] -> LayoutItem -> Int
layoutItemSize _ (LayoutConst (BitLitKnown len _)) = len
layoutItemSize _ (LayoutConst (BitLitUnknown _)) = 0
layoutItemSize fs (LayoutField name) =
  case find ((== (mkName name)) . thFieldName) fs of
    Just field -> fromIntegral (thFieldLen field)
    Nothing    -> error "undefined field"

-- | Return the total size of a layout, not including fields with
-- unknown sizes.
layoutSize :: [THField] -> Layout -> Int
layoutSize fs ls = sum (map (layoutItemSize fs) ls)

-- | Return true if a layout item has an unknown size.
hasUnknownSize :: LayoutItem -> Bool
hasUnknownSize (LayoutConst (BitLitUnknown _)) = True
hasUnknownSize _ = False

-- | Update a layout item of unknown size to a known size.
updateSizeL :: Int -> LayoutItem -> LayoutItem
updateSizeL size l =
  case l of
    LayoutConst (BitLitUnknown x) -> LayoutConst (BitLitKnown size x)
    _ -> l

-- | Replace the first layout item with unknown size with the given
-- size.
updateFirstL :: Int -> Layout -> Layout
updateFirstL _ [] = []
updateFirstL size (l:ls)
  | hasUnknownSize l = updateSizeL size l : ls
  | otherwise        = l : updateFirstL size ls

-- | Update bit literals in a constructor' layout, given the size of
-- the bit data definition and the constructor.
--
-- There can be at most a single constant with an unknown size.  If
-- there is more than one, raise an error.  Otherwise, assign it a
-- size if there is one.
updateLiterals :: Int -> [THField] -> Layout -> Q Layout
updateLiterals defLen fs ls = do
  let slop = defLen - layoutSize fs ls
      ls'  = updateFirstL slop ls
  when (any hasUnknownSize ls') $
    fail "multiple unknown size bit fields"
  return ls'

-- | Fold a function over each layout item within a constructor.
foldLayout :: (b -> THLayoutItem -> b) -> b -> THConstr -> b
foldLayout f z c = foldl' f z (thConstrLayout c)

-- | Map a function over each layout item and its position within a
-- constructor.
mapLayout :: (THLayoutItem -> a) -> THConstr -> [a]
mapLayout f c = map f (thConstrLayout c)

-- | Return the size in bits of an annotated layout item.  A constant
-- layout item must be of known size.
thLayoutItemSize :: THLayoutItem -> Int
thLayoutItemSize (THLayoutConst (BitLitKnown len _) _) = len
thLayoutItemSize (THLayoutConst _ _) = error "invalid layout item"
thLayoutItemSize (THLayoutField f _) = thFieldLen f

-- | Return the size of an annotated layout definition.  All constant
-- values must have a known size.
thLayoutSize :: THLayout -> Int
thLayoutSize l = sum $ map thLayoutItemSize l

-- | A constructor definition annotated with TH information.
data THConstr = THConstr
  { thConstrName   :: Name
  , thConstrFields :: [THField]
  , thConstrLayout :: THLayout
  } deriving Show

-- | Return a list of field names defined in a constructor.
constrFieldNames :: THConstr -> [Name]
constrFieldNames c = map thFieldName (thConstrFields c)

-- | Annotate a constructor definition.
annotateConstr :: Int -> Constr -> Q THConstr
annotateConstr len (Constr n fs ls) = do
  fs' <- mapM annotateField fs
  ls' <- updateLiterals len fs' ls
  return $ THConstr (mkName n) fs' (annotateLayout len ls' fs')

-- | A bit data definition annotated with TH information.
data THDef = THDef
  { thDefName    :: Name
  , thDefType    :: Type
  , thDefConstrs :: [THConstr]
  , thDefLen     :: Int
  } deriving Show

-- | Annotate a bitdata definition.
annotateDef :: Def -> Q THDef
annotateDef (Def n t cs) = do
  ty  <- convertType t
  len <- tyBits ty
  cs' <- mapM (annotateConstr len) cs
  return $ THDef (mkName n) ty cs' len

----------------------------------------------------------------------
-- Annotated AST Validation

-- | Validate a bitdata definition.
checkDef :: THDef -> Q ()
checkDef def = do
  mapM_ (checkConstr def) (thDefConstrs def)

-- | Validate a bitdata constructor definition.  Make sure all the
-- layouts are valid for the data size.
checkConstr :: THDef -> THConstr -> Q ()
checkConstr def constr = do
  checkLayout def constr (thConstrLayout constr)

-- | Return the field names mentioned in a layout.
layoutFieldNames :: THLayout -> [Name]
layoutFieldNames = mapMaybe go
  where go (THLayoutField f _) = Just $ thFieldName f
        go _ = Nothing

-- | Validate a constructor layout.  We verify these properties:
--
-- * Each field that is not named "_" is mentioned exactly once in the
--   layout.
--
-- * The total size of the layout does not exceed the size of the
--   enclosing bit data definition.
checkLayout :: THDef -> THConstr -> THLayout -> Q ()
checkLayout def c l = do
  let cnames = filter (/= (mkName "_")) (constrFieldNames c)
      lnames = filter (/= (mkName "_")) (layoutFieldNames l)
  unless (sort cnames == sort lnames) $
    fail "layout does not mention each field exactly once"
  when (thLayoutSize l > thDefLen def) $
    fail "constructor layout is too large"

----------------------------------------------------------------------
-- Code Generation

-- | Process and convert a bit data definition to a Template Haskell
-- declaration to splice in.
mkDef :: Def -> Q [Dec]
mkDef d = do
  def <- annotateDef d
  checkDef def
  ds <- sequence $ concat
    [ mkDefNewtype def
    , mkDefInstance def
    , concatMap (mkConstr def) (thDefConstrs def)
    ]
  eqs <- mkIvoryEqInst def
  return (ds ++ eqs)

-- | Generate a newtype definition for a bit data definition.
mkDefNewtype :: THDef -> [DecQ]
mkDefNewtype def = [newtypeD (cxt []) name []
                    (normalC name [strictType notStrict (return ty)]) []]
  where
    name = thDefName def
    ty   = thDefType def

-- | Generate an instance of the "BitData" type class for a bit data
-- definition.
mkDefInstance :: THDef -> [DecQ]
mkDefInstance def = [instanceD (cxt []) instTy body]
  where
    name    = thDefName def
    baseTy  = thDefType def
    instTy  = [t| B.BitData $(conT (thDefName def)) |]
    body    = [tyDef, toFun, fromFun]
    tyDef   = tySynInstD ''B.BitType [conT name] (return baseTy)
    x       = mkName "x"
    toFun   = funD 'B.toBits [clause [conP name [varP x]]
                              (normalB (varE x)) []]
    fromFun = valD (varP 'B.fromBits) (normalB (conE name)) []

-- | Generate an instance of the "IvoryEq" type class for a bit data
-- definition.
mkIvoryEqInst :: THDef -> Q [Dec]
mkIvoryEqInst def = return []
  -- We cannot use Standalone Deriving here because it is not
  -- supported by TH (it will parse in a [d||] but not emit a Dec
  -- So, we must write instances for IvoryType, IvoryVar, IvoryExpr,
  -- and IvoryEq from baseTy to instTy by hand, which requires private
  -- primitives from the ivory package.
  -- We will implement this once bitdata and hw become part of the ivory
  -- package and trevor gets some time to figure out the exports... -pch
  where
    _baseTy  = thDefType def
    _instTy  = [t| I.IvoryEq $(conT (thDefName def)) |]

-- | Return a list of types for each field in a constructor.
constrFieldTypes :: THConstr -> [Type]
constrFieldTypes c = map thFieldType fields
  where fields = filter ((/= (mkName "_")) . thFieldName) (thConstrFields c)

-- | Create a Template Haskell function type for a bit data
-- constructor.
mkConstrType :: THDef -> THConstr -> Type
mkConstrType d c = foldr (AppT . AppT ArrowT) (ConT (thDefName d)) fields
  where fields = constrFieldTypes c

-- | Return the Template Haskell name for the "n"th argument to a bit
-- data constructor.
argName :: Int -> Name
argName n = mkName ("arg" ++ show n)

-- | Create a Template Haskell pattern list for a bit data
-- constructor.
mkConstrArgs :: THConstr -> [PatQ]
mkConstrArgs c = zipWith f [0..] names
  where names = filter (/= (mkName "_")) (constrFieldNames c)
        f x _ = varP (argName x)

-- | Create an expression for a layout item in the constructor.  We
-- maintain a count of the number of fields we've seen so that we can
-- reconstruct the argument name based on the field number.  This
-- isn't the most elegant solution but it will work for now.
--
-- This function can be folded over the layout with "foldLayout" given
-- an initial expression.
constrBodyExpr :: (Int, ExpQ) -> THLayoutItem -> (Int, ExpQ)
constrBodyExpr (n, expr) l =
  case l of
    -- XXX I would love to use an expression quasiquoter here, but
    -- there was a lot of arguing with the quasiquoter typechecker
    -- that seemed unnecessary, so I punted and built the expression
    -- by hand.
    THLayoutField _ pos ->
      (n + 1, infixApp expr (varE '(.|))
               (infixApp (appE (varE 'safeCast) (appE (varE 'B.toRep)
                                                 (varE (argName n))))
                          (varE 'iShiftL) (litE (integerL
                                                 (fromIntegral pos)))))
    THLayoutConst val pos
      | bitLitVal val /= 0 ->
        (n, infixApp expr (varE '(.|))
         (infixApp (litE (integerL (fromIntegral (bitLitVal val))))
          (varE 'iShiftL) (litE (integerL (fromIntegral pos)))))
      -- There's no need to OR in the zero value here:
      | otherwise -> (n, expr)

-- | Generate a value definition for a bit data constructor.
--
-- The generated constructor will take one argument for each field in
-- the constructor.  The body of the constructor will assemble the
-- arguments into a value using the layout.
mkConstr :: THDef -> THConstr -> [DecQ]
mkConstr def constr = [sig, fun] ++ mkConstrFields def constr
  where
    cname = thConstrName constr
    sig   = sigD cname (return (mkConstrType def constr))
    args  = mkConstrArgs constr
    zexpr = litE (integerL 0)
    expr  = snd (foldLayout constrBodyExpr (0, zexpr) constr)
    body  = normalB (appE (varE 'B.unsafeFromRep) expr)
    fun   = funD cname [clause args body []]

-- | Generate fields for a constructor using its layout.
mkConstrFields :: THDef -> THConstr -> [DecQ]
mkConstrFields def c = concat $ mapLayout (mkField def) c

-- | Generate a constructor field definition.
mkField :: THDef -> THLayoutItem -> [DecQ]
mkField def l@(THLayoutField f pos) =
  [ sigD name ty
  , valD (varP name) (normalB [| B.BitDataField $posE $lenE |]) []]
  where
    name = thFieldName f
    lenE = litE (integerL (fromIntegral (thFieldLen f)))
    posE = litE (integerL (fromIntegral pos))
    fty = return (thFieldType f)
    ty  = [t| B.BitDataField $(conT (thDefName def)) $fty |]
mkField _ _ = []
