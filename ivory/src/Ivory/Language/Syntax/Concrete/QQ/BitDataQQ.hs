{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE TemplateHaskell            #-}

--
-- Quote.hs --- Bit data quasiquoter.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.Language.Syntax.Concrete.QQ.BitDataQQ (fromBitData) where

import           Control.Monad                            (MonadPlus, join,
                                                           msum, mzero, unless,
                                                           when)
import           Data.Foldable                            (find, foldl')
import           Data.List                                (sort)
import           Data.Maybe                               (catMaybes, isJust,
                                                           mapMaybe)
import           Data.Traversable                         (mapAccumL)
import           MonadLib                                 (ChoiceT, findOne,
                                                           lift)

import           Language.Haskell.TH                      hiding (Exp, Type)
import qualified Language.Haskell.TH                      as TH

import qualified Ivory.Language.Bits                      as I
import qualified Ivory.Language.Cast                      as I
import qualified Ivory.Language.IBool                     as I
import qualified Ivory.Language.Init                      as I
import qualified Ivory.Language.Ref                       as I
import           Ivory.Language.Syntax.Concrete.ParseAST  hiding (tyDef)
import qualified Ivory.Language.Type                      as I

import qualified Ivory.Language.BitData.Array             as B
import qualified Ivory.Language.BitData.BitData           as B
import qualified Ivory.Language.BitData.Bits              as B
#if __GLASGOW_HASKELL__ >= 709
import           Ivory.Language.Syntax.Concrete.QQ.Common
#endif
import           Ivory.Language.Syntax.Concrete.Location
import           Ivory.Language.Syntax.Concrete.QQ.TypeQQ

----------------------------------------------------------------------

-- | Combine a list of items using a "MonadPlus" Monad.  This is used
-- to lift a list of items into a "ChoiceT m a".
anyOf :: MonadPlus m => [a] -> m a
anyOf = msum . map return

----------------------------------------------------------------------
-- AST Annotation

-- | Convert a parser type to a Template Haskell type.
convertType :: BitTy -> Q TH.Type
convertType Bit = getType "Bit"
convertType (Bits n) =
  let b = getType "Bits" in
  appT b (return $ szTy n)
convertType (BitArray n t) = do
  appT (appT (getType "BitArray") (return $ szTy n)) (convertType t)
convertType (BitTySynonym s) = getType s
convertType (LocBitTy b) = convertType (unLoc b)

getType :: String -> Q TH.Type
getType s = do
  m <- lookupTypeName s
  case m of
    Just ty -> return $ ConT ty
    Nothing -> fail $ "undefined type: " ++ s

-- | Look up a type and return its size in bits.
getTyBits :: TH.Type -> ChoiceT Q Integer
getTyBits ty =
  case ty of
    ConT name
      | name == ''B.Bit -> return 1
      | otherwise       -> tyInsts ''B.BitType ty >>= decBits
    AppT (AppT (ConT name) (LitT (NumTyLit n))) ty2
      | name == ''B.BitArray -> do
         m <- lift $ tyBits ty2
         return (fromIntegral n * m)
    AppT (ConT name) (LitT (NumTyLit n))
      | name == ''B.Bits -> return $ fromIntegral n
      | otherwise        -> mzero
    _ -> mzero
  where
    tyInsts name t = lift (reifyInstances name [t]) >>= anyOf
#if __GLASGOW_HASKELL__ >= 708
    decBits (TySynInstD _ (TySynEqn _ t)) = getTyBits t
#else
    decBits (TySynInstD _ _ t) = getTyBits t
#endif
    decBits _ = mzero

-- | Return the size in bits of a type that can appear in a bitdata
-- definition.  The allowed forms are "Bit", "Bits n", "BitArray n t"
-- where "t" is a valid bit data type, or a name defined by "bitdata".
tyBits :: TH.Type -> Q Integer
tyBits ty = do
  r <- findOne (getTyBits ty)
  case r of
    Just x  -> return x
    Nothing -> fail $ "invalid bit value base type: " ++ show ty

-- | A field definition annotated with its TH name, type, and bit
-- size.
data THField = THField
  { thFieldName :: Maybe Name
  , thFieldType :: TH.Type
  , thFieldLen  :: Integer
  } deriving Show

-- | Annotate a field definition.
annotateField :: BitField -> Q THField
annotateField (BitField mn t _) = do
  ty <- convertType t
  len <- tyBits ty
  return $ THField (fmap mkName mn) ty len

-- | A annotated layout item containing a bit literal or a field
-- definition, and the position of the item.
data THLayoutItem =
    THLayoutConst BitLiteral Integer
  | THLayoutField THField Integer
  deriving Show

-- | A list of annotated layout items.
type THLayout = [THLayoutItem]

-- | Return the default layout given the total bit data size and a
-- list of fields.  If there are no fields, we'll initialize with a
-- zero literal.
defaultLayout :: Integer -> [THField] -> THLayout
defaultLayout len [] = [THLayoutConst (BitLitKnown len 0) 0]
defaultLayout _ fs = snd $ mapAccumL go 0 fs
  where
  go pos f =
    let len = thFieldLen f in
    case thFieldName f of
      Nothing -> (pos + len, THLayoutConst (BitLitKnown len 0) pos)
      Just _  -> (pos + thFieldLen f, THLayoutField f pos)

-- | Annotate a layout by looking up names in a list of fields and
-- assigning field positions.  If the layout is empty, create a
-- default layout containing all the fields.
--
-- Note that we reverse the fields and layout, since they are in the
-- AST in MSB-first order.  We assign them positions starting from bit
-- 0.
annotateLayout :: Integer -> [LayoutItem] -> [THField] -> THLayout
annotateLayout len [] fs = defaultLayout len (reverse fs)
annotateLayout _ ls fs = snd $ mapAccumL go 0 (reverse ls)
  where
    go pos l =
      case l of
        LayoutConst lit@(BitLitKnown len _)
          -> (pos + len, THLayoutConst lit pos)
        LayoutField name
          | Just f <- lookupField name fs
            -> (pos + thFieldLen f, THLayoutField f pos)
        _ -> error "invalid bitdata layout"

-- | Return the size, in bits, of a layout item, given a list of
-- fields with type information.
layoutItemSize :: [THField] -> LayoutItem -> Integer
layoutItemSize _ (LayoutConst (BitLitKnown len _)) = len
layoutItemSize _ (LayoutConst (BitLitUnknown _)) = 0
layoutItemSize fs (LayoutField name) =
  case lookupField name fs of
    Just field -> fromIntegral (thFieldLen field)
    Nothing    -> error "undefined field"

lookupField :: String -> [THField] -> Maybe THField
lookupField name fs = find getNm fs
  where
  getNm th = case thFieldName th of
               Just n  -> mkName name == n
               Nothing -> False

-- | Return the total size of a layout, not including fields with
-- unknown sizes.
layoutSize :: [THField] -> [LayoutItem] -> Integer
layoutSize fs ls = sum (map (layoutItemSize fs) ls)

-- | Return true if a layout item has an unknown size.
hasUnknownSize :: LayoutItem -> Bool
hasUnknownSize (LayoutConst (BitLitUnknown _)) = True
hasUnknownSize _ = False

-- | Update a layout item of unknown size to a known size.
updateSizeL :: Integer -> LayoutItem -> LayoutItem
updateSizeL size l =
  case l of
    LayoutConst (BitLitUnknown x) -> LayoutConst (BitLitKnown size x)
    _ -> l

-- | Replace the first layout item with unknown size with the given
-- size.
updateFirstL :: Integer -> [LayoutItem] -> [LayoutItem]
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
updateLiterals :: Integer -> [THField] -> [LayoutItem] -> Q [LayoutItem]
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
thLayoutItemSize :: THLayoutItem -> Integer
thLayoutItemSize (THLayoutConst (BitLitKnown len _) _) = len
thLayoutItemSize (THLayoutConst _ _) = error "invalid layout item"
thLayoutItemSize (THLayoutField f _) = thFieldLen f

-- | Return the size of an annotated layout definition.  All constant
-- values must have a known size.
thLayoutSize :: THLayout -> Integer
thLayoutSize l = sum $ map thLayoutItemSize l

-- | A constructor definition annotated with TH information.
data THConstr = THConstr
  { thConstrName   :: Name
  , thConstrFields :: [THField]
  , thConstrLayout :: THLayout
  } deriving Show

-- | Return a list of field names defined in a constructor.
constrFieldNames :: THConstr -> [Name]
constrFieldNames c = catMaybes $ map thFieldName (thConstrFields c)

-- | Annotate a constructor definition.
annotateConstr :: Integer -> Constr -> Q THConstr
annotateConstr len (Constr n fs ls _) = do
  fs' <- mapM annotateField fs
  ls' <- updateLiterals len fs' ls
  return $ THConstr (mkName n) fs' (annotateLayout len ls' fs')

-- | A bit data definition annotated with TH information.
data THDef = THDef
  { thDefName    :: Name
  , thDefType    :: TH.Type
  , thDefConstrs :: [THConstr]
  , thDefLen     :: Integer
  } deriving Show

-- | Annotate a bitdata definition.
annotateDef :: BitDataDef -> Q THDef
annotateDef (BitDataDef n t cs _) = do
  ty  <- convertType t
  len <- tyBits ty
  cs' <- mapM (annotateConstr len) cs
  return (THDef (mkName n) ty cs' len)

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
layoutFieldNames = mapMaybe (join . go)
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
fromBitData :: BitDataDef -> Q [Dec]
fromBitData d = do
  def <- annotateDef d
  checkDef def
  defs <- sequence $ concat
    [ mkDefNewtype def
    , mkDefInstance def
    , concatMap (mkConstr def) (thDefConstrs def)
    , mkArraySizeTypeInsts def
    ]
#if __GLASGOW_HASKELL__ >= 709
  ln <- lnPragma (bdLoc d)
  return (ln ++ defs)
#else
  return defs
#endif

-- | Generate a newtype definition for a bit data definition.
mkDefNewtype :: THDef -> [DecQ]
mkDefNewtype def =
#if __GLASGOW_HASKELL__ >= 800
  [newtypeD (cxt []) name []
   Nothing
   (normalC name
    [bangType (bang noSourceUnpackedness noSourceStrictness) (return ty)])
   (mapM conT
    [ ''I.IvoryType, ''I.IvoryVar, ''I.IvoryExpr , ''I.IvoryEq
    , ''I.IvoryInit, ''I.IvoryStore, ''I.IvoryZeroVal ])]
#else
  [newtypeD (cxt []) name []
   (normalC name [strictType notStrict (return ty)])
   [ ''I.IvoryType, ''I.IvoryVar, ''I.IvoryExpr , ''I.IvoryEq
   , ''I.IvoryInit, ''I.IvoryStore, ''I.IvoryZeroVal ]]
#endif
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
#if __GLASGOW_HASKELL__ >= 708
    tyDef   = return (TySynInstD ''B.BitType (TySynEqn [ConT name] baseTy))
#else
    tyDef   = return (TySynInstD ''B.BitType [ConT name] baseTy)
#endif
    x       = mkName "x"
    toFun   = funD 'B.toBits [clause [conP name [varP x]]
                              (normalB (varE x)) []]
    fromFun = valD (varP 'B.fromBits) (normalB (conE name)) []

-- | Generate instances of the "ArraySize" type family for any fields
-- with a bit array type.
mkArraySizeTypeInsts :: THDef -> [DecQ]
mkArraySizeTypeInsts def =
 concatMap (uncurry mkArraySizeTypeInst)
           (mapMaybe getArrayType
                     (concatMap constrFieldTypes (thDefConstrs def)))
  where
  -- | Deconstruct a bit array type into its size and base type.
  -- Returns "Nothing" if the type is not a bit array type.
  getArrayType :: TH.Type -> Maybe (Integer, TH.Type)
  getArrayType (AppT (AppT (ConT name) (LitT (NumTyLit n))) ty)
    | name == ''B.BitArray = Just (fromIntegral n, ty)
  getArrayType _ = Nothing

-- | Generate an instance of the "ArraySize" type family for a bit
-- array type.  We don't check to see if the instance already exists
-- because duplicates are allowed by the overlapping rules when the
-- result type is the same.
mkArraySizeTypeInst :: Integer -> TH.Type -> [DecQ]
mkArraySizeTypeInst n ty =
#if __GLASGOW_HASKELL__ >= 708
  [tySynInstD ''B.ArraySize (tySynEqn args size)]
#else
  [tySynInstD ''B.ArraySize args size]
#endif
  where
    size = tyBits ty >>= litT . numTyLit . fromIntegral . (* n)
    args = [litT (numTyLit (fromIntegral n)), return ty]

-- | Return a list of types for each field in a constructor.
constrFieldTypes :: THConstr -> [TH.Type]
constrFieldTypes c = map thFieldType fields
  where fields = filter (isJust . thFieldName) (thConstrFields c)

-- | Create a Template Haskell function type for a bit data
-- constructor.
mkConstrType :: THDef -> THConstr -> TH.Type
mkConstrType d c = foldl (flip (AppT . AppT ArrowT)) (ConT (thDefName d)) fields
  where fields = constrFieldTypes c

-- | Return the Template Haskell name for the "n"th argument to a bit
-- data constructor.
argName :: Integer -> Name
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
constrBodyExpr :: (Integer, ExpQ) -> THLayoutItem -> (Integer, ExpQ)
constrBodyExpr (n, expr) l =
  case l of
    -- XXX I would love to use an expression quasiquoter here, but
    -- there was a lot of arguing with the quasiquoter typechecker
    -- that seemed unnecessary, so I punted and built the expression
    -- by hand.
    THLayoutField _ pos ->
      (n + 1, infixApp expr (varE '(I..|))
               (infixApp (appE (varE 'I.safeCast) (appE (varE 'B.toRep)
                                                 (varE (argName n))))
                          (varE 'I.iShiftL) (litE (integerL
                                                 (fromIntegral pos)))))
    THLayoutConst val pos
      | bitLitVal val /= 0 ->
        (n, infixApp expr (varE '(I..|))
         (infixApp (litE (integerL (fromIntegral (bitLitVal val))))
          (varE 'I.iShiftL) (litE (integerL (fromIntegral pos)))))
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
mkField def (THLayoutField f pos) =
  case thFieldName f of
    Nothing   -> []
    Just name ->
      [ sigD name ty
      , valD (varP name) (normalB [| B.BitDataField $posE $lenE $nameE |]) []]
      where
      nameE = litE (stringL (nameBase name))
      lenE  = litE (integerL (fromIntegral (thFieldLen f)))
      posE  = litE (integerL (fromIntegral pos))
      fty   = return (thFieldType f)
      ty    = [t| B.BitDataField $(conT (thDefName def)) $fty |]
mkField _ _ = []
