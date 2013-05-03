--
-- AST.hs --- HW quasiquoter AST.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BitData.AST where

-- | Basic type representation allowed in bit definitions.
data BitTy = TyCon String
           | TyNat Integer
           | TyApp BitTy BitTy
  deriving Show

-- | A bit integer literal with a known or unknown size.
data BitLiteral =
    BitLitKnown   { bitLitLen :: Int , bitLitVal :: Int }
  | BitLitUnknown { bitLitVal :: Int }
 deriving Show

-- | One element of a bit data constructor layout.
data LayoutItem = LayoutConst BitLiteral
                | LayoutField String
  deriving Show

-- | A constructor layout is a list of layout items.
type Layout = [LayoutItem]

-- | A "bitdata" definition.
data Def = Def
  { defName    :: String
  , defType    :: BitTy
  , defConstrs :: [Constr]
  }

-- | A constructor definition within a "bitdata".
data Constr = Constr
  { constrName   :: String
  , constrFields :: [Field]
  , constrLayout :: Layout
  }

-- | A record-like field defined within a "bitdata" constructor.
data Field = Field
  { fieldName :: String
  , fieldType :: BitTy
  }
