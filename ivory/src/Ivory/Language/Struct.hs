{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Ivory.Language.Struct where

import Ivory.Language.Area
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Type(IvoryExpr(..), IvoryVar(..))
import qualified Ivory.Language.Syntax as I

import GHC.TypeLits(Symbol)

-- Structs ---------------------------------------------------------------------

instance (IvoryStruct sym, ASymbol sym) => IvoryArea ('Struct sym) where
  ivoryArea _ = I.TyStruct (fromTypeSym (aSymbol :: SymbolType sym))

newtype StructDef (sym :: Symbol) = StructDef { getStructDef :: I.Struct }

type family StructName (a :: Area *) :: Symbol
type instance StructName ('Struct sym) = sym

class (IvoryArea ('Struct sym), ASymbol sym) => IvoryStruct (sym :: Symbol) where
  structDef :: StructDef sym

-- | Struct field labels.
newtype Label (sym :: Symbol) (field :: Area *) = Label { getLabel :: String }

instance Eq (Label (sym :: Symbol) (field :: Area *)) where
  l0 == l1 = getLabel l0 == getLabel l1

-- | Label indexing in a structure.
(~>) :: forall ref s sym field.
        ( IvoryStruct sym, IvoryRef ref
        , IvoryExpr (ref s ('Struct sym)), IvoryExpr (ref s field) )
     => ref s ('Struct sym) -> Label sym field -> ref s field
s ~> l = wrapExpr (I.ExpLabel ty (unwrapExpr s) (getLabel l))
  where
  ty = ivoryArea (Proxy :: Proxy ('Struct sym))
