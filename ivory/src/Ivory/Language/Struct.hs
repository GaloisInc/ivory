{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Language.Struct where

import Ivory.Language.Area
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Type(IvoryExpr(..), IvoryVar(..))
import qualified Ivory.Language.Syntax as I

import GHC.TypeLits (SingI,sing,Sing,Symbol)


-- Structs ---------------------------------------------------------------------

instance (IvoryStruct sym, SingI sym) => IvoryArea (Struct sym) where
  ivoryArea _ = I.TyStruct (fromTypeSym (sing :: Sing sym))

newtype StructDef (sym :: Symbol) = StructDef { getStructDef :: I.Struct }

class (IvoryArea (Struct sym), SingI sym) => IvoryStruct (sym :: Symbol) where
  type StructName (a :: Area *) :: Symbol
  type StructName (Struct sym) = sym

  structDef :: StructDef sym

-- | Struct field labels.
newtype Label (sym :: Symbol) (field :: Area *) = Label { getLabel :: String }

instance Eq (Label (sym :: Symbol) (field :: Area *)) where
  l0 == l1 = getLabel l0 == getLabel l1

-- | Label indexing in a structure.
(~>) :: forall ref s sym field.
        ( IvoryStruct sym, IvoryRef ref
        , IvoryExpr (ref s (Struct sym)), IvoryExpr (ref s field) )
     => ref s (Struct sym) -> Label sym field -> ref s field
s ~> l = wrapExpr (I.ExpLabel ty (unwrapExpr s) (getLabel l))
  where
  ty = ivoryArea (Proxy :: Proxy (Struct sym))
