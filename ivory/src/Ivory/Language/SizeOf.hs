{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Language.SizeOf where

import Ivory.Language.Area
import Ivory.Language.Proxy
import qualified Ivory.Language.Syntax as AST
import Ivory.Language.Type

class IvorySizeOf (t :: Area *) where

instance IvorySizeOf ('Struct sym) where
instance (ANat len, IvorySizeOf area) => IvorySizeOf ('Array len area) where
-- NOTE: (CArray area) is not safe to take sizeOf on.
instance IvoryType area => IvorySizeOf ('Stored area) where

-- | Get the size of an ivory type.
sizeOf :: (IvoryArea t, IvorySizeOf t, IvoryExpr a, Num a) => Proxy t -> a
sizeOf = wrapExpr . AST.ExpSizeOf . ivoryArea
