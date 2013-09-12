{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.Language.Ptr where

import Ivory.Language.Area
import Ivory.Language.Proxy
import Ivory.Language.Ref
import Ivory.Language.Scope
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I


-- Pointers --------------------------------------------------------------------

-- | Pointers (nullable references).
newtype Ptr (s :: RefScope) (a :: Area *) = Ptr { getPtr :: I.Expr }

instance IvoryArea area => IvoryType (Ptr s area) where
  ivoryType _ = I.TyPtr (ivoryArea (Proxy :: Proxy area))

instance IvoryArea area => IvoryVar (Ptr s area) where
  wrapVar    = wrapVarExpr
  unwrapExpr = getPtr

instance IvoryArea area => IvoryExpr (Ptr s area) where
  wrapExpr = Ptr

-- Only allow global pointers to be stored in structures.
instance IvoryArea a => IvoryStore (Ptr Global a)

nullPtr :: IvoryArea area => Ptr s area
nullPtr  = Ptr (I.ExpLit I.LitNull)

-- | Convert a reference to a pointer.  This direction is safe as we know that
-- the reference is a non-null pointer.
refToPtr :: IvoryArea area => Ref s area -> Ptr s area
refToPtr  = wrapExpr . unwrapExpr

-- XXX do not export
ptrToRef :: IvoryArea area => Ptr s area -> Ref s area
ptrToRef  = wrapExpr . unwrapExpr
