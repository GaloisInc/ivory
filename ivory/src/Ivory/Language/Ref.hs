{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Ivory.Language.Ref where

import Ivory.Language.IChar (IChar)
import Ivory.Language.Sint (Sint8,Sint16,Sint32,Sint64)
import Ivory.Language.Uint (Uint8,Uint16,Uint32,Uint64)

import Ivory.Language.Area
import Ivory.Language.Monad
import Ivory.Language.Proxy
import Ivory.Language.Scope
import Ivory.Language.IBool
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I


-- References ------------------------------------------------------------------

-- | A non-null pointer to a memory area.
newtype Ref (s :: RefScope) (a :: Area *) = Ref { getRef :: I.Expr }

instance IvoryArea area => IvoryType (Ref s area) where
  ivoryType _ = I.TyRef (ivoryArea (Proxy :: Proxy area))

instance IvoryArea area => IvoryVar (Ref s area) where
  wrapVar    = wrapVarExpr
  unwrapExpr = getRef

instance IvoryArea area => IvoryExpr (Ref s area) where
  wrapExpr = Ref

-- Constant References ---------------------------------------------------------

-- | Turn a reference into a constant reference.
constRef :: IvoryArea area => Ref s area -> ConstRef s area
constRef  = wrapExpr . unwrapExpr

newtype ConstRef (s ::  RefScope) (a :: Area *) = ConstRef
  { getConstRef :: I.Expr
  }

instance IvoryArea area => IvoryType (ConstRef s area) where
  ivoryType _ = I.TyConstRef (ivoryArea (Proxy :: Proxy area))

instance IvoryArea area => IvoryVar (ConstRef s area) where
  wrapVar    = wrapVarExpr
  unwrapExpr = getConstRef

instance IvoryArea area => IvoryExpr (ConstRef s area) where
  wrapExpr = ConstRef

-- Dereferencing ---------------------------------------------------------------

class IvoryRef (ref ::  RefScope -> Area * -> *) where
  unwrapRef :: IvoryVar a => ref s ('Stored a) -> I.Expr

instance IvoryRef Ref where
  unwrapRef = unwrapExpr

instance IvoryRef ConstRef where
  unwrapRef = unwrapExpr

-- | Dereferenceing.
deref :: forall eff ref s a.
         (IvoryStore a, IvoryVar a, IvoryVar (ref s ('Stored a)), IvoryRef ref)
      => ref s ('Stored a) -> Ivory eff a
deref ref = do
  r <- freshVar "deref"
  emit (I.Deref (ivoryType (Proxy :: Proxy a)) r (unwrapRef ref))
  return (wrapVar r)

-- Copying ---------------------------------------------------------------------

-- | Memory copy.  Emits an assertion that the two references are unequal.
refCopy :: forall eff sTo ref sFrom a.
     ( IvoryRef ref, IvoryVar (Ref sTo a), IvoryVar (ref sFrom a), IvoryArea a)
  => Ref sTo a -> ref sFrom a -> Ivory eff ()
refCopy destRef srcRef =
  emit (I.RefCopy (ivoryArea (Proxy :: Proxy a))
       (unwrapExpr destRef) (unwrapExpr srcRef))

-- Storing ---------------------------------------------------------------------

store :: forall eff s a. IvoryStore a => Ref s ('Stored a) -> a -> Ivory eff ()
store ref a = emit (I.Store ty (unwrapExpr ref) (unwrapExpr a))
  where
  ty = ivoryType (Proxy :: Proxy a)

-- | Things that can be safely stored in references.
class IvoryVar a => IvoryStore a where

-- simple types
instance IvoryStore IBool
instance IvoryStore IChar
instance IvoryStore Uint8
instance IvoryStore Uint16
instance IvoryStore Uint32
instance IvoryStore Uint64
instance IvoryStore Sint8
instance IvoryStore Sint16
instance IvoryStore Sint32
instance IvoryStore Sint64
