{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

#if MIN_VERSION_base(4,10,0)
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
#endif

module Ivory.Language.Ref
  ( ConstRef
  , IvoryRef
  , IvoryStore
  , Ref
  , constRef
  , deref
  , refCopy
  , store
  ) where

import Ivory.Language.IChar (IChar)
import Ivory.Language.Sint (Sint8,Sint16,Sint32,Sint64)
import Ivory.Language.Uint (Uint8,Uint16,Uint32,Uint64)

import Ivory.Language.Area
import Ivory.Language.IBool
import Ivory.Language.Monad
import Ivory.Language.Pointer
       (Constancy(Const, Mutable), KnownConstancy, Nullability(Nullable, Valid),
        Pointer, getPointer, pointerCastToConst)
import Ivory.Language.Proxy
import Ivory.Language.Scope
import qualified Ivory.Language.Syntax as I
import Ivory.Language.Type


-- References ------------------------------------------------------------------

-- | A non-null pointer to a memory area.
type Ref = Pointer 'Valid 'Mutable

-- Constant References ---------------------------------------------------------

-- | Turn a reference into a constant reference.
-- TODO deprecate in favor of 'pointerCastToConst'
constRef :: IvoryArea area => Ref s area -> ConstRef s area
constRef = pointerCastToConst

type ConstRef = Pointer 'Valid 'Const

-- Dereferencing ---------------------------------------------------------------

-- | TODO remove class, leave function only
class IvoryRef (ref :: RefScope -> Area * -> *) where
  unwrapRef
    :: IvoryVar a
    => ref s ('Stored a) -> I.Expr

instance IvoryRef (Pointer 'Valid c) where
  unwrapRef = getPointer

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
  emit
    (I.RefCopy
       (ivoryArea (Proxy :: Proxy a))
       (unwrapExpr destRef)
       (unwrapExpr srcRef))

-- Storing ---------------------------------------------------------------------

store :: forall eff s a. IvoryStore a => Ref s ('Stored a) -> a -> Ivory eff ()
store ref a = emit (I.Store ty (unwrapExpr ref) (unwrapExpr a))
  where
  ty = ivoryType (Proxy :: Proxy a)

-- | Things that can be safely stored in references.
class IvoryVar a => IvoryStore a

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

-- Only allow global nullable pointers to be stored in structures.
instance (KnownConstancy c, IvoryArea a, n ~ 'Nullable, s ~ 'Global) =>
         IvoryStore (Pointer n c s a)
