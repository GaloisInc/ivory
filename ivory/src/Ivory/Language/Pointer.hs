{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic pointer
module Ivory.Language.Pointer
  ( Constancy(Const, Mutable)
  , KnownConstancy
  , Nullability(Nullable, Valid)
  , Pointer(Pointer, getPointer)
  , nullPtr
  , pointerCast
  , pointerCastToConst
  , pointerCastToNullable
  , unsafePointerCast
  , withRef
  ) where

import           Ivory.Language.Area (Area, IvoryArea, ivoryArea)
import           Ivory.Language.IBool ((/=?), IvoryEq, ifte_)
import           Ivory.Language.Monad (Ivory)
import           Ivory.Language.Proxy (Proxy(..))
import           Ivory.Language.Scope (RefScope)
import           Ivory.Language.Syntax
                 (Expr(ExpLit), Literal(LitNull),
                  Type(TyConstPtr, TyConstRef, TyPtr, TyRef))
import           Ivory.Language.Type
                 (IvoryExpr, IvoryType, IvoryVar, ivoryType, unwrapExpr,
                  wrapExpr, wrapVar, wrapVarExpr)

-- * Nullability
data Nullability
  = Nullable -- ^ may be NULL
  | Valid -- ^ may not be NULL

class KnownNullability (n :: Nullability) where
  demoteNullability :: Proxy n -> Nullability

instance KnownNullability 'Nullable where
  demoteNullability _ = Nullable

instance KnownNullability 'Valid where
  demoteNullability _ = Valid

-- * Constancy
data Constancy
  = Const -- ^ data may not be modified
  | Mutable -- ^ data may be modified

class KnownConstancy (c :: Constancy) where
  demoteConstancy :: Proxy c -> Constancy

instance KnownConstancy 'Const where
  demoteConstancy _ = Const

instance KnownConstancy 'Mutable where
  demoteConstancy _ = Mutable

-- * Generic Pointer
data Pointer (n :: Nullability) (c :: Constancy) (s :: RefScope) (a :: Area *) = Pointer
  { getPointer :: Expr
  }

instance (KnownNullability n, KnownConstancy c, IvoryArea a) =>
         IvoryType (Pointer n c s a) where
  ivoryType _ =
    pointerTyCon
      (demoteNullability (Proxy :: Proxy n))
      (demoteConstancy (Proxy :: Proxy c))
      (ivoryArea (Proxy :: Proxy a))
    where
      pointerTyCon Nullable Const = TyConstPtr
      pointerTyCon Nullable Mutable = TyPtr
      pointerTyCon Valid Const = TyConstRef
      pointerTyCon Valid Mutable = TyRef

instance (KnownNullability n, KnownConstancy c, IvoryArea a) =>
         IvoryVar (Pointer n c s a) where
  wrapVar = wrapVarExpr
  unwrapExpr = getPointer

instance (KnownNullability n, KnownConstancy c, IvoryArea a) =>
         IvoryExpr (Pointer n c s a) where
  wrapExpr = Pointer

instance (KnownNullability n, KnownConstancy c, IvoryArea a) =>
         IvoryEq (Pointer n c s a)

-- * Special constants
nullPtr
  :: IvoryArea a
  => Pointer 'Nullable c s a
nullPtr = Pointer (ExpLit LitNull)

-- * Pointer conversions
-- | Safe pointer casting. Only defined if conversion is safe.
class PointerCast n1 c1 n2 c2 where
  pointerCast
    :: forall (s :: RefScope) (a :: Area *).
       IvoryArea a
    => Pointer n1 c1 s a -> Pointer n2 c2 s a

instance KnownNullability n =>
         PointerCast n 'Mutable n 'Const where
  pointerCast = unsafePointerCast

instance KnownConstancy c =>
         PointerCast 'Valid c 'Nullable c where
  pointerCast = unsafePointerCast

pointerCastToConst
  :: (KnownNullability n, IvoryArea a)
  => Pointer n 'Mutable s a -> Pointer n 'Const s a
pointerCastToConst = pointerCast

pointerCastToNullable
  :: (KnownConstancy c, IvoryArea a)
  => Pointer 'Valid c s a -> Pointer 'Nullable c s a
pointerCastToNullable = pointerCast

unsafePointerCast
  :: ( KnownNullability n1
     , KnownNullability n2
     , KnownConstancy c1
     , KnownConstancy c2
     , IvoryArea a
     )
  => Pointer n1 c1 s a -> Pointer n2 c2 s a
unsafePointerCast = wrapExpr . unwrapExpr

-- | Unwrap a pointer, and use it as a reference.
withRef
  :: (KnownConstancy c, IvoryArea a)
  => Pointer 'Nullable c s a
  -> (Pointer 'Valid c s a -> Ivory eff t)
  -> Ivory eff f
  -> Ivory eff ()
withRef ptr t = ifte_ (nullPtr /=? ptr) (t (unsafePointerCast ptr))
