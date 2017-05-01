{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.Language.Ptr
  ( Ptr
  , nullPtr
  , ptrToRef
  , refToPtr
  , withRef
  ) where

import Ivory.Language.Area
import Ivory.Language.Pointer
       (Constancy(Mutable), Nullability(Nullable), Pointer,
        pointerCastToNullable, unsafePointerCast, nullPtr, withRef)
import Ivory.Language.Ref

-- Pointers --------------------------------------------------------------------

-- | Pointers (nullable references).
type Ptr = Pointer 'Nullable 'Mutable

-- | Convert valid to nullable pointer.
-- TODO deprecate in favor of 'pointerCastToNullable'
refToPtr :: IvoryArea area => Ref s area -> Ptr s area
refToPtr = pointerCastToNullable

-- | Convert nullable to valid pointer unsafely.
-- TODO deprecate in favor of 'unsafePointerCast'
ptrToRef :: IvoryArea area => Ptr s area -> Ref s area
ptrToRef = unsafePointerCast
