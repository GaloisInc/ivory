{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Language.Area where

import Ivory.Language.Proxy
import Ivory.Language.Type
import qualified Ivory.Language.Syntax as I

import GHC.TypeLits (Nat,Symbol,SingI,Sing,sing)


-- Memory Areas ----------------------------------------------------------------

-- | Type proxies for @Area@s.
type AProxy a = Proxy (a :: Area)

-- | The kind of memory-area types.
data Area
  = Struct Symbol
  | Array Nat Area
  | CArray Area
  | forall a. Stored a
    -- ^ This is lifting for a *-kinded type

instance (SingI len, IvoryType area) => IvoryType (Array len area) where
  ivoryType _ = I.TyArr len area
    where
    len  = fromInteger (fromTypeNat (sing :: Sing len))
    area = ivoryType (Proxy :: Proxy area)

instance IvoryType a => IvoryType (Stored a) where
  ivoryType _ = ivoryType (Proxy :: Proxy a)
