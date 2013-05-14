{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Language.Proxy where

import GHC.TypeLits (Sing,fromSing,Symbol,Nat)


data Proxy (a :: k) = Proxy

-- | Type proxies for * types.
type SProxy a = Proxy (a :: *)

-- | The string associated with a type-symbol.
fromTypeSym :: Sing (sym :: Symbol) -> String
fromTypeSym  = fromSing

-- | The integer associated with a type-nat.
fromTypeNat :: Sing (i :: Nat) -> Integer
fromTypeNat  = fromSing
