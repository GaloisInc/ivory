{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Ivory.Language.Proxy where

import GHC.TypeLits (natVal, symbolVal, Symbol, Nat, KnownNat, KnownSymbol)

data Proxy (a :: k) = Proxy

-- | Type proxies for * types.
type SProxy a = Proxy (a :: *)

type ANat    n = (KnownNat n)
type NatType n = Proxy n
aNat :: KnownNat n => Proxy n
aNat = Proxy

type ASymbol    s = (KnownSymbol s)
type SymbolType s = Proxy s
aSymbol :: KnownSymbol s => Proxy s
aSymbol = Proxy

-- | The string associated with a type-symbol.
fromTypeSym :: KnownSymbol sym => proxy (sym :: Symbol) -> String
fromTypeSym  = symbolVal

-- | The integer associated with a type-nat.
fromTypeNat :: KnownNat i => proxy (i :: Nat) -> Integer
fromTypeNat  = natVal
