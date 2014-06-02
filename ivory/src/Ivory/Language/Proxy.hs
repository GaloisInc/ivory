{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Ivory.Language.Proxy where

#if __GLASGOW_HASKELL__ >= 781
import GHC.TypeLits (natVal, symbolVal, Symbol, Nat, KnownNat, KnownSymbol)
#else
import GHC.TypeLits (Sing, SingI, sing, fromSing, Symbol, Nat)
#endif

data Proxy (a :: k) = Proxy

-- | Type proxies for * types.
type SProxy a = Proxy (a :: *)

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 781

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

--------------------------------------------------------------------------------
#else

type ANat n    = (SingI n)
type NatType n = Sing n
aNat :: SingI n => Sing n
aNat = sing

type ASymbol s    = (SingI s)
type SymbolType s = Sing s
aSymbol :: SingI s => Sing s
aSymbol = sing

-- | The string associated with a type-symbol.
fromTypeSym :: Sing (sym :: Symbol) -> String
fromTypeSym  = fromSing

-- | The integer associated with a type-nat.
fromTypeNat :: Sing (i :: Nat) -> Integer
fromTypeNat  = fromSing

#endif
--------------------------------------------------------------------------------
