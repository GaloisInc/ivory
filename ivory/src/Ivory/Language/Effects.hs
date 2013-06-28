{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Ivory.Language.Effects
  ( --Effects()
    ProcEffects()
  , NoEffects()

  , ReturnEff(..)
  , Returns()
  , WithReturns()

  , BreakEff()
--  , Breaks()
  , WithBreaks()

  , AllocEff()
--  , Allocs()
  , WithAllocs()
  ) where

--------------------------------------------------------------------------------

-- | Effect kinds.
data ReturnEff t = Return t | NoReturn
data BreakEff    = Break    | NoBreak
data AllocEff    = Alloc    | NoAlloc

--------------------------------------------------------------------------------
-- Returns

type family   Returns (effs :: (ReturnEff *, BreakEff, AllocEff))
           :: ReturnEff *
type instance Returns '(Return t , b, a) = Return t
type instance Returns '(NoReturn , b, a) = NoReturn

type family   WithReturns (effs :: (ReturnEff *, BreakEff, AllocEff)) (t :: *)
           :: (ReturnEff *, BreakEff, AllocEff)
type instance WithReturns '(r t0, b, a) t1 = '(Return t1, b, a)

--------------------------------------------------------------------------------
-- Breaks

-- type family   Breaks (effs :: (ReturnEff *, BreakEff, AllocEff))
--            :: BreakEff
-- type instance Breaks '(r, Break  , a) = Break
-- type instance Breaks '(r, NoBreak, a) = NoBreak

type family   WithBreaks (effs :: (ReturnEff *, BreakEff, AllocEff))
          :: (ReturnEff *, BreakEff, AllocEff)
type instance WithBreaks '(r, b, a) = '(r, Break, a)

--------------------------------------------------------------------------------
-- Allocs

-- type family   Allocs (effs :: (ReturnEff *, BreakEff, AllocEff))
--            :: AllocEff
-- type instance Allocs '(r , b, Alloc)   = Alloc
-- type instance Allocs '(r , b, NoAlloc) = NoAlloc

type family   WithAllocs (effs :: (ReturnEff *, BreakEff, AllocEff))
          :: (ReturnEff *, BreakEff, AllocEff)
type instance WithAllocs '(r, b, a) = '(r, b, Alloc)

--------------------------------------------------------------------------------

-- -- | Wrap Effects so that they have kind *.
-- data Effects :: (ReturnEff *, BreakEff, AllocEff) -> * --where
--  Effects :: Effects '(r t, b, a)

--------------------------------------------------------------------------------

-- Helpers

type ProcEffects t = '(Return t, NoBreak, Alloc)
type NoEffects     = '(NoReturn, NoBreak, NoAlloc)

--------------------------------------------------------------------------------
