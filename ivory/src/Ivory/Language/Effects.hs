{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ivory.Language.Effects
  ( Effects(..)
  , ProcEffects
  , NoEffects

  , ReturnEff(..)
  , GetReturn()
  , ClearReturn()

  , BreakEff(..)
  , CanBreak()
  , AllowBreak()
  , ClearBreak()

  , AllocEff(..)
  , GetAlloc()
  , ClearAlloc()
  ) where


--------------------------------------------------------------------------------
-- Effect Context

-- | The effect context for 'Ivory' operations.
data Effects = Effects ReturnEff BreakEff AllocEff

-- | Function return effect.
data ReturnEff = forall t. Returns t | NoReturn

-- | Loop break effect.
data BreakEff = Break | NoBreak

-- | Stack allocation effect.
data AllocEff = forall s. Scope s | NoAlloc


--------------------------------------------------------------------------------
-- Returns

-- | Retrieve any 'Return' effect present.
type family   GetReturn (effs :: Effects) :: ReturnEff
type instance GetReturn ('Effects r b a) = r

-- | Remove any 'Return' effects present.
type family   ClearReturn (effs :: Effects) :: Effects
type instance ClearReturn ('Effects r b a) = 'Effects 'NoReturn b a

--------------------------------------------------------------------------------
-- Breaks

-- | Test for the presence of the 'Break' effect.
class CanBreak (effs :: Effects)
instance CanBreak ('Effects r 'Break a)

-- | Add the 'Break' effect into an effect context.
type family   AllowBreak (effs :: Effects) :: Effects
type instance AllowBreak ('Effects r b a) = 'Effects r 'Break a

-- | Remove any 'Break' effect present.
type family   ClearBreak (effs :: Effects) :: Effects
type instance ClearBreak ('Effects r b a) = 'Effects r 'NoBreak a

--------------------------------------------------------------------------------
-- Allocs

-- | Retrieve the current allocation effect.
type family   GetAlloc (effs :: Effects) :: AllocEff
type instance GetAlloc ('Effects r b a) = a

-- | Remove any allocation effect currently present.
type family   ClearAlloc (effs :: Effects) :: Effects
type instance ClearAlloc ('Effects r b a) = 'Effects r b 'NoAlloc

--------------------------------------------------------------------------------
-- Helpers

type ProcEffects s t = 'Effects (Returns t) NoBreak (Scope s)
type NoEffects       = 'Effects 'NoReturn 'NoBreak 'NoAlloc

--------------------------------------------------------------------------------
