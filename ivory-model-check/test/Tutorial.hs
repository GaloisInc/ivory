{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tutorial where

import Ivory.Language
import Ivory.Language.Cond

[ivory|

struct Character
  { hp     :: Stored Uint16
  ; max_hp :: Stored Uint16
  ; mp     :: Stored Uint16
  ; max_mp :: Stored Uint16
  ; potions:: Stored Uint8
  }

void use_potion(*struct Character c) {
  let ps = *c.potions;
  assume ps > 0;

  if (ps > 0) {
    -- decrement the number of available potions
    store c.potions as (ps - 1);

    -- increase mp up to the maximum
    let mp_val = *c.mp;
    let mp_max = *c.max_mp;
    if ((mp_val + 25) < mp_max) {
      store c.mp as (mp_val + 25);
    } else {
      store c.mp as mp_max;
    }
  } else {}
  let ps' = *c.potions;
  assert ps > ps';
}
|]

mypackage :: Module
mypackage = package "mypackage" $ do
  defStruct (Proxy :: Proxy "Character")
  incl use_potion

potions_available
  :: (IvoryExpr (ref s ('Struct "Character")),
      IvoryExpr (ref s ('Stored Uint8)), IvoryRef ref) =>
     ref s ('Struct "Character") -> Cond
potions_available ref =
  checkStored (ref ~> potions) $ \ potions_val ->
    1 <=? potions_val

check_potion_use :: Def ('[Ref s ('Struct "Character")] ':-> ())
check_potion_use =
  proc "check_potion_use" $ \ ref ->
  requires (potions_available ref) $
  body $ do
    old_potions <- deref (ref ~> potions)
    call_ use_potion ref
    new_potions <- deref (ref ~> potions)
    assert (old_potions >? new_potions)
