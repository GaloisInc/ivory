-- stack runghc
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module represents a complete solution through Advanced Exercise 2.
module Example where

import Prelude hiding (when,unless)
import Ivory.Language
import Ivory.Stdlib.Control (when)

-- define the example module, our compilation unit
exampleModule =
  package "example" $
    do incl ivoryMain
       defStruct (Proxy :: Proxy "Character")
       incl heal_char
       incl recover_mp
       incl clock
       incl srand
       incl rand
       incl printf_u16
       incl printf_void
       incl apply_damage
       incl heal_spell
       incl use_potion_embedded
       incl start_blocking
       incl stop_blocking

show_status :: Ref s ('Struct "Character") -> Ivory eff ()
show_status ref =
  do h <- deref (ref ~> hp)
     m <- deref (ref ~> mp)
     call_ printf_u16 "Character health: %d\n" h
     call_ printf_u16 "Character magic:  %d\n" m

ivoryMain :: Def ('[] ':-> Sint32)
ivoryMain  =
  proc "main" $
  body $
    do init_rng

       -- introduce a local variable for the exit code. If all goes
       -- well, it will stay at 0. If we lose all of our health
       -- points, it'll change before we return
       exit_code <- local (ival 0)

       char <- local $ istruct [ hp       .= ival 100
                               , max_hp   .= ival 250
                               , mp       .= ival 20
                               , max_mp   .= ival 100
                               , potions  .= ival 3
                               , statuses .= ival 0
                               ]

       call_ printf_void "Start of game\n"
       show_status char
       call_ start_blocking char

       for (20 :: Ix 21) $ \i -> do
         call_ printf_u16 "Start of turn %d\n" (safeCast i + 1)
         m  <- deref (char ~> mp)
         ps <- deref (char ~> potions)
         ss <- deref (char ~> statuses)

         -- drink a potion if necessary: either our magic points are
         -- low enough to use an entire potion, or we are silenced and
         -- need to remove that status effect
         maximum_mp <- deref (char ~> max_mp)
         mp_thresh  <- assign (maximum_mp - 25)
         when (ps >? 0 .&&
               (m <? mp_thresh .|| bitToBool (fromRep ss #. stat_silenced))
              ) $ do
           ifte_ (bitToBool (fromRep ss #. stat_blocking))
             (do call_ stop_blocking char
                 call_ use_potion_embedded char
                 call_ start_blocking char)
             (do call_ use_potion_embedded char)

         call_ apply_damage 20 20 char
         show_status char

         h <- deref (char ~> hp)

         -- if we've died, game over! Note: we could just return in
         -- this case, but Ivory will warn us about
         -- potentially-unreachable statements following
         when (h ==? 0) $ do
           store exit_code 1
           breakOut

         -- cast a healing spell if necessary: wait until we're below
         -- 75% of our maximum health points, so the spell doesn't go
         -- to waste
         maximum_hp <- deref (char ~> max_hp)
         hp_thresh  <- assign (maximum_hp - maximum_hp `iDiv` 4)
         when (h <? hp_thresh) $ do
           call_ heal_spell char

       -- indentation shift marks the end of the `for` loop

       ec <- deref exit_code
       ret ec

[ivory|

struct Character
  { uint16_t hp
  ; uint16_t max_hp
  ; uint16_t mp
  ; uint16_t max_mp
  ; uint16_t potions
  ; uint8_t  statuses
  }

|]

add_var :: Uint16 -> Ref s ('Stored Uint16) -> Ref s ('Stored Uint16) -> Ivory eff ()
add_var amount var max_var =
  do current <- deref var
     total   <- deref max_var
     new     <- assign (current + amount)
     store var ((new >? total) ? (total,new))

heal_char :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
heal_char  =
  proc "heal_char" $ \ amount ref ->
  body $
    add_var amount (ref ~> hp) (ref ~> max_hp)

recover_mp :: Def ('[Uint16, Ref s ('Struct "Character")] ':-> ())
recover_mp  =
  proc "recover_mp" $ \ amount ref ->
  body $
    add_var amount (ref ~> mp) (ref ~> max_mp)

[ivory|

import (time.h, clock)   uint64_t clock()
import (stdlib.h, srand) void srand(uint64_t x)
import (stdlib.h, rand)  uint16_t rand()
import (stdio.h, printf) void printf_u16(string x, uint16_t y)
import (stdio.h, printf) void printf_void(string x)

|]

init_rng :: Ivory eff ()
init_rng  =
  do val <- call clock
     call_ srand val

gen_rand :: Uint16 -> Ivory eff Uint16
gen_rand max_val =
  do val <- call rand
     return (val .% max_val)

apply_damage :: Def ('[Uint16, Uint16, Ref s ('Struct "Character")] ':-> ())
apply_damage  =
  proc "apply_damage" $ \ base max_additional ref ->
  body $
    do additional <- gen_rand max_additional
       health     <- deref (ref ~> hp)
       ss_byte    <- deref (ref ~> statuses)
       let ss = fromRep ss_byte
       bleeding   <- assign (bitToBool (ss #. stat_bleeding))
       blocking   <- assign (bitToBool (ss #. stat_blocking))
       add_bleed  <- assign (bleeding ? (additional,additional+5))
       damage     <- assign (base + (blocking ? (0,add_bleed)))
       store (ref ~> hp) ((damage >? health) ? (0,health - damage))
       when (damage ==? base + max_additional) $ do
         call_ printf_void "Character bleeding\n"
         withBitsRef (ref ~> statuses) $ setBit stat_bleeding
       d10 <- gen_rand 10
       when (d10 <? 1) $ do
         call_ printf_void "Character silenced\n"
         withBitsRef (ref ~> statuses) $ setBit stat_silenced

heal_spell :: Def ('[Ref s ('Struct "Character")] ':-> ())
heal_spell  =
  proc "heal_spell" $ \ ref ->
  body $
    do avail_mp <- deref (ref ~> mp)
       when (avail_mp >? 10) $
         do store (ref ~> mp) (avail_mp - 10)
            maximum_hp <- deref (ref ~> max_hp)
            val        <- assign (maximum_hp `iDiv` 4)
            call_ heal_char val ref
            withBitsRef (ref ~> statuses) $ clearBit stat_bleeding


-- concrete syntax doesn't support manipulating bit data yet, so we
-- have to rewrite this one in the embedded syntax for exercise 3
[ivory|

void use_potion(*struct Character c) {
  let ps = *c.potions;
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
}

|]

use_potion_embedded :: Def ('[Ref s ('Struct "Character")] ':-> ())
use_potion_embedded  =
  proc "use_potion_2" $ \ ref ->
  body $
    do ps <- deref (ref ~> potions)
       ss <- deref (ref ~> statuses)
       blocking <- assign (bitToBool (fromRep ss #. stat_blocking))
       when (ps >? 0 .&& iNot blocking) $ do
         store (ref ~> potions) (ps - 1)
         mp_val <- deref (ref ~> mp)
         mp_max <- deref (ref ~> max_mp)
         ifte_ (mp_val + 25 <? mp_max)
           (store (ref ~> mp) (mp_val + 25))
           (store (ref ~> mp) mp_max)

[ivory|
bitdata Statuses :: Bits 8 = statuses_data
  { stat_bleeding :: Bit
  , stat_blocking :: Bit
  , stat_silenced :: Bit
  , _             :: Bits 5
  }
|]

start_blocking :: Def ('[Ref s ('Struct "Character")] ':-> ())
start_blocking  =
  proc "start_blocking" $ \ ref ->
  body $
    do call_ printf_void "Character starting to block\n"
       withBitsRef (ref ~> statuses) $
         do setBit stat_blocking

stop_blocking :: Def ('[Ref s ('Struct "Character")] ':-> ())
stop_blocking  =
  proc "stop_blocking" $ \ ref ->
  body $
    do call_ printf_void "Character no longer blocking\n"
       withBitsRef (ref ~> statuses) $
         do clearBit stat_blocking
