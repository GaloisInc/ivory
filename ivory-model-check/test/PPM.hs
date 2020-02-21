{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module PPM where

import Ivory.Language
import Ivory.Stdlib

data PPMDecoder =
  PPMDecoder
    { ppmd_init       :: forall eff    . Ivory eff ()
    , ppmd_no_sample  :: forall eff    . ITime -> Ivory eff ()
    , ppmd_new_sample :: forall eff s  . Ref s PPMs -> ITime -> Ivory eff ()
    , ppmd_get_ui     :: forall eff cs . (GetAlloc eff ~ 'Scope cs)
         => Ivory eff (ConstRef ('Stack cs) ('Struct "userinput_result"))
    , ppmd_get_cl_req :: forall eff cs . (GetAlloc eff ~ 'Scope cs)
         => Ivory eff (ConstRef ('Stack cs) ('Struct "control_law_request"))
    }

-- taskPPMDecoder :: Task p PPMDecoder
-- taskPPMDecoder = do
--   fr <- fresh
--   let named n = "ppmdecoder_" ++ n ++ "_" ++ show fr
--   ppm_valid             <- taskLocal "ppm_valid"
--   ppm_last              <- taskLocal "ppm_last"
--   ppm_last_time         <- taskLocal "ppm_last_time"

--   modeswitch <- taskModeSwitch
--   armingmachine <- taskArmingMachine

--   let init_proc :: Def('[]:->())
--       init_proc = proc (named "init") $ body $ do
--         ms_init modeswitch


--       invalidate :: Ivory eff ()
--       invalidate = do
--           store ppm_valid false
--           ms_no_sample modeswitch
--           am_no_sample armingmachine

--       new_sample_proc :: Def('[Ref s PPMs, ITime ]:->())
--       new_sample_proc = proc (named "new_sample") $ \ppms time -> body $ do
--         all_good <- local (ival true)
--         arrayMap $ \ix -> when (ix <? useful_channels) $ do
--           ch <- deref (ppms ! ix)
--           unless (ch >=? minBound .&& ch <=? maxBound)
--                  (store all_good false)

--         s <- deref all_good
--         unless s $ invalidate
--         when   s $ do
--           arrayMap $ \ix -> when (ix <? useful_channels)
--             (deref (ppms ! ix) >>= store (ppm_last ! ix))
--           store ppm_last_time time
--           store ppm_valid true
--           ms_new_sample modeswitch ppms time
--           am_new_sample armingmachine ppms time

--       no_sample_proc :: Def('[ITime]:->())
--       no_sample_proc = proc (named "no_sample") $ \time -> body $ do
--         prev <- deref ppm_last_time
--         when ((time - prev) >? timeout_limit) invalidate

--       get_ui_proc :: Def('[Ref s (Struct "userinput_result")]:->())
--       get_ui_proc = proc (named "get_ui") $ \ui -> body $ do
--         valid <- deref ppm_valid
--         time <- deref ppm_last_time
--         ifte_ valid
--           (call_  ppm_decode_ui_proc ppm_last ui time)
--           (failsafe ui)

--       get_cl_req_proc :: Def('[Ref s (Struct "control_law_request")]:->())
--       get_cl_req_proc = proc (named "get_cl_req") $ \cl_req -> body $ do
--         ms_get_cl_req modeswitch cl_req
--         am_get_cl_req armingmachine cl_req

--   taskModuleDef $ do
--     incl init_proc
--     incl new_sample_proc
--     incl no_sample_proc
--     incl get_ui_proc
--     incl get_cl_req_proc
--     incl scale_proc
--     incl ppm_decode_ui_proc

--   return PPMDecoder
--     { ppmd_init       = call_ init_proc
--     , ppmd_new_sample = call_ new_sample_proc
--     , ppmd_no_sample  = call_ no_sample_proc
--     , ppmd_get_ui     = do
--         l <- local (istruct [])
--         call_ get_ui_proc l
--         return (constRef l)
--     , ppmd_get_cl_req = do
--         l <- local (istruct [])
--         call_ get_cl_req_proc l
--         return (constRef l)
--     }
--   where
--   useful_channels = 6
--   timeout_limit = fromIMilliseconds (150 :: Uint8)-- ms

useful_channels :: Ix 8
useful_channels = 6

ppm_valid_area :: MemArea ('Stored IBool)
ppm_valid_area = area "ppm_valid" Nothing
ppm_valid :: Ref 'Global ('Stored IBool)
ppm_valid = addrOf ppm_valid_area

ppm_last_area :: MemArea ('Array 8 ('Stored PPM))
ppm_last_area = area "ppm_last" Nothing
ppm_last :: Ref 'Global ('Array 8 ('Stored PPM))
ppm_last = addrOf ppm_last_area

ppm_last_time_area :: MemArea ('Stored ITime)
ppm_last_time_area = area "ppm_last_time" Nothing
ppm_last_time :: Ref 'Global ('Stored ITime)
ppm_last_time = addrOf ppm_last_time_area

invalidate :: Ivory eff ()
invalidate = do
    store ppm_valid false
    -- ms_no_sample modeswitch
    -- am_no_sample armingmachine

new_sample_proc :: Def('[Ref s PPMs, ITime ] :-> ())
new_sample_proc = proc "ppm_new_sample_proc" $ \ppms tm -> body $ do
  all_good <- local (ival true)
  arrayMap $ \ix -> when (ix <? useful_channels) $ do
    ch <- deref (ppms ! ix)
    unless (ch >=? minBound .&& ch <=? maxBound)
           (store all_good false)

  s <- deref all_good
  unless s $ invalidate
  when   s $ do
    arrayMap $ \ix -> when (ix <? useful_channels)
      (deref (ppms ! ix) >>= store (ppm_last ! ix))
    store ppm_last_time tm
    store ppm_valid true
    -- ms_new_sample modeswitch ppms time
    -- am_new_sample armingmachine ppms time

  -- get_ui_proc :: Def('[Ref s (Struct "userinput_result")]:->())
  -- get_ui_proc = proc (named "get_ui") $ \ui -> body $ do
  -- XXX: inlined
  ui <- local $ istruct []
  valid <- deref ppm_valid
  last_time <- deref ppm_last_time
  ifte_ valid
    (call_  ppm_decode_ui_proc ppm_last ui last_time)
    (failsafe ui)

ppmModule :: Module
ppmModule = package "ppm_userinput" $ do
  defStruct (Proxy :: Proxy "userinput_result")
  -- depend userInputTypeModule
  defMemArea ppm_valid_area
  defMemArea ppm_last_area
  defMemArea ppm_last_time_area
  incl new_sample_proc
  incl scale_proc
  incl ppm_decode_ui_proc

failsafe :: Ref s ('Struct "userinput_result") -> Ivory eff ()
failsafe ui = do
  store (ui ~> roll)      0
  store (ui ~> pitch)     0
  store (ui ~> throttle) (-1)
  store (ui ~> yaw)       0

scale_ppm_channel :: PPM -> Ivory eff IFloat
scale_ppm_channel input = call scale_proc ppmCenter 500 (-1.0) 1.0 input

valid_ppm :: PPM -> IBool
valid_ppm p = p >=? minBound .&& p <=? maxBound

scale_proc :: Def ('[PPM, Uint16, IFloat, IFloat, PPM] :-> IFloat)
scale_proc = proc "ppm_scale_proc" $ \center range outmin outmax input ->
  requires (    (range /=? 0)
            .&& valid_ppm input
            -- .&& (input >=? minBound)
            -- .&& (input <=? maxBound)
            .&& outmin <=? outmax
           )
  $ ensures (\r -> r >=? outmin .&& r <=? outmax)
  $ body $ do
    let centered = safeCast input - safeCast center
    let ranged = centered / safeCast range
    -- ranged <- assign $ centered / safeCast range
    ifte_ (ranged <? outmin)
      (ret outmin)
      (ifte_ (ranged >? outmax)
        (ret outmax)
        (ret ranged))

valid_ui :: IFloat -> IBool
valid_ui f = f >=? (-1.0) .&& f <=? (1.0)
  
ppm_decode_ui_proc :: Def ('[ Ref s0 ('Array 8 ('Stored PPM))
                            , Ref s1 ('Struct "userinput_result")
                            , ITime
                            ] :-> ())
ppm_decode_ui_proc = proc "ppm_decode_userinput" $ \ppms ui now ->
  requires (checkStored (ppms ! 0) valid_ppm) $
  requires (checkStored (ppms ! 1) valid_ppm) $
  requires (checkStored (ppms ! 2) valid_ppm) $
  requires (checkStored (ppms ! 3) valid_ppm) $
  ensures_ (checkStored (ui ~> roll) valid_ui) $
  ensures_ (checkStored (ui ~> pitch) valid_ui) $
  ensures_ (checkStored (ui ~> throttle) valid_ui) $
  ensures_ (checkStored (ui ~> yaw) valid_ui) $
  body $ do
  -- Scale 1000-2000 inputs to -1 to 1 inputs.
  let chtransform :: Ix 8
                  -> Label "userinput_result" ('Stored IFloat)
                  -> Ivory eff ()
      chtransform ix ofield = do
        p <- deref (ppms ! (ix :: Ix 8))
        v <- scale_ppm_channel p
        store (ui ~> ofield) v
  chtransform 0 roll
  chtransform 1 pitch
  chtransform 2 throttle
  chtransform 3 yaw
  store (ui ~> time) now
  retVoid

--------------------------------------------------------------------------------
-- From SMACCMPilot.Flight.Types.UserInput
--------------------------------------------------------------------------------

-- userInputTypeModule :: Module
-- userInputTypeModule = package "userinput_type" $ do
--   defStruct (Proxy :: Proxy "userinput_result")

[ivory|
struct userinput_result
  { throttle :: Stored IFloat
  ; roll     :: Stored IFloat
  ; pitch    :: Stored IFloat
  ; yaw      :: Stored IFloat
  ; time     :: Stored ITime
  ; source   :: Stored UISource
  }

|]

type ITime = Sint64

--------------------------------------------------------------------------------
-- PPM type

newtype PPM = PPM Uint16
  deriving ( IvoryType, IvoryOrd, IvoryVar, IvoryExpr
           , IvoryEq, IvoryStore, IvoryInit, Num)

type PPMs = 'Array 8 ('Stored PPM)

instance SafeCast PPM IFloat

instance SafeCast Uint16 PPM where
  safeCast = PPM

instance SafeCast PPM Uint16 where
  safeCast (PPM u) = u

instance Bounded PPM where
  minBound = PPM 800
  maxBound = PPM 2200

instance IvoryZeroVal PPM where
  izeroval = ival (PPM 1500)

ppmHigh, ppmLow, ppmCenter :: PPM
ppmHigh   = 1900
ppmLow    = 1100
ppmCenter = 1500

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- From SMACCMPilot.Flight.Types.UISource
--------------------------------------------------------------------------------

newtype UISource = UISource Uint32
  deriving ( IvoryType, IvoryVar, IvoryExpr, IvoryEq
           , IvoryStore, IvoryInit, IvoryZeroVal )

ppm :: UISource
ppm = UISource 0

mavlink :: UISource
mavlink = UISource 1
