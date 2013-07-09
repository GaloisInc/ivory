{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module PID where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

[ivory|

struct PID
  { pid_mv  :: Stored IFloat
  ; pid_i   :: Stored IFloat
  ; pid_err :: Stored IFloat
  }

|]

type SP   = IFloat -- Set point
type PV   = IFloat -- Process (measured) value
type Time = IFloat

{-
void pid_update(struct PID * pid,
                sp_t sp,
                pv_t pv,
                timeinc_t dt
                )
{
  float err = sp - pv;
  float i   = pid->i + err*dt;
  float d   = (err - pid->err) / dt;
  pid->i    = ki*i;
  pid->mv   = kp*err + pid->i + kd*d;
  pid->err  = err;
  return;
}
-}

kp, ki, kd :: IFloat
kp = 1.0
ki = 0.1
kd = 0.1

pidUpdate :: Def ('[ Ref s (Struct "PID")
                   , SP
                   , PV
                   , Time ]
                  :-> IFloat)
pidUpdate = proc "pid_update" $
  \ pid sp pv dt ->
  -- These are made up requires/ensures for testing purposes.
    requires (checkStored (pid ~> pid_err) (\err -> err <? 1))
  $ ensures  (\res -> checkStored (pid ~> pid_err) (\err -> err <? res))
  $ body
  $ do
    err     <- assign (sp - pv)
    i       <- deref $ pid ~> pid_i
    i'      <- assign  $ ki * (i + err*dt)
    prevErr <- deref $ pid ~> pid_err
    d       <- assign  $ (err - prevErr) / dt
    store (pid ~> pid_i)   i'
    store (pid ~> pid_mv)  (kp*err + i' + kd*d)
    store (pid ~> pid_err) err
    ret err

runPID :: IO ()
runPID = runCompiler [cmodule] initialOpts { stdOut = True }

cmodule :: Module
cmodule = package "PID" $ do
  defStruct (Proxy :: Proxy "PID")
  incl pidUpdate
  incl alloc_test

alloc_test :: Def ('[] :-> IFloat)
alloc_test  = proc "alloc_test" $ body $ do
  pid <- local (istruct [pid_i .= ival 1])
  ret =<< deref (pid ~> pid_i)
