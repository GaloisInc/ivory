{-# LANGUAGE ScopedTypeVariables #-}

-- | Ensure constraints are correct for testing for underflow/overflow for
-- signed ints.  Verified via SMT.

-- author : Lee Pike
-- date   : Jan 2014
-- license: BSD3

--import Data.SBV.Bridge.CVC4

import Prelude ()
import Prelude.Compat hiding (min,max,signum,abs)

import Data.SBV.Bridge.Z3


--------------------------------------------------------------------------------

type SI = SBV Integer

-- We'll consider int8_t in this example.
max = 127
min = -128

sz :: SI -> SBool
sz x = (x .>= min) &&& (x .<= max)

szConstraint :: SI -> SI -> SBool
szConstraint x y = sz x &&& sz y

negative :: SI -> SBool
negative x = x .< 0

positive :: SI -> SBool
positive x = x .> 0

zero :: SI -> SBool
zero x = x .== 0

abs :: SI -> SI
abs x =
  ite (signum x .== -1)
      (0-x)
      x

signum :: SI -> SI
signum x =
  ite (negative x)
      (-1)
    $ ite (positive x)
          1
          0

preconditionCheck :: (SI -> SI -> SBool) -> IO ()
preconditionCheck pred = do
  let signChk :: SI -> SI -> Predicate
      signChk a b = forSome_ $ \(x::SI) (y::SI)
                 -> pred x y &&& signum x .== a &&& signum y .== b &&& sz x &&& sz y
  res <- mapM sat [ signChk 0 0   , signChk 0 (-1)   , signChk 0 1
                  , signChk 1 0   , signChk 1 (-1)   , signChk 1 1
                  , signChk (-1) 0, signChk (-1) (-1), signChk (-1) 1
                  ]
  let fstChk a = forSome_ $ \(x::SI) -> pred x a &&& sz x
  let sndChk a = forSome_ $ \(x::SI) -> pred a x &&& sz x
  res0 <- mapM sat [ fstChk max, sndChk max
                   , fstChk min, sndChk min ]
  mapM_ print res
  mapM_ print res0

--------------------------------------------------------------------------------

addConstraint :: SI -> SI -> SBool
addConstraint x y =
      (x .>= 0 &&& y .>= 0 &&& max - x .>= y)
  ||| (x .<= 0 &&& y .<= 0 &&& min - x .<= y)
  ||| (signum x ./= signum y)

addCorrect :: SI -> SI -> SBool
addCorrect x y = (sz x &&& sz y &&& addConstraint x y) ==> sz (x + y)

runAdd = do
  prove addCorrect >>= print
  preconditionCheck addConstraint

--------------------------------------------------------------------------------

subConstraint :: SI -> SI -> SBool
subConstraint x y =
      (x .>= 0 &&& y .<= 0 &&& max + y .>= x)
  ||| (x .<= 0 &&& y .>= 0 &&& min + y .<= x)
  ||| (signum x .== signum y)

subCorrect :: SI -> SI -> SBool
subCorrect x y = (sz x &&& sz y &&& subConstraint x y) ==> sz (x - y)

runSub = do
  prove subCorrect >>= print
  preconditionCheck subConstraint

--------------------------------------------------------------------------------

minNegOne :: SI -> SI -> SBool
minNegOne a b = a ./= min ||| b ./= (-1)

-- CVC4 doesn't handle the non-linear constraints.  Z3 does, but seems to have a
-- spurious warning that there is no neg. x and y==0 that satisfies the
-- constraints.
multConstraint :: SI -> SI -> SBool
multConstraint x y =
      (minNegOne x y ||| minNegOne y x)
  &&& (    x .== 0
       ||| y .== 0
       ||| (max `sQuot` abs x .>= abs y &&& min `sQuot` abs x .<= abs y))

multCorrect :: SI -> SI -> SBool
multCorrect x y = (sz x &&& sz y &&& multConstraint x y) ==> sz (x * y)

runMult = do
  prove multCorrect >>= print
  preconditionCheck multConstraint

--------------------------------------------------------------------------------

divConstraint :: SI -> SI -> SBool
divConstraint x y = y ./= 0 &&& minNegOne x y

divCorrect :: SI -> SI -> SBool
divCorrect x y = (sz x &&& sz y &&& divConstraint x y) ==> sz (x `sQuot` y)

runDiv = do
  prove divCorrect >>= print
  preconditionCheck divConstraint

--------------------------------------------------------------------------------
