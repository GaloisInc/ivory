{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module DynArray where

import Data.Char (ord)
import Prelude hiding (sum)

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

-- | Define a dynamic-length constant look-up table.
table :: ConstMemArea (DynArray (Stored Uint8))
table = constDynArea "table"
          [ ival 0x10 , ival 0x30 , ival 0x80 , ival 0xde ]

-- | Module containing functions to generate.
cmodule :: Module
cmodule = package "DynArray" $ do
  incl dynArrayExample
  incl sum
  incl third
  incl toUpper
  defConstMemArea table

-- | Compile the test functions in this module.
runDynArrayExample :: IO ()
runDynArrayExample = runCompiler [cmodule] initialOpts { stdOut = True }

-- | Create a stack-allocated array given the length via a proxy.
localArr :: (SingI len, GetAlloc eff ~ Scope s, IvoryInit a)
         => proxy len
         -> [a]
         -> Ivory eff (Ref (Stack s) (Array len (Stored a)))
localArr _ xs = local (iarray (map ival xs))

-- | Example calling functions with dynamic array parameters.
dynArrayExample :: Def ('[] :-> Uint32)
dynArrayExample = proc "dynArrayExample" $ body $ do
  x     <- localArr (Proxy :: Proxy 5) [1, 2, 3, 4, 5]
  y     <- toDynArray x
  total <- call sum (constRef y)
  tot'  <- call sum (addrOf table)
  elt3  <- call third (constRef y)

  s     <- localArr (Proxy :: Proxy 12) (map (fromIntegral . ord) "Hello world!")
  dyn_s <- toDynArray s
  call_ toUpper dyn_s

  ret (total + safeCast elt3)

-- | This is pulled out into a function because we need to declare
-- the array length inside the success continuation of "withDynArray".
thirdAux :: ConstRef s (Array 3 (Stored Uint8)) -> ConstRef s (Stored Uint8)
thirdAux a = a ! 2

-- | Return the third element of a dynamic array, or 255 if the
-- array size is less than 3.
third :: Def ('[ConstRef s (DynArray (Stored Uint8))] :-> Uint8)
third = proc "third" $ \darr -> body $ do
  rv <- local (ival 0)
  withDynArray darr
    (\a _ -> store rv =<< deref (thirdAux a))
    (\_   -> store rv 255)
  ret =<< deref rv

-- | Return the 32-bit sum of an 8-bit array of any length.
sum :: Def ('[ConstRef s (DynArray (Stored Uint8))] :-> Uint32)
sum = proc "sum" $ \darr -> body $ do
  result <- local (ival 0)
  dynArrayMap darr $ \ref _ -> do
    x <- deref result
    y <- deref ref
    store result (x + safeCast y)
  ret =<< deref result

-- | Destructively change all lower case letters in a byte array
-- to upper case (assuming ASCII).
toUpper :: Def ('[Ref s (DynArray (Stored Uint8))] :-> ())
toUpper = proc "toUpper" $ \arr -> body $ do
  let ch_a = fromIntegral (ord 'a')
  let ch_z = fromIntegral (ord 'z')
  let d    = fromIntegral (ord 'a' - ord 'A')
  dynArrayMap arr $ \ref _ -> do
    ch <- deref ref
    ifte_ (ch >=? ch_a .&& ch <=? ch_z)
      (store ref (ch - d))
      (return ())
