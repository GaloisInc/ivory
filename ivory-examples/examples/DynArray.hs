{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module DynArray where

import Data.Char (ord)
import Prelude hiding (sum)

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

ivalL :: IvoryInit a => [a] -> [Init (Stored a)]
ivalL = map ival

idynstring :: String -> Init (DynArray (Stored Uint8))
idynstring cs = idynarray (map (ival . fromIntegral . ord) cs)

-- DynArray Memory Areas -------------------------------------------------------

-- | A mutable DynArray area (a buffer).
buffer :: MemArea (DynArray (Stored Uint8))
buffer = area "buffer" (Just (idynarray (replicate 32 (ival 0))))

-- | A constant DynArray area (a look-up table).
table1 :: ConstMemArea (DynArray (Stored Uint8))
table1 = constArea "table1" (idynarray (ivalL [1, 2, 3, 4, 5]))

-- | A structure mapping integers to byte arrays.
[ivory|
  struct foo
    { foo_a :: Stored Uint8
    ; foo_b :: Array 32 (Stored Uint8)
    }
|]

-- | A constant table of structures.
table2 :: ConstMemArea (DynArray (Struct "foo"))
table2 = constArea "table2" $ idynarray
           [ istruct [ foo_a .= ival 1, foo_b .= iarray (ivalL [1, 2, 3]) ]
           , istruct [ foo_a .= ival 3, foo_b .= iarray (ivalL [4, 5, 6]) ]
           , istruct [ foo_a .= ival 5, foo_b .= iarray (ivalL [7, 8, 9]) ]
           ]

-- DynArray Test Functions -----------------------------------------------------

-- | Return the 32-bit sum of an 8-bit array of any length.
sum :: Def ('[ConstRef s (DynArray (Stored Uint8))] :-> Uint32)
sum = proc "sum" $ \darr -> body $ do
  result <- local (ival 0)
  dynArrayMap darr $ \ref _ -> do
    x <- deref result
    y <- deref ref
    store result (x + safeCast y)
  ret =<< deref result

-- | Destructively change all ASCII lower case letters in a byte
-- array to upper case.
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

-- | Binding to the "write" system call.
--
-- FIXME: I wish we could express that the types are "int" and "size_t".
c_write :: Def ('[ Sint32
                 , ConstRef s (CArray (Stored Uint8))
                 , Sint32 ] :-> Uint32)
c_write = importProc "write" "unistd.h"

-- | Write a byte array to the standard output.
write :: Def ('[ConstRef s (DynArray (Stored Uint8))] :-> ())
write = proc "do_write" $ \ref -> body $ do
  withDynArrayData ref $ \carr len -> do
    call_ c_write 0 carr len
  retVoid

-- DynArray Local Allocation Tests ---------------------------------------------

testLocal1 :: Def ('[] :-> ())
testLocal1 = proc "testLocal1" $ body $ do
  s <- local (idynstring "Hello, world!\n")
  call_ toUpper s
  call_ write (constRef s)
  s2 <- local (idynstring "That's all folks!\n")
  call_ write (constRef s2)
  retVoid

-- Module ----------------------------------------------------------------------

-- | Run all tests in this module.
main :: Def ('[] :-> ())
main = proc "main" $ body $ do
  call_ testLocal1
  retVoid

-- | Compile the test functions in this module.
runDynArrayExample :: IO ()
runDynArrayExample = runCompiler [cmodule] initialOpts { stdOut = True }

-- | Module containing functions to generate.
cmodule :: Module
cmodule = package "DynArray" $ do
  incl sum
  incl toUpper
  incl c_write
  incl write
  incl testLocal1
  incl main
  defStruct (Proxy :: Proxy "foo")
  defMemArea buffer
  defConstMemArea table1
  defConstMemArea table2
