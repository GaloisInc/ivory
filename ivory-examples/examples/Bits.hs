{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Bits (runBits, cmodule) where

import Control.Monad (void)
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language hiding (setBit, clearBit, runBits)
import MonadLib.Monads (runState, sets)

runBits :: IO ()
runBits = void $ runCompiler [cmodule] initialOpts {stdOut = True}

cmodule :: Module
cmodule = package "Bits" $ do
  incl test1
  incl test2
  incl test3
  incl test4

test1 :: Def ('[Uint8, Uint16, Uint32, Uint64] :-> Uint64)
test1 = proc "test1" $ \u8 u16 u32 u64 -> body $ do
  a <- assign $ u8  .& 0xFF
  b <- assign $ u16 .& 0xFF00
  c <- assign $ u32 .& 0xFF0000
  d <- assign $ u64 .& 0xFF000000
  ret $ (safeCast a) .| (safeCast b) .| (safeCast c) .| d

-- | Convert an array of four 8-bit integers into a 32-bit integer.
test2 :: Def ('[Ref s (Array 4 (Stored Uint8))] :-> Uint32)
test2 = proc "test2" $ \arr -> body $ do
  a <- deref (arr ! 0)
  b <- deref (arr ! 1)
  c <- deref (arr ! 2)
  d <- deref (arr ! 3)
  ret $ ((safeCast a) `iShiftL` 24) .|
        ((safeCast b) `iShiftL` 16) .|
        ((safeCast c) `iShiftL` 8)  .|
        ((safeCast d) `iShiftL` 0)

-- | Example of using "extractByte" with a state monad.
extractUint32 :: Uint32 -> (Uint8, Uint8, Uint8, Uint8)
extractUint32 x = fst $ runState x $ do
  a <- sets extractByte
  b <- sets extractByte
  c <- sets extractByte
  d <- sets extractByte
  return (a, b, c, d)

-- | Convert a 32-bit integer to an array of 8-bit integers.
test3 :: Def ('[Uint32, Ref s (Array 4 (Stored Uint8))] :-> ())
test3 = proc "test3" $ \n arr -> body $ do
  let (a, b, c, d) = extractUint32 n
  store (arr ! 0) d
  store (arr ! 1) c
  store (arr ! 2) b
  store (arr ! 3) a

setBit :: (IvoryBits a, IvoryStore a)
       => (Ref s (Stored a)) -> Int -> Ivory eff ()
setBit ref bit = do
  val <- deref ref
  store ref (val .| (1 `iShiftL` (fromIntegral bit)))

clearBit :: (IvoryBits a, IvoryStore a)
         => (Ref s (Stored a)) -> Int -> Ivory eff ()
clearBit ref bit = do
  val <- deref ref
  store ref (val .& (iComplement (1 `iShiftL` (fromIntegral bit))))

test4 :: Def ('[] :-> Uint32)
test4 = proc "test4" $ body $ do
  n <- local (ival 0)
  setBit n 1
  setBit n 3
  setBit n 5
  setBit n 8
  clearBit n 3
  ret =<< deref n
