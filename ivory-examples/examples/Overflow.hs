{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Overflow where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

ovf1 :: Def ('[Sint8] :-> Sint8)
ovf1  = proc "ovf1" $ \ n -> body $
  ifte_ (n <? maxBound - 20)
       (ret (n + 15))
       (ret (n .% 2))

ovf2 :: Def ('[Sint8] :-> Sint8)
ovf2  = proc "ovf2" $ \ n -> requires (n <? 1)
                           $ body
                           $ ret (n + 15)

ovf3 :: Def ('[IFloat, IFloat, IFloat] :-> IBool)
ovf3  = proc "ovf3" $ \ n m o -> body $ do
  x <- assign (n / m / o)
  ret $ x >? (n / m)


foo :: Def ('[Uint8, Uint8, Uint8, Uint8] :-> Sint32)
foo = proc "foo" $ \ a b c d -> body $ do
  foo' a b c d >>= ret

foo' :: Uint8 -> Uint8 -> Uint8 -> Uint8 -> Ivory eff Sint32
foo' a b c d = do
  uint <- fromTwosComplement $
            (a `shift` 24) .|
            (b `shift` 16) .|
            (c `shift` 8)  .|
            safeCast d
  return (castWith 0 uint)
  where
  shift :: Uint8 -> Uint32 -> Uint32
  shift x y = safeCast x `iShiftL` y

fromTwosComplement :: Uint32 -> Ivory eff Uint32
fromTwosComplement i = do
  i' <- assign i
  n  <- assign (i' >? maxBound `iDiv` 2)
  twosComp <- assign (iComplement i' .^ 1)
  return (n ? (twosComp, i'))

cmodule :: Module
cmodule = package "Overflow" $ incl ovf1 >> incl ovf2 >> incl ovf3

fooM :: Module
fooM = package "foo" $ incl foo

writeOverflow :: Opts -> IO ()
writeOverflow opts = runCompiler [fooM]
  opts { stdOut = True, constFold = True, overflow = True }
