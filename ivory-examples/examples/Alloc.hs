{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Alloc where

import Control.Monad (void)
import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Compile.C.Modules


[ivory|

struct Foo
  { i :: Stored Uint32
  ; p :: Array 10 (Stored Uint32)
  ; d :: Stored Uint32
  }

struct Str {
  name :: Array 32 (Stored IChar)
  }

|]

test :: Def ('[Ref s (Struct "Foo")] :-> Ref s (Stored Uint32))
test  = proc "test" (\ pid -> body (ret (pid ~> i)))

alloc_test :: Def ('[] :-> Uint32)
alloc_test  = proc "alloc_test" $ body $ do
  pid <- local (istruct [])
  ret =<< deref (pid ~> d)

memcpy1 :: Def ('[ Ref a (Struct "Foo"), Ref a (Struct "Foo") ] :-> Uint32)
memcpy1 = proc "memcpy1" $ \a b -> body $ do
  refCopy b a
  ret =<< deref (b ~> i)

memcpy2 :: Def ('[ Ref a (Array 10 (Stored Uint32)), Ref a (Array 10 (Stored Uint32)) ] :-> ())
memcpy2 = proc "memcpy2" $ \a b -> body $ do
  refCopy b a
  arrayMap (\ix -> store (a ! (ix :: Ix 10)) 1)
  retVoid

memcpy3 :: Def ('[ Ref Global (Array 10 (Stored Uint32))] :-> ())
memcpy3 = proc "memcpy3" $ \a -> body $ do
  b <- local (iarray $ replicate 10 (ival $ 0))
  refCopy b a
  arrayMap (\ix -> store (a ! (ix :: Ix 10)) 1)
  retVoid

-- Can't do this! (Which is good.)
-- memcpy4 :: Def ('[ Ref Global (Array 1 (Stored (Ref (Stack s) (Stored Uint32))))] :-> ())
-- memcpy4 = proc "memcpy3" $ \a -> body $ do
--   val  <- local (ival 2)
--   larr <- local (iarray [ival val])
--   refCopy a larr
--   arrayMap (\ix -> store (a ! (ix :: Ix 1)) 1)
--   retVoid

{-
-- The type system prevents this.
bad_alloc :: Def ('[] :-> Ref s (Stored Uint32))
bad_alloc = proc "bad_alloc" $ body $ do
  pid <- local (istruct [])
  ret (pid~>i)
-}

arrMap :: Def ('[Ref s (Array 15 (Stored Sint32))] :-> ())
arrMap = proc "arrMap" $ \ arr -> body $ do
  arrayMap (\ix -> store (arr ! (ix :: Ix 15)) 1)
  retVoid

-- String copy test -------------------------
ptrstrcpy :: Def ('[Ref s (CArray (Stored IChar)), IString, Uint32] :-> ())
ptrstrcpy = proc "ptrstrcpy" $ \ _ _ _ ->  body $ do
  retVoid

callstrcpy :: Def ('[] :-> ())
callstrcpy  = proc "callstrcpy" $ body $ do
  buf' <- local (iarray [])
  call_ mystrcpy buf' "hello"
  retVoid

-- | Safely copy a string literal into a character array.
mystrcpy :: Def ('[Ref s (Array 10 (Stored IChar)), IString] :-> ())
mystrcpy = proc "mystrcpy" $ \ buf s -> body $ do
  buf' <- assign $ toCArray buf
  call_ ptrstrcpy buf' s (arrayLen buf)
  retVoid

assign_test :: Def ('[] :-> ())
assign_test  = proc "assign_test" $ body $ do
  val <- local (istruct [])
  _ <- assign (val ~> p)
  retVoid

bar :: Def ('[] :-> ())
bar = proc "var" $ body $ do
  pid <- local $ istruct [i .= ival 3, i .= ival 7]
  arr <- local $ iarray [ ival c | c <- replicate 10 (char 'a') ]
  call_ mystrcpy arr "hello"
  store (pid~>i) 4

castIx :: Def ('[Ix 253] :-> Uint8)
castIx = proc "castIx" $ \ix -> body $ do
  ret $ safeCast (ix :: Ix 253)

loopTest :: Def ('[Ref s (Array 15 (Stored Sint32))] :-> ())
loopTest = proc "loopTest" $ \ arr -> body $ do
  arrayMap (\ix -> store (arr ! (ix :: Ix 15)) 1)
  times 3 (\ix -> store (arr ! (ix :: Ix 15)) 1)
  for 0 (\ix -> store (arr ! (ix :: Ix 15)) 1)
  retVoid

testToIx :: Def ('[Sint32, Ref s (Array 10 (Stored Uint32))] :-> Ref s (Stored Uint32))
testToIx = proc "testToIx" $ \ ind arr -> body $ do
  let idx = toIx ind :: Ix 10
  ret (arr ! idx)

arrayTest :: MemArea (Array 10 (Struct "Foo"))
arrayTest  = area "arrayTest" $ Just $ iarray
  [ istruct [ i .= ival 10 ]
  ]

-- DefProc (Proc {procSym = "foo", procRetTy = TyWord Word32, procArgs = [Typed
-- {tType = TyRef (TyStruct "Foo"), tValue = VarName "var0"}], procBody = [

--   Deref (TyWord Word32) (VarName "deref0") (ExpLabel (TyStruct "Foo") (ExpVar
--     (VarName "var0")) "i")
-- , Return (Typed {tType = TyWord Word32, tValue = ExpVar
--     (VarName "deref0")})
-- ], procRequires = [], procEnsures = Nothing})

-- uint32_t n_deref0 = *&n_var0->i;
foo :: Def ('[Ref s (Struct "Foo")] :-> Uint32)
foo = proc "foo" $ \str -> body $ do
  ret =<< deref (str ~> i)

-- DefProc (Proc {procSym = "foo2", procRetTy = TyWord Word32, procArgs = [Typed
-- {tType = TyRef (TyStruct "Foo"), tValue = VarName "var0"}], procBody = [

-- Deref (TyWord Word32) (VarName "deref0") (ExpIndex (TyArr 10 (TyWord Word32))
-- (ExpLabel (TyStruct "Foo") (ExpVar (VarName "var0")) "p") (TyInt Int32)
-- (ExpOp ExpMod [ExpLit (LitInteger 0),ExpLit (LitInteger 10)]))

-- ,Return (Typed
-- {tType = TyWord Word32, tValue = ExpVar (VarName "deref0")})], procRequires =
-- [], procEnsures = Nothing})

-- uint32_t n_deref0 = *&n_var0->p[0 % 10];
foo2 :: Def ('[Ref s (Struct "Foo")] :-> Uint32)
foo2 = proc "foo2" $ \str -> body $ do
  let arr = (str ~> p)
  let x = arr ! (0 :: Ix 10)
  ret =<< deref x --deref (arr ! 0)

-- foo3 :: Def ('[Ref s (Struct "Bar")] :-> Uint32)
-- foo3 = proc "foo3" $ \str -> body $ do
--   v <- deref (str ~> aa)
--   ret =<< deref v

---------------------------------------------

cmodule :: Module
cmodule = package "Alloc" $ do
  defStruct (Proxy :: Proxy "Foo")
  defStruct (Proxy :: Proxy "Str")
  incl test
  incl alloc_test
  incl arrMap

  incl ptrstrcpy
  incl mystrcpy

  incl callstrcpy

  incl assign_test

  incl bar
  incl castIx
  incl loopTest
  incl memcpy1
  incl memcpy2
  incl testToIx
  incl memcpy3

  defMemArea arrayTest

runAlloc :: IO ()
runAlloc = void $ runCompiler [cmodule] initialOpts { stdOut = True }

test2 :: [[String]]
test2 = showModule (compileModule cmodule)
