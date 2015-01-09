{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module FunPtr where

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

f :: Def ('[Sint32] :-> Sint32)
f  = proc "f" (\ n -> body (ret (n + 1)))

invoke :: Def ('[ ProcPtr ('[Sint32] :-> Sint32), Sint32] :-> Sint32)
invoke  = proc "invoke" (\ k n -> body (ret =<< indirect k n))

test :: Def ('[] :-> Sint32)
test  = proc "test" (body (ret =<< call invoke (procPtr f) 10))

runFunPtr :: IO ()
runFunPtr = runCompiler [cmodule] [] initialOpts { outDir = Nothing }

cmodule :: Module
cmodule = package "FunPtr" $ do
  incl f
  incl test
  incl invoke

--------------------------------------------------------------------------------

type Fn = ('[Uint64] :-> Uint8)

bar :: Def Fn
bar = proc "bar" $ \_ -> body $ ret 42

procArea :: MemArea (Stored (ProcPtr Fn))
procArea = refArea "procArea" (ival (procPtr bar))

foo :: Def ('[] :-> ())
foo = proc "foo" $ body $ do
  store (addrOf procArea) (procPtr bar)
  p <- deref (addrOf procArea)
  indirect_ p 3
  r <- local (ival (procPtr bar))
  r' <- deref r
  indirect_ r' 3

-- Impossible (good)
-- procArea2 :: MemArea (Stored (ProcPtr Fn))
-- procArea2 = area "procArea2" (Just (ival (procPtr bar)))
-- procArea2 :: MemArea (Stored (Ref Global (Stored (ProcPtr Fn))))
-- procArea2 = area "procArea2" Nothing
-- procArea3 :: MemArea (Stored (ProcPtr Fn))
-- procArea3 = area "procArea3" Nothing
-- procArea3 :: MemArea (Stored (Uint8))
-- procArea3 = area "procArea3" Nothing

cmodule2 :: Module
cmodule2 = package "funptr2" $ do
  incl bar
  incl foo
  defMemArea procArea

runFunPtr2 :: IO ()
runFunPtr2 = runCompiler [cmodule2] [] initialOpts { outDir = Just "cmodule2" }

