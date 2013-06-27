{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

-- | Internal sanity-checks for the interpreter.
module Ivory.Interp.Tests where

import Ivory.Interp
import Ivory.Language


testInterp :: Eval ()
testInterp  = do

  io (putStrLn "type IBool")
  equality (Proxy :: Proxy IBool)

  io (putStrLn "type Sint8")
  equality (Proxy :: Proxy Sint8)

  io (putStrLn "type Sint16")
  equality (Proxy :: Proxy Sint16)

  io (putStrLn "type Sint32")
  equality (Proxy :: Proxy Sint32)

  io (putStrLn "type Sint32")
  equality (Proxy :: Proxy Sint64)

  io (putStrLn "type Uint8")
  equality (Proxy :: Proxy Uint8)

  io (putStrLn "type Uint16")
  equality (Proxy :: Proxy Uint16)

  io (putStrLn "type Uint32")
  equality (Proxy :: Proxy Uint32)

  io (putStrLn "type Uint32")
  equality (Proxy :: Proxy Uint64)

  io (putStrLn "read/write ref")
  quickcheck $ \ ref -> body $ do
    val <- deref ref
    store ref (val + 1 :: Sint32)
    val' <- deref ref
    ret ((val + 1) ==? val')

  io (putStrLn "read/write array")
  let arrBody :: Ref s (Array 10 (Stored Uint8)) -> Body IBool
      arrBody arr = body $ do
        res <- local (ival true)
        arrayMap $ \ ix -> do
          let ref = arr ! (ix :: Ix 10)
          store ref 0xff
          val <- deref ref
          acc <- deref res
          store res (acc .&& (val ==? 0xff))
        ret =<< deref res
  quickcheck arrBody

  io (putStrLn "conditional expressions (true branch)")
  quickcheck (\ n -> body (ret (n ==? (true ? (n,n+1 :: Uint32)))))

  io (putStrLn "conditional expressions (false branch)")
  quickcheck (\ n -> body (ret (n ==? (false ? (n+1,n :: Uint32)))))


-- | Properties of equality.
equality :: IvoryEq a => Proxy a -> Eval ()
equality ty = do
  prop_eq_sym ty

-- | Test symmetry of equality for types that support it.
prop_eq_sym :: forall ty. (IvoryEq ty) => Proxy ty -> Eval ()
prop_eq_sym _ = do
  io (putStr "  prop_eq_sym ")
  quickcheck (\ a -> body (ret (a ==? (a :: ty))))
