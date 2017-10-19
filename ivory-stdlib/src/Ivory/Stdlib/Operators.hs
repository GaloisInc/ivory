{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Stdlib.Operators where

import Ivory.Language

-- | Infix structure field access and dereference.
-- This is a shorthand for 'deref $ s~>x'.
(~>*) :: (IvoryVar a, IvoryStruct sym, IvoryStore a, KnownConstancy c)
      => Pointer 'Valid c s ('Struct sym)
      -> Label sym ('Stored a)
      -> Ivory eff a
struct ~>* label = deref (struct~>label)
infixl 8 ~>*


-- | Modify the value stored at a reference by a function.
(%=) :: IvoryStore a =>
     Ref s ('Stored a) -> (a -> a) -> Ivory eff ()
ref %= f = do
  val <- deref ref
  store ref (f val)

-- | Modify the value stored at a reference by a function that returns
-- a value in the Ivory monad
(%=!) :: IvoryStore a =>
         Ref s ('Stored a) -> (a -> Ivory eff a) -> Ivory eff ()
ref %=! mf = do
  val  <- deref ref
  val' <- mf val
  store ref val'

-- | Increment the value stored at a reference.
(+=) :: (Num a, IvoryStore a) =>
        Ref s ('Stored a) -> a -> Ivory eff ()
ref += x = ref %= (+ x)
