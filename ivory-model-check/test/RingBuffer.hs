{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module RingBuffer where

import GHC.TypeLits
import Ivory.Language
import Ivory.Stdlib
import Ivory.ModelCheck
-- import Ivory.Tower

data RingBuffer (n :: Nat) a =
  RingBuffer
    { ringbuffer_push   :: forall s eff . ConstRef s a -> Ivory eff IBool
    , ringbuffer_pop    :: forall s eff .      Ref s a -> Ivory eff IBool
    , ringbuffer_moddef :: ModuleDef
    }


-- monitorRingBuffer :: forall e n a
--                    . (ANat n, IvoryArea a)
--                   => String -> Monitor e (RingBuffer n a)
-- monitorRingBuffer name = do
--   n <- freshname name
--   let b :: RingBuffer n a
--       b = ringBuffer (showUnique n)
--   monitorModuleDef (ringbuffer_moddef b)
--   return b

ringBuffer :: forall n a
            . (ANat n, IvoryArea a)
           => String -> RingBuffer n a
ringBuffer s = RingBuffer
  { ringbuffer_push   = call push_proc
  , ringbuffer_pop    = call pop_proc
  , ringbuffer_moddef = do
      incl push_proc
      incl pop_proc
      defMemArea insert_area
      defMemArea remove_area
      defMemArea buf_area
  }
  where
  named n = s ++ "_ringbuffer_" ++ n

  remove_area :: MemArea (Stored (Ix n))
  remove_area = area (named "remove") (Just (ival 0))
  remove = addrOf remove_area
  insert_area :: MemArea (Stored (Ix n))
  insert_area = area (named "insert") (Just (ival 0))
  insert = addrOf insert_area
  buf_area :: MemArea (Array n a)
  buf_area = area (named "buf") Nothing
  buf = addrOf buf_area

  incr :: (GetAlloc eff ~ Scope s')
       => Ref s (Stored (Ix n)) -> Ivory eff (Ix n)
  incr ix = do
    i <- deref ix
    ifte (i ==? (fromIntegral ((ixSize i) - 1)))
      (return 0)
      (return (i + 1))

  full :: (GetAlloc eff ~ Scope s') => Ivory eff IBool
  full = do
    i <- incr insert
    r <- deref remove
    return (i ==? r)

  empty :: Ivory eff IBool
  empty = do
    i <- deref insert
    r <- deref remove
    return (i ==? r)

  push_proc :: Def('[ConstRef s a]:->IBool)
  push_proc = proc (named "push") $ \v ->
   body $ do
    f <- full
    ifte_ f (ret false) $ do
      i <- deref insert
      refCopy (buf ! i) v
      incr insert >>= store insert
      ret true

  pop_proc :: Def('[Ref s a]:->IBool)
  pop_proc = proc (named "pop") $ \v -> body $ do
    e <- empty
    ifte_ e (ret false) $ do
      r <- deref remove
      refCopy v (buf ! r)
      incr remove >>= store remove
      ret true

--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

queue :: RingBuffer 10 (Stored Uint8)
queue = ringBuffer "queue"

remove_area :: MemArea (Stored (Ix 10))
remove_area = area "queue_ringbuffer_remove" (Just (ival 0))
remove = addrOf remove_area
insert_area :: MemArea (Stored (Ix 10))
insert_area = area "queue_ringbuffer_insert" (Just (ival 0))
insert = addrOf insert_area

push_pop_inv :: Def('[ConstRef s (Stored Uint8)]:->())
push_pop_inv = proc "push_pop_inv" $ \x ->
  -- only holds if the buffer is not full
  requires (checkStored insert (\i -> checkStored remove (\r -> iNot (i+1 ==? r)))) $
  body $ do
    y <- local izero
    _ <- ringbuffer_push queue x
    _ <- ringbuffer_pop queue y
    xv <- deref x
    yv <- deref y
    assert (xv ==? yv)

testModule :: Module
testModule = package "ringbuffer" $ do
  ringbuffer_moddef queue
  incl push_pop_inv
