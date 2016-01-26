{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module RingBuffer where

import           GHC.TypeLits
import           Ivory.Language
import           Ivory.Stdlib

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
            . (ANat n, IvoryArea a, IvoryZero a)
           => String -> RingBuffer n a
ringBuffer s = RingBuffer
  { ringbuffer_push   = call push_proc
  , ringbuffer_pop    = call pop_proc
  , ringbuffer_moddef = do
      incl push_proc
      incl pop_proc
      defMemArea insert_area'
      defMemArea remove_area'
      defMemArea buf_area'
  }
  where
  named n = s ++ "_ringbuffer_" ++ n

  remove_area' :: MemArea ('Stored (Ix n))
  remove_area' = area (named "remove") (Just (ival 0))
  remove' = addrOf remove_area'

  insert_area' :: MemArea ('Stored (Ix n))
  insert_area' = area (named "insert") (Just (ival 0))
  insert' = addrOf insert_area'

  buf_area' :: MemArea ('Array n a)
  buf_area' = area (named "buf") Nothing
  buf' = addrOf buf_area'

  incr :: (GetAlloc eff ~ 'Scope s')
       => Ref s ('Stored (Ix n)) -> Ivory eff (Ix n)
  incr ix = do
    i <- deref ix
    ifte (i ==? (fromIntegral ((ixSize i) - 1)))
      (return 0)
      (return (i + 1))

  full :: (GetAlloc eff ~ 'Scope s') => Ivory eff IBool
  full = do
    i <- incr insert'
    r <- deref remove'
    return (i ==? r)

  isEmpty' :: Ivory eff IBool
  isEmpty' = do
    i <- deref insert'
    r <- deref remove'
    return (i ==? r)

  push_proc :: Def('[ConstRef s a] ':-> IBool)
  push_proc = proc (named "push") $ \v ->
   -- requires/ensures in terms of hidden state?
   body $ do
    f <- full
    ifte_ f (ret false) $ do
      i <- deref insert'
      refCopy (buf' ! i) v
      incr insert' >>= store insert'
      ret true

  pop_proc :: Def('[Ref s a] ':-> IBool)
  pop_proc = proc (named "pop") $ \v -> body $ do
    e <- isEmpty'
    ifte_ e (ret false) $ do
      r <- deref remove'
      refCopy v (buf' ! r)
      incr remove' >>= store remove'
      ret true

--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

-- A `RingBuffer n a` can hold `n-1` values!
queue :: RingBuffer 3 ('Stored Uint8)
queue = ringBuffer "queue"

remove_area :: MemArea ('Stored (Ix 3))
remove_area = area "queue_ringbuffer_remove" (Just (ival 0))

remove :: Ref 'Global ('Stored (Ix 3))
remove  = addrOf remove_area

insert_area :: MemArea ('Stored (Ix 3))
insert_area = area "queue_ringbuffer_insert" (Just (ival 0))

insert :: Ref 'Global ('Stored (Ix 3))
insert  = addrOf insert_area

bounded :: ANat n => Ix n -> IBool
bounded x = x >=? 0 .&& x <=? (fromIntegral $ ixSize x - 1)

isEmpty :: ANat n => Ix n -> Ix n -> IBool
isEmpty i r = i' ==? r'
  where
  i' = fromIx i
  r' = fromIx r

push_pop_inv :: Def('[ConstRef s ('Stored Uint8), ConstRef s ('Stored Uint8)] ':-> ())
push_pop_inv = proc "push_pop_inv" $ \x y ->
  -- assume buffer is empty to start
  requires (checkStored insert (\i -> checkStored remove (\r ->
     isEmpty i r .&& bounded i .&& bounded r))) $
  body $ do
    o <- local izero

    let test ref =
          do xv <- deref ref
             ov <- deref o
             assert (xv ==? ov)

    assert =<< ringbuffer_push queue x
    assert =<< ringbuffer_push queue y
    assert =<< ringbuffer_pop  queue o
    test x

    assert =<< ringbuffer_push queue x
    assert =<< ringbuffer_pop  queue o
    test y

    assert =<< ringbuffer_pop  queue o
    test x

testModule :: Module
testModule = package "ringbuffer" $ do
  ringbuffer_moddef queue
  incl push_pop_inv
