{-# OPTIONS_GHC  -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AddrOfRegression where

import Ivory.Language

[ivory|
struct param_info
  { param_requested :: Stored Uint8
  ; other           :: Stored Uint16
  }
|]

test1 :: ModuleDef
test1 = do
  defMemArea param_info_area
  incl t1
  where
  param_info_area :: MemArea ('Array 512 ('Struct "param_info"))
  param_info_area = area "g_param_info" Nothing

  param_info_ref :: Ref 'Global ('Array 512 ('Struct "param_info"))
  param_info_ref = addrOf param_info_area

  t1 :: Def ('[] ':-> ())
  t1 = proc "t1" $ body $ do
    arrayMap $ \ix ->
      store ((param_info_ref ! ix) ~> param_requested) 1

test1_noarray :: ModuleDef
test1_noarray = do
  defMemArea param_info_area
  incl t1
  where
  param_info_area :: MemArea ('Struct "param_info")
  param_info_area = area "single_param_info" Nothing

  param_info_ref :: Ref 'Global ('Struct "param_info")
  param_info_ref = addrOf param_info_area

  t1 :: Def ('[] ':-> ())
  t1 = proc "t1_noarray" $ body $ do
    store (param_info_ref ~> param_requested) 1

test2 :: ModuleDef
test2 = do
  defMemArea atom_array_area
  incl t2
  where
  atom_array_area :: MemArea ('Array 512 ('Stored IFloat))
  atom_array_area = area "atom_array" Nothing

  atom_array_ref :: Ref 'Global ('Array 512 ('Stored IFloat))
  atom_array_ref = addrOf atom_array_area

  t2 :: Def ('[] ':-> ())
  t2 = proc "t2" $ body $ do
    arrayMap $ \ix ->
      store (atom_array_ref ! ix) 1

test3 :: ModuleDef
test3 = do
  incl t3
  where
  t3 :: Def ('[] ':-> ())
  t3 = proc "t3" $ body $ do
    (stack_array :: Ref ('Stack s) ('Array 512 ('Stored IFloat))) <- local izero
    arrayMap $ \ix ->
      store (stack_array ! ix) 1


cmodule :: Module
cmodule = package "AddrOfRegression" $ do
  defStruct (Proxy :: Proxy "param_info")
  test1
  test1_noarray
  test2
  test3



