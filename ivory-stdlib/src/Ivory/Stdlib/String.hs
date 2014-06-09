{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
--
-- String.hs --- C-string utilities for Ivory.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.Stdlib.String
  ( copy_istring , strcpy , strncpy , strncpy_uint8 , strncmp
  , stdlibStringModule, stringInit, istr_eq, sz_from_istr
  , istr_from_sz
  ) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Language.Proxy

import qualified Control.Monad as M

-- Should be same underlying type as IxRep to make casting easier.
type Len = Sint32

----------------------------------------------------------------------
-- Yet Another Ivory String Type

-- TODO: Should we generate a warning or error if the string
-- is too long for the string type?
gen_stringInit :: (IvoryStruct name, ANat len)
               => Label name (Array len (Stored Uint8))
               -> Label name (Stored Len)
               -> String
               -> Init (Struct name)
gen_stringInit l_data l_len xs =
  istruct
    [ l_data .= iarray (map (ival . fromIntegral . ord) xs)
    , l_len  .= ival len
    ]
  where
  len :: Len
  len = fromIntegral (length xs)

stringInit :: IvoryString str => String -> Init str
stringInit = gen_stringInit stringDataL stringLengthL

----------------------------------------------------------------------
-- Generic Functions

stringCapacity :: forall ref str s.
                  (IvoryString str, IvoryRef ref)
               => ref s str -> Len
stringCapacity _ = len
  where
  len :: Len
  len = fromIntegral (fromTypeNat (aNat :: NatType (Capacity str)))

-- Polymorphic "stringLength" function allowing read/write access
-- to the string length.  This is not exported, only a specialized
-- read-only version.
stringLength :: ( IvoryString str
                , IvoryRef ref
                , IvoryExpr (ref s (Stored Len))
                , IvoryExpr (ref s str))
             => ref s str -> ref s (Stored Len)
stringLength x = x ~> stringLengthL

stringData :: ( IvoryString str
              , IvoryRef ref
              , IvoryExpr (ref s (Array (Capacity str) (Stored Uint8)))
              , IvoryExpr (ref s (CArray (Stored Uint8)))
              , IvoryExpr (ref s str))
           => ref s str -> ref s (CArray (Stored Uint8))
stringData x = toCArray (x ~> stringDataL)

-- | Binding to the C "memcmp" function.
memcmp :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                , ConstRef s2 (CArray (Stored Uint8))
                , Len] :-> Len)
memcmp = importProc "memcmp" "string.h"

-- | Binding to the C "memcpy" function.
memcpy :: Def ('[ Ref      s1 (CArray (Stored Uint8))
                , ConstRef s2 (CArray (Stored Uint8))
                , Len] :-> Len)
memcpy = importProc "memcpy" "string.h"

-- | Return the length of a string.
istr_len :: IvoryString str
         => ConstRef s str
         -> Ivory eff Len
istr_len = deref . stringLength

-- | Copy one string into another of the same type.
istr_copy :: IvoryString str
          => Ref      s1 str
          -> ConstRef s2 str
          -> Ivory eff ()
istr_copy dest src = do
  len <- istr_len src
  call_ memcpy (stringData dest) (stringData src) len
  store (stringLength dest) len

-- | Copy one string to another of a possibly different type.  If
-- the destination string is too small, the output may be truncated.
-- This returns true if the string fit, and false if it was
-- truncated to fit the destination.
--
-- TODO: Implement this once it's needed.
istr_convert :: (IvoryString str1, IvoryString str2)
             => Ref      s1 str1
             -> ConstRef s2 str2
             -> Ivory eff IBool
istr_convert = undefined

-- | Internal function to compare strings for equality.
do_istr_eq :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                    , Len
                    , ConstRef s2 (CArray (Stored Uint8))
                    , Len
                    ] :-> IBool)
do_istr_eq = proc "ivory_string_eq" $ \s1 len1 s2 len2 -> body $ do
  ifte_ (len1 ==? len2)
    (do r <- call memcmp s1 s2 len1
        ret (r ==? 0))
    (ret false)

-- | Compare strings (of possibly different types) for equality.
-- Returns true if the strings are the same length and contain the
-- same bytes.
istr_eq :: (IvoryString str1, IvoryString str2)
        => ConstRef s1 str1
        -> ConstRef s2 str2
        -> Ivory eff IBool
istr_eq s1 s2 = do
  len1 <- istr_len s1
  ptr1 <- assign (stringData s1)
  len2 <- istr_len s2
  ptr2 <- assign (stringData s2)
  call do_istr_eq ptr1 len1 ptr2 len2

-- | Primitive function to do a bounded string copy.
string_copy :: Def ('[ Ref s1 (CArray (Stored Uint8))
                     , Len
                     , ConstRef s2 (CArray (Stored Uint8))
                     , Len] :-> Len)
string_copy = importProc "ivory_stdlib_string_copy"
                         "ivory_stdlib_string_prim.h"

-- | Primitive function to do a bounded null-terminated string copy.
string_copy_z :: Def ('[ Ref s1 (CArray (Stored Uint8))
                       , Len
                       , ConstRef s2 (CArray (Stored Uint8))
                       , Len] :-> Len)
string_copy_z = importProc "ivory_stdlib_string_copy_z"
                           "ivory_stdlib_string_prim.h"

-- | Copy the contents of a fixed-size C string into an Ivory
-- string.  If the source string is not null terminated (and
-- therefore corrupt), this will copy no more than 'len'
-- characters.
--
-- FIXME: This should return false if the string was truncated.
-- (Can we actually detect this at compile-time?  I think we
-- should be able to...)
istr_from_sz :: forall s1 s2 eff len str.
                (ANat len, IvoryString str)
             => Ref      s1 str
             -> ConstRef s2 (Array len (Stored Uint8))
             -> Ivory eff ()
istr_from_sz dest src = do
  len1     <- assign (stringCapacity (constRef dest))
  ptr1     <- assign (stringData dest)
  let len2  = fromIntegral (fromTypeNat (aNat :: NatType len))
  ptr2     <- assign (toCArray src)
  result   <- call string_copy_z ptr1 len1 ptr2 len2
  store (stringLength dest) result

-- | Copy an Ivory string to a fixed-size, null-terminated C
-- string.  The destination string is always properly terminated,
-- but may be truncated if the buffer is too small.
--
-- FIXME: This should return false if the string was truncated.
-- (Can we actually detect this at compile-time?  I think we
-- should be able to...)
sz_from_istr :: forall s1 s2 eff len str.
                (ANat len, IvoryString str)
             => Ref      s1 (Array len (Stored Uint8))
             -> ConstRef s2 str
             -> Ivory eff ()
sz_from_istr dest src = do
  let dest_capacity = fromTypeNat (aNat :: NatType len)
  M.when (dest_capacity > 0) $ do
    let dest_len = fromIntegral (dest_capacity - 1)
    src_data <- assign (stringData src)
    src_len  <- istr_len src
    _result  <- call string_copy (toCArray dest) dest_len src_data src_len
    -- XXX is this right?  shouldn't it use "result"?
    store (dest ! toIx (dest_len - 1)) 0

----------------------------------------------------------------------
-- Old String Functions

-- TODO: Delete these if they aren't used anymore.

-- | Safely copy an IString (string literal) into a character array.
-- The resulting string will always be null terminated.
copy_istring :: Def ('[ Ref s (CArray (Stored IChar)) -- dest
                      , IString                       -- src
                      , Uint32]                       -- len
                     :-> ())
copy_istring = importProc "ivory_stdlib_strlcpy" "ivory_stdlib_string_prim.h"

-- | Type class to generate the correct call to a string function to
-- copy one C string to another.
class (IvoryType dest, IvoryType src) => Strcpy dest src where
  strcpy :: dest -> src -> Ivory eff ()

-- | Strcpy instance for copying string constants to arrays of
-- characters.
instance (ANat len) => Strcpy (Ref s (Array len (Stored IChar))) IString where
  strcpy dest src = call_ copy_istring (toCArray dest) src (arrayLen dest)

-- | Binding to the C "strncmp" function.
strncmp :: Def ('[ ConstRef s1 (CArray (Stored IChar)) -- s1
                 , ConstRef s2 (CArray (Stored IChar)) -- s2
                 , Uint32]                             -- len
                :-> Sint32)
strncmp = importProc "strncmp" "string.h"


strncpy :: Def ('[ Ref s1  (CArray (Stored IChar))
                 , ConstRef s2 (CArray (Stored IChar))
                 , Uint32
                 ] :-> ())
strncpy = importProc "strncpy" "string.h"

strncpy_uint8 :: Def ('[ Ref s1  (CArray (Stored Uint8))
                 , ConstRef s2 (CArray (Stored IChar))
                 , Uint32
                 ] :-> ())
strncpy_uint8 = importProc "ivory_stdlib_strncpy_uint8" "ivory_stdlib_string_prim.h"

-- | Ivory module definition.
stdlibStringModule :: Module
stdlibStringModule = package "ivory_stdlib_string" $ do
  inclHeader "ivory_stdlib_string_prim.h"
  inclHeader "string.h"
  incl copy_istring
  incl strncmp
  incl memcmp
  incl memcpy
  incl do_istr_eq
  incl string_copy
  incl string_copy_z
  sourceDep "ivory_stdlib_string_prim.h"
  sourceDep "ivory_stdlib_string_prim.c"

