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

module Ivory.Stdlib.String where
  -- ( copy_istring , strcpy , strncpy , strncpy_uint8 , strncmp
  -- , stdlibStringModule
  -- ) where

import Data.Char (ord)
import GHC.TypeLits

import Ivory.Language

import qualified Control.Monad as M

----------------------------------------------------------------------
-- Yet Another Ivory String Type

-- XXX we will clean up these names, but some will be exported
-- and some will not (we definitely will not allow writing to
-- the string length, for example)

class IvoryArea a => IvoryString a where
  stringCapacity    :: ConstRef s a -> Sint32
  stringData        :: Ref      s a -> Ref      s (CArray (Stored Uint8))
  stringDataC       :: ConstRef s a -> ConstRef s (CArray (Stored Uint8))
  stringLength      :: Ref      s a -> Ref      s (Stored Sint32)
  stringLengthC     :: ConstRef s a -> ConstRef s (Stored Sint32)

-- TODO: Should we generate a warning or error if the string
-- is too long for the string type?
gen_stringInit :: (IvoryStruct name, SingI len)
               => Label name (Array len (Stored Uint8))
               -> Label name (Stored Sint32)
               -> String
               -> Init (Struct name)
gen_stringInit l_data l_len xs =
  istruct
    [ l_data .= iarray (map (ival . fromIntegral . ord) xs)
    , l_len  .= ival (fromIntegral (length xs))
    ]

----------------------------------------------------------------------
-- Example Generated Code

{-
[ivory|
string ParamName 16
|]

[ivory|
struct string_ParamName
  { string_ParamName_data :: Array 16 (Stored Uint8)
  ; string_ParamName_len  :: Stored Sint32
  }
|]

type ParamName = Struct "string_ParamName"

init_ParamName :: String -> Init ParamName
init_ParamName = gen_stringInit string_ParamName_data string_ParamName_len

instance IvoryString (Struct "string_ParamName") where
  stringCapacity x = arrayLen (x ~> string_ParamName_data)
  stringData x     = toCArray (x ~> string_ParamName_data)
  stringDataC x    = toCArray (x ~> string_ParamName_data)
  stringLength x   = x ~> string_ParamName_len
  stringLengthC x  = x ~> string_ParamName_len
-}

----------------------------------------------------------------------
-- Generic Functions

-- | Binding to the C "memcmp" function.
memcmp :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                , ConstRef s2 (CArray (Stored Uint8))
                , Sint32] :-> Sint32)
memcmp = importProc "memcmp" "string.h"

-- | Binding to the C "memcpy" function.
memcpy :: Def ('[ Ref      s1 (CArray (Stored Uint8))
                , ConstRef s2 (CArray (Stored Uint8))
                , Sint32] :-> Sint32)
memcpy = importProc "memcpy" "string.h"

-- | Return the length of a string.
istr_len :: IvoryString str
         => ConstRef s str
         -> Ivory eff Sint32
istr_len str = do
  len <- deref (stringLengthC str)
  assert (len <? stringCapacity str)
  return len

-- | Copy one string into another of the same type.
istr_copy :: IvoryString str
          => Ref      s1 str
          -> ConstRef s2 str
          -> Ivory eff ()
istr_copy dest src = do
  len <- istr_len src
  call_ memcpy (stringData dest) (stringDataC src) len
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
                    , Sint32
                    , ConstRef s2 (CArray (Stored Uint8))
                    , Sint32
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
  ptr1 <- assign (stringDataC s1)
  len2 <- istr_len s2
  ptr2 <- assign (stringDataC s2)
  call do_istr_eq ptr1 len1 ptr2 len2

-- | Primitive function to do a bounded string copy.
string_copy :: Def ('[ Ref s1 (CArray (Stored Uint8))
                     , Sint32
                     , ConstRef s2 (CArray (Stored Uint8))
                     , Sint32] :-> Sint32)
string_copy = importProc "ivory_stdlib_string_copy"
                         "ivory_stdlib_string_prim.h"

-- | Primitive function to do a bounded null-terminated string copy.
string_copy_z :: Def ('[ Ref s1 (CArray (Stored Uint8))
                       , Sint32
                       , ConstRef s2 (CArray (Stored Uint8))
                       , Sint32] :-> Sint32)
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
                (SingI len, IvoryString str)
             => Ref      s1 str
             -> ConstRef s2 (Array len (Stored Uint8))
             -> Ivory eff ()
istr_from_sz dest src = do
  len1     <- assign (stringCapacity (constRef dest))
  ptr1     <- assign (stringData dest)
  let len2  = fromIntegral (fromSing (sing :: Sing len))
  ptr2     <- assign (toCArray src)
  result   <- call string_copy_z ptr1 len1 ptr2 len2
  assert (result <=? len1)
  store (stringLength dest) result

-- | Copy an Ivory string to a fixed-size, null-terminated C
-- string.  The destination string is always properly terminated,
-- but may be truncated if the buffer is too small.
--
-- FIXME: This should return false if the string was truncated.
-- (Can we actually detect this at compile-time?  I think we
-- should be able to...)
sz_from_istr :: forall s1 s2 eff len str.
                (SingI len, IvoryString str)
             => Ref      s1 (Array len (Stored Uint8))
             -> ConstRef s2 str
             -> Ivory eff ()
sz_from_istr dest src = do
  let dest_capacity = fromSing (sing :: Sing len)
  M.when (dest_capacity > 0) $ do
    let dest_len = fromIntegral (dest_capacity - 1)
    src_data <- assign (stringDataC src)
    src_len  <- istr_len src
    result   <- call string_copy (toCArray dest) dest_len src_data src_len
    assert (result <=? dest_len)
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
instance (SingI len) => Strcpy (Ref s (Array len (Stored IChar))) IString where
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

