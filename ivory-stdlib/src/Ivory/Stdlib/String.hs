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
  {-
  ( copy_istring , strcpy , strncpy , strncpy_uint8 , strncmp
  , idynstring, memcmp, istr_eq
  , stdlibStringModule
  ) where
  -}

import Data.Char (ord)
import GHC.TypeLits

import Ivory.Language

import qualified Control.Monad as M

----------------------------------------------------------------------
-- DynArray-Based String Type
--
-- TODO: Document this mess.  Especially how we have so many
-- different ways of representing strings, and this is intended
-- to be the common denominator between them.

[ivory|
  struct ivory_string
    { iv_str_data   :: DynArray (Stored Uint8)
    ; iv_str_length :: Stored Sint32
    }
|]

-- | Type alias for strings.
--
-- XXX it is confusing to have both 'IString' and 'IStr'.  We
-- might be able to get rid of 'IString' and automatically
-- create 'IStr' for string literals, but currently we need
-- to be in the Ivory monad to allocate an 'IStr'.
type IStr = Struct "ivory_string"

-- | Return a DynArray initializer for a string constant.
idynstring :: String -> Init (DynArray (Stored Uint8))
idynstring cs = idynarray (map (ival . fromIntegral . ord) cs)

-- | Return the length of a string.
istr_len :: Def ('[ConstRef s IStr] :-> Sint32)
istr_len = proc "ivory_string_len" $ \str -> body $ do
  capacity <- deref (dynArrayLength (str ~> iv_str_data))
  len      <- deref (str ~> iv_str_length)
  assert (len <=? capacity)
  ret len

----------------------------------------------------------------------
-- Creating Strings

-- | Initializer for an empty string (length 0, capacity 'n').
istr_empty :: Int -> Init IStr
istr_empty n = istruct
  [ iv_str_data   .= idynarray (replicate n (ival 0))
  , iv_str_length .= ival 0
  ]

-- | Initialize a string from a Haskell string (both length and
-- capacity are the Haskell string length).
istr_lit :: String -> Init IStr
istr_lit s = istruct
  [ iv_str_data   .= idynstring s
  , iv_str_length .= ival (fromIntegral (length s))
  ]

----------------------------------------------------------------------
-- Modifying Strings
--
-- TODO: Consider having all these functions return a boolean
-- that is true if the string fit in the destination, or false
-- if it was truncated.

-- | Primitive function to do a bounded string copy.
string_copy :: Def ('[ Ref      s1 (CArray (Stored Uint8))
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

-- | Copy one Ivory string into another.  The destination will be
-- truncated if it does not have enough capacity.
istr_copy :: Def ('[ Ref      s1 IStr
                   , ConstRef s2 IStr
                   ] :-> ())
istr_copy = proc "ivory_string_copy" $ \dest src -> body $ do
  src_data  <- assign (src  ~> iv_str_data)
  src_len   <- deref  (src  ~> iv_str_length)
  dest_data <- assign (dest ~> iv_str_data)
  withDynArrayData src_data $ \src_ptr _ -> do
    withDynArrayData dest_data $ \dest_ptr dest_len -> do
      result <- call string_copy dest_ptr dest_len src_ptr src_len
      assert (result <=? dest_len)
      store (dest ~> iv_str_length) result

-- | Copy the contents of a fixed-size C string into an Ivory
-- string.  If the source string is not null terminated (and
-- therefore corrupt), this will copy no more than 'len'
-- characters.
istr_from_sz :: forall s1 s2 eff len.
                SingI len
             => Ref      s2 IStr
             -> ConstRef s1 (Array len (Stored Uint8))
             -> Ivory eff ()
istr_from_sz str src = do
  str_data   <- assign (str ~> iv_str_data)
  let src_len = fromIntegral (fromSing (sing :: Sing len))
  withDynArrayData str_data $ \dest dest_len -> do
    result <- call string_copy_z dest dest_len (toCArray src) src_len
    assert (result <=? dest_len)
    store (str ~> iv_str_length) result

-- | Copy an Ivory string to a fixed-size, null-terminated C
-- string.  The destination string is always properly terminated,
-- but may be truncated if the buffer is too small.
sz_from_istr :: forall s1 s2 eff len.
                SingI len
             => Ref      s1 (Array len (Stored Uint8))
             -> ConstRef s2 IStr
             -> Ivory eff ()
sz_from_istr dest src = do
  let dest_capacity = fromSing (sing :: Sing len)
  M.when (dest_capacity > 0) $ do
    let dest_len  = fromIntegral (dest_capacity - 1)
    src_data     <- assign (src ~> iv_str_data)
    src_len      <- deref  (src ~> iv_str_length)

    withDynArrayData src_data $ \src_ptr _ -> do
      result <- call string_copy (toCArray dest) dest_len src_ptr src_len
      assert (result <=? dest_len)
      store (dest ! toIx (dest_len - 1)) 0

----------------------------------------------------------------------
-- Comparing Strings

-- | Binding to the C "memcmp" function (with some bogus types since
-- we need a signed size and can't express 'size_t').
memcmp :: Def ('[ ConstRef s1 (CArray (Stored Uint8))
                , ConstRef s2 (CArray (Stored Uint8))
                , Sint32] :-> Sint32)
memcmp = importProc "memcmp" "string.h"

-- | Compare two strings (byte dynarrays) for equality.
istr_eq :: Def ('[ ConstRef s1 IStr, ConstRef s2 IStr] :-> IBool)
istr_eq = proc "ivory_string_eq" $ \s1 s2 -> body $ do
  len1 <- deref (s1 ~> iv_str_length)
  len2 <- deref (s2 ~> iv_str_length)
  ifte_ (len1 /=? len2)
    (ret false)
    (do d1 <- assign (s1 ~> iv_str_data)
        d2 <- assign (s2 ~> iv_str_data)
        withDynArrayData d1 $ \p1 _ ->
          withDynArrayData d2 $ \p2 _ -> do
            r <- call memcmp p1 p2 len1
            ret (r ==? 0))

----------------------------------------------------------------------
-- Old String Functions

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
  incl istr_eq
  incl istr_len
  incl istr_copy
  defStruct (Proxy :: Proxy "ivory_string")
  sourceDep "ivory_stdlib_string_prim.h"
  sourceDep "ivory_stdlib_string_prim.c"

