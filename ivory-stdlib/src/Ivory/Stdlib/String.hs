{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

--
-- String.hs --- C-string utilities for Ivory.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.Stdlib.String
  ( stdlibStringModule, stringInit, istr_eq, sz_from_istr, istr_len
  , istr_from_sz, istr_copy, string_lit_store
  , stdlibStringArtifacts, string_lit_array
  ) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Language.Array (IxRep)
import Ivory.Artifact
import Ivory.Language.Struct

import qualified Control.Monad as M
import qualified Paths_ivory_stdlib as P

-- Should be same underlying type as IxRep to make casting easier.
type Len = IxRep

----------------------------------------------------------------------
-- Yet Another Ivory String Type

undefinedRef :: Label name field -> Ref s field
undefinedRef _ = undefined

-- | String initialization. Error returned if the `String` is too large.
stringInit :: IvoryString str => String -> Init str
stringInit xs
  | len > nat =
    error $ "stringInit: String " ++ show xs
      ++ " is too large to initialize dynamic string with"
      ++ " maximum size " ++ show nat
  | otherwise =
    istruct
      [ l_data .= iarray (map ival (stringArray xs))
      , stringLengthL .= ival (fromInteger len)
      ]
  where
  l_data = stringDataL
  len = toInteger (length xs)
  nat = arrayLen (undefinedRef l_data)

-- | Store a constant string into an `IvoryString`. Error returned if the
-- `String` is too large.
string_lit_store :: IvoryString str
                 => String
                 -> Ref s str
                 -> Ivory eff ()
string_lit_store s str = do
  string_lit_array s (str ~> stringDataL)
  store (str ~> stringLengthL) $ fromIntegral $ length s

-- | Copy a Haskell string directly to an array of uint8s.
string_lit_array :: ANat n
                 => String
                 -> Ref s ('Array n ('Stored Uint8))
                 -> Ivory eff ()
string_lit_array s arr =
  let go (ix, c) = store (arr ! fromInteger ix) c in
  let ls = stringArray s in
  let ln = toInteger (length ls) in
  let nat = arrayLen arr in
  if ln > nat
    then error $ "string_lit_array: String " ++ show s
      ++ " is too large for the dynamic string max size "
      ++ show nat
    else mapM_ go (zip [0..] ls)

stringArray :: String -> [Uint8]
stringArray = map (fromIntegral . ord)

----------------------------------------------------------------------
-- Generic Functions

stringCapacity :: ( IvoryString str
                  , IvoryRef ref
                  , IvoryExpr (ref s ('Struct (StructName str)))
                  , IvoryExpr (ref s ('Array (Capacity ('Struct (StructName str))) ('Stored Uint8)))
                  , Num n
                  )
               => ref s str -> n
stringCapacity str = arrayLen (str ~> stringDataL)

stringData :: ( IvoryString str
              , IvoryRef ref
              , IvoryExpr (ref s ('Array (Capacity str) ('Stored Uint8)))
              , IvoryExpr (ref s ('CArray ('Stored Uint8)))
              , IvoryExpr (ref s str))
           => ref s str -> ref s ('CArray ('Stored Uint8))
stringData x = toCArray (x ~> stringDataL)

-- XXX don't export
-- | Binding to the C "memcmp" function.
memcmp :: Def ('[ ConstRef s1 ('CArray ('Stored Uint8))
                , ConstRef s2 ('CArray ('Stored Uint8))
                , Len] ':-> Len)
memcmp = importProc "memcmp" "string.h"

-- XXX don't export
-- | Binding to the C "memcpy" function.
memcpy :: Def ('[ Ref      s1 ('CArray ('Stored Uint8))
                , ConstRef s2 ('CArray ('Stored Uint8))
                , Len] ':-> Len)
memcpy = importProc "memcpy" "string.h"

-- | Return the length of a string.
istr_len :: IvoryString str
         => ConstRef s str
         -> Ivory eff Len
istr_len str = deref (str ~> stringLengthL)

-- | Copy one string into another of the same type.
istr_copy :: IvoryString str
          => Ref      s1 str
          -> ConstRef s2 str
          -> Ivory eff ()
istr_copy dest src = do
  len <- istr_len src
  call_ memcpy (stringData dest) (stringData src) len
  store (dest ~> stringLengthL) len

-- | Internal function to compare strings for equality.
do_istr_eq :: Def ('[ ConstRef s1 ('CArray ('Stored Uint8))
                    , Len
                    , ConstRef s2 ('CArray ('Stored Uint8))
                    , Len
                    ] ':-> IBool)
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

-- | Primitive function to do a bounded null-terminated string copy.
string_copy_z :: Def ('[ Ref s1 ('CArray ('Stored Uint8))
                       , Len
                       , ConstRef s2 ('CArray ('Stored Uint8))
                       , Len] ':-> Len)
string_copy_z = importProc "ivory_stdlib_string_copy_z"
                           "ivory_stdlib_string_prim.h"

-- | Copy the contents of a fixed-size C string into an Ivory
-- string.  If the source string is not null terminated (and
-- therefore corrupt), this will copy no more than 'len'
-- characters.
--
-- FIXME: This should return false if the string was truncated.
istr_from_sz :: (ANat len, IvoryString str)
             => Ref      s1 str
             -> ConstRef s2 ('Array len ('Stored Uint8))
             -> Ivory eff ()
istr_from_sz dest src = do
  let len1 = stringCapacity dest
  let ptr1 = stringData dest
  let len2 = arrayLen src
  let ptr2 = toCArray src
  result <- call string_copy_z ptr1 len1 ptr2 len2
  store (dest ~> stringLengthL) result

-- | Copy an Ivory string to a fixed-size, null-terminated C
-- string.  The destination string is always properly terminated,
-- but may be truncated if the buffer is too small.
--
-- FIXME: This should return false if the string was truncated.
sz_from_istr :: (ANat len, IvoryString str)
             => Ref      s1 ('Array len ('Stored Uint8))
             -> ConstRef s2 str
             -> Ivory eff ()
sz_from_istr dest src = do
  let dest_capacity = arrayLen dest
  M.when (dest_capacity > 0) $ do
    -- leave room for a trailing NUL in dest
    let dest_len = fromInteger (dest_capacity - 1)
    src_len  <- istr_len src
    let src_capacity = stringCapacity src
    -- this can never truncate if dest is at least one byte bigger than src
    let truncated = if dest_capacity > src_capacity then false else dest_len <? src_len
    let result_len = truncated ? (dest_len, src_len)
    call_ memcpy (toCArray dest) (stringData src) result_len
    store (dest ! toIx result_len) 0

-- | Ivory module definition.
stdlibStringModule :: Module
stdlibStringModule = package "ivory_stdlib_string" $ do
  incl memcmp
  incl memcpy
  incl do_istr_eq
  incl string_copy_z

stdlibStringArtifacts :: [Located Artifact]
stdlibStringArtifacts =
  [ Incl $ supportfile "ivory_stdlib_string_prim.h"
  , Src $ supportfile "ivory_stdlib_string_prim.c"
  ]
  where
  supportfile f = artifactCabalFile P.getDataDir ("support/" ++ f)

