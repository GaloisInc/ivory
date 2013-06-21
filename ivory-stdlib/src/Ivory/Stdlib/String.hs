{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
--
-- String.hs --- C-string utilities for Ivory.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.Stdlib.String
  ( copy_istring
  , strcpy
  , strncpy
  , strncpy_uint8
  , strncmp
  , stdlibStringModule
  ) where

import Ivory.Language

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
  sourceDep "ivory_stdlib_string_prim.h"
  sourceDep "ivory_stdlib_string_prim.c"

