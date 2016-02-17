{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
--
-- Maybe.hs --- Optional Ivory values.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--
-- This software is released under the "BSD3" license.  Read the file
-- "LICENSE" for more information.
--

-- | This module provides an interface for a nullable Ivory type.
--
-- To define a type like Haskell's @Maybe Float@, define an
-- Ivory structure type, and make the structure an instance
-- of 'MaybeType'.
--
-- > [ivory|
-- > struct maybe_float
-- >   { mf_valid :: Stored IBool
-- >   ; mf_value :: Stored IFloat
-- >   }
-- > |]
-- >
-- > instance MaybeType "maybe_float" IFloat where
-- >   maybeValidLabel = mf_valid
-- >   maybeValueLabel = mf_value
--
-- With this definition in place, any of the functions in this
-- module will accept a @Struct \"maybe_float\"@.
--
-- These structure types must be defined in an Ivory module as
-- usual, and it is recommended to make them private unless they
-- are necessary as part of the module's public interface.
module Ivory.Stdlib.Maybe
  (
  -- * Interface
  MaybeType(..)

  -- * Initialization
  , initJust, initNothing

  -- * Getting
  , getMaybe

  -- * Setting
  , setJust, setNothing
  , setDefault, setDefault_

  -- * Modifying
  , mapMaybe
  , mapMaybeM, mapMaybeM_
  , forMaybeM, forMaybeM_
  ) where

import GHC.TypeLits

import Ivory.Language

class (IvoryStruct sym, IvoryExpr t, IvoryStore t, IvoryInit t) =>
      MaybeType (sym :: Symbol) t | sym -> t where
  -- | Return a boolean field indicating whether the value is valid.
  maybeValidLabel :: Label sym ('Stored IBool)
  -- | Return the field containing a value, if it is valid.
  maybeValueLabel :: Label sym ('Stored t)

-- | Return an initializer for a maybe type with a valid value.
initJust :: MaybeType sym a => a -> Init ('Struct sym)
initJust x =
  istruct
    [ maybeValidLabel .= ival true
    , maybeValueLabel .= ival x
    ]

-- | Return an initializer for a maybe type with no value.
initNothing :: MaybeType sym a => Init ('Struct sym)
initNothing =
  istruct
    [ maybeValidLabel .= ival false
    ]

-- | Retrieve a maybe's value given a default if it is nothing.
getMaybe :: MaybeType sym a
         => ConstRef s1 ('Struct sym)
         -> a
         -> Ivory eff a
getMaybe ref def = do
  valid <- deref (ref ~> maybeValidLabel)
  value <- deref (ref ~> maybeValueLabel)
  assign (valid ? (value, def))

-- | Set a maybe's value to a default if it is nothing, returning
-- the current value.
setDefault :: MaybeType sym a
           => Ref s1 ('Struct sym)
           -> a
           -> Ivory eff a
setDefault ref def = do
  setDefault_ ref def
  deref (ref ~> maybeValueLabel)

-- | Set a maybe's value to a default value if it is nothing.
setDefault_ :: MaybeType sym a
            => Ref s1 ('Struct sym)
            -> a
            -> Ivory eff ()
setDefault_ ref def = do
  valid <- deref (ref ~> maybeValidLabel)
  ifte_ (iNot valid)
    (do store (ref ~> maybeValidLabel) true
        store (ref ~> maybeValueLabel) def)
    (return ())

-- | Modify a maybe value by an expression if it is not nothing.
mapMaybe :: MaybeType sym a
         => (a -> a)
         -> Ref s1 ('Struct sym)
         -> Ivory eff ()
mapMaybe f ref = mapMaybeM (return . f) ref

-- | Modify a maybe value by an action if it is not nothing.
mapMaybeM :: MaybeType sym a
          => (a -> Ivory eff a)
          -> Ref s1 ('Struct sym)
          -> Ivory eff ()
mapMaybeM f ref = do
  valid <- deref (ref ~> maybeValidLabel)
  ifte_ valid
    (do value  <- deref (ref ~> maybeValueLabel)
        value' <- f value
        store (ref ~> maybeValueLabel) value')
    (return ())

-- | Flipped version of 'mapMaybeM'.
forMaybeM :: MaybeType sym a
          => Ref s1 ('Struct sym)
          -> (a -> Ivory eff a)
          -> Ivory eff ()
forMaybeM = flip mapMaybeM

-- | Call an action with a maybe value if it is not nothing.
mapMaybeM_ :: MaybeType sym a
           => (a -> Ivory eff ())
           -> Ref s1 ('Struct sym)
           -> Ivory eff ()
mapMaybeM_ f ref = do
  valid <- deref (ref ~> maybeValidLabel)
  ifte_ valid
    (do value <- deref (ref ~> maybeValueLabel)
        f value)
    (return ())

-- | Flipped version of 'mapMaybeM_'.
forMaybeM_ :: MaybeType sym a
           => Ref s1 ('Struct sym)
           -> (a -> Ivory eff ())
           -> Ivory eff ()
forMaybeM_ = flip mapMaybeM_

-- | Set a maybe value to a valid value.
setJust :: MaybeType sym a
        => Ref s1 ('Struct sym)
        -> a
        -> Ivory eff ()
setJust ref x = do
  store (ref ~> maybeValidLabel) true
  store (ref ~> maybeValueLabel) x

-- | Set a maybe value to an invalid value.
setNothing :: MaybeType sym a
           => Ref s1 ('Struct sym)
           -> Ivory eff ()
setNothing ref = store (ref ~> maybeValidLabel) false

