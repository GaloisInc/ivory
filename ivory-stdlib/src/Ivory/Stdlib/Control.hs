{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Stdlib.Control
  ( ifte
  , when
  , unless
  ) where

import Ivory.Language

ifte :: (eff `AllocsIn` s, IvoryStore a, IvoryZero (Stored a))
     => IBool -> Ivory eff a -> Ivory eff a -> Ivory eff a
ifte c t f = do
  r <- local izero
  ifte_ c
    (t >>= (store r))
    (f >>= (store r))
  deref r

when :: IBool -> Ivory eff () -> Ivory eff ()
when c t = ifte_ c t (return ())

unless :: IBool -> Ivory eff () -> Ivory eff ()
unless c f =  ifte_ c (return ()) f
