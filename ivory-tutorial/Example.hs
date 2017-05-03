-- stack runghc
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Example where

import Prelude hiding (when,unless)
import Ivory.Language
import Ivory.Stdlib.Control (when)

-- define the example module, our compilation unit
exampleModule =
  package "example" $
    do incl ivoryMain

ivoryMain :: Def ('[] ':-> Sint32)
ivoryMain  =
  proc "main" $
  body $
    do ret 0
