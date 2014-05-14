{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Language.String where

import Ivory.Language.Area
import Ivory.Language.Array
import Ivory.Language.Struct
import Ivory.Language.Uint
import Ivory.Language.Proxy (ANat)

import GHC.TypeLits(Nat)

class ( ANat (Capacity a)
      , IvoryStruct (StructName a)
      , IvoryArea a
      , a ~ Struct (StructName a)
      ) => IvoryString a where
  type Capacity a :: Nat

  stringDataL   :: Label (StructName a) (Array (Capacity a) (Stored Uint8))
  stringLengthL :: Label (StructName a) (Stored (Ix (Capacity a)))

