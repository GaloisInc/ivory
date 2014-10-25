{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Ivory.Language.Syntax.Type where

import GHC.Generics (Generic)
import Language.Haskell.TH.Lift (deriveLiftMany)


-- Types -----------------------------------------------------------------------

data Type
  = TyVoid             -- ^ Unit type
  | TyInt IntSize      -- ^ Signed ints
  | TyWord WordSize    -- ^ Unsigned ints
  | TyIndex Integer    -- ^ Indices with an upper bound
  | TyBool             -- ^ Booleans
  | TyChar             -- ^ Characters
  | TyFloat            -- ^ Floats
  | TyDouble           -- ^ Doubles
  | TyProc Type [Type] -- ^ Procedures
  | TyRef Type         -- ^ References
  | TyConstRef Type    -- ^ Constant References
  | TyPtr Type         -- ^ Pointers
  | TyArr Int Type     -- ^ Arrays
  | TyStruct String    -- ^ Structures
  | TyCArray Type      -- ^ C Arrays
  | TyOpaque           -- ^ Opaque type---not implementable.
    deriving (Show, Eq, Ord, Generic)

data IntSize
  = Int8
  | Int16
  | Int32
  | Int64
  deriving (Show,Eq,Ord,Generic)


data WordSize
  = Word8
  | Word16
  | Word32
  | Word64
  deriving (Show,Eq,Ord,Generic)


data Typed a = Typed
  { tType  :: Type
  , tValue :: a
  } deriving (Show,Functor,Eq,Ord, Generic)


-- TH Lifting ------------------------------------------------------------------

deriveLiftMany [ ''Type, ''IntSize, ''WordSize, ''Typed ]
