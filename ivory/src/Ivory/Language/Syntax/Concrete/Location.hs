{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

--
-- Ivory concrete syntax locations.
--
-- Copyright (C) 2014, Galois, Inc.
-- All rights reserved.
--

module Ivory.Language.Syntax.Concrete.Location where

import Prelude ()
import Prelude.Compat

import Control.Monad (mplus)
import Data.Function (on)
import Data.List (foldl')
import Data.Maybe (maybeToList)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import qualified Text.PrettyPrint as P

import Ivory.Language.Syntax.Concrete.Pretty

-- Located Things --------------------------------------------------------------

class HasLocation a where
  getLoc :: a -> SrcLoc
  stripLoc :: a -> a

instance HasLocation a => HasLocation [a] where
  {-# INLINE getLoc #-}
  getLoc = foldMap getLoc

  {-# INLINE stripLoc #-}
  stripLoc = fmap stripLoc

instance (HasLocation a, HasLocation b) => HasLocation (a,b) where
  {-# INLINE getLoc #-}
  getLoc (a,b) = getLoc a `mappend` getLoc b

  {-# INLINE stripLoc #-}
  stripLoc (a,b) = (stripLoc a, stripLoc b)

instance HasLocation a => HasLocation (Maybe a) where
  {-# INLINE getLoc #-}
  getLoc = foldMap getLoc

  {-# INLINE stripLoc #-}
  stripLoc = fmap stripLoc

locStart :: HasLocation a => a -> Position
locStart a = srcStart (getLoc a)

locEnd :: HasLocation a => a -> Position
locEnd a = srcEnd (getLoc a)

-- | Things that carry a range in the source syntax.
data Located a = Located
  { locRange :: !SrcLoc
  , locValue ::  a
  } deriving (Show,Read,Functor,Ord,Eq,Foldable,Traversable)

instance HasLocation (Located a) where
  {-# INLINE getLoc #-}
  getLoc = locRange

  {-# INLINE stripLoc #-}
  stripLoc l = l { locRange = NoLoc }

-- | Attach no location information to a value.
noLoc :: a -> Located a
noLoc a = Located
  { locRange = NoLoc
  , locValue = a
  }

-- | Attach location information to a value.
at :: HasLocation loc => a -> loc -> Located a
at a loc = Located
  { locRange = getLoc loc
  , locValue = a
  }

-- | `at` helper for binary operators.
atBin :: (HasLocation loc0, HasLocation loc1)
      => a -> loc0 -> loc1 -> Located a
atBin a l0 l1 = a `at` (getLoc l0 <> getLoc l1)

-- | `at` helper for list args.
atList :: a -> [SrcLoc] -> Located a
atList a locs = a `at` mconcat locs

-- | Strip off location information.
unLoc :: Located a -> a
unLoc  = locValue

-- | Extend the range of a located thing.
extendLoc :: SrcLoc -> Located a -> Located a
extendLoc r loc = loc { locRange = locRange loc `mappend` r }

-- Source Locations ------------------------------------------------------------

-- | Source locations.
type Source = Maybe String

-- | A range in the program source.
data SrcLoc = NoLoc | SrcLoc !Range Source
    deriving (Show,Read,Ord,Eq)

instance Pretty SrcLoc where
  pretty NoLoc = P.lbrack P.<+> P.text "No location available" P.<+> P.rbrack
  pretty (SrcLoc rng msrc) =
    case msrc of
      Nothing  -> pretty rng
      Just src -> pretty src P.<> P.colon P.<> pretty rng

instance HasLocation SrcLoc where
  {-# INLINE getLoc #-}
  getLoc = id

  {-# INLINE stripLoc #-}
  stripLoc _ = NoLoc

instance Semigroup SrcLoc where
  -- widen source ranges, and prefer source names from the left
  SrcLoc lr ls <> SrcLoc rr rs = SrcLoc (lr <> rr) (mplus ls rs)
  NoLoc        <> r              = r
  l            <> NoLoc          = l

instance Monoid SrcLoc where
  mempty = NoLoc
  mappend = (<>)

-- | Get info to build a line pragma from a 'SrcLoc'. Returns a value only if
-- there is a valid range. Returns the starting line number.
srcLoclinePragma :: SrcLoc -> Maybe (Int, String)
srcLoclinePragma srcloc = case srcloc of
  NoLoc        -> Nothing
  SrcLoc _ src -> Just ( posLine (srcStart srcloc)
                       , concat (maybeToList src))

srcRange :: SrcLoc -> Range
srcRange loc = case loc of
  SrcLoc r _ -> r
  NoLoc      -> mempty

-- | Starting Position of a 'SrcLoc'.
srcStart :: SrcLoc -> Position
srcStart loc = case loc of
  SrcLoc r _ -> rangeStart r
  NoLoc      -> zeroPosition

-- | Ending Position of a 'SrcLoc'.
srcEnd :: SrcLoc -> Position
srcEnd loc = case loc of
  SrcLoc r _ -> rangeStart r
  NoLoc      -> zeroPosition


-- Ranges ----------------------------------------------------------------------

-- | The region between to source positions.
data Range = Range
  { rangeStart :: !Position
  , rangeEnd   :: !Position
  } deriving (Show,Read,Eq,Ord)

instance Semigroup Range where
  -- widen the range
  Range ls le <> Range rs re = Range (smallerOf ls rs) (largerOf le re)

instance Monoid Range where
  mempty = Range zeroPosition zeroPosition
  mappend = (<>)

instance Pretty Range where
  pretty (Range st end) =
    if st == end
      then pretty st
      else pretty st P.<+> P.char '-' P.<+> pretty end

-- Positions -------------------------------------------------------------------

-- | Position information within a source.
data Position = Position
  { posOff  :: !Int
  , posLine :: !Int
  , posCol  :: !Int
  } deriving (Show,Read,Eq)

-- | This only compares offset, assuming that the positions come from the same
-- source.
instance Ord Position where
  compare = compare `on` posOff

instance Pretty Position where
  pretty (Position _ ln col) =
    pretty ln P.<> P.colon P.<> pretty col

-- | Starting position.
zeroPosition :: Position
zeroPosition  = Position
  { posOff  = 0
  , posLine = 1
  , posCol  = 1
  }

-- | Return smaller of the two positions, taking care to not allow the zero
-- position to dominate.
smallerOf :: Position -> Position -> Position
smallerOf l r
  | l < r && l /= zeroPosition = l
  | otherwise                  = r

-- | Return the larger of the two positions.
largerOf :: Position -> Position -> Position
largerOf l r
  | l > r     = l
  | otherwise = r

-- | Given a character, increment a position.
movePos :: Position -> Char -> Position
movePos (Position off line col) c =
  case c of
    '\t' -> Position (off+1)  line    (col+8)
    '\n' -> Position (off+1) (line+1)  1
    _    -> Position (off+1)  line    (col+1)

-- | Move many characters at once.
movesPos :: Position -> String -> Position
movesPos pos = foldl' movePos pos
