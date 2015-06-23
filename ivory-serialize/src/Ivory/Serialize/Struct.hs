{-# LANGUAGE DataKinds #-}

module Ivory.Serialize.Struct (
  labelPackRep,
  PackLabel, packLabel, packLabel',
  packStruct
) where

import Ivory.Language
import Ivory.Serialize.Atoms
import Ivory.Serialize.PackRep

labelPackRep :: Packable t => Label sym t -> PackRep t
labelPackRep _ = packRep

newtype PackLabel sym = PackLabel (PackRep (Struct sym))

packLabel :: (IvoryStruct sym, IvoryArea t, Packable t) => Label sym t -> PackLabel sym
packLabel label = packLabel' label $ labelPackRep label

packLabel' :: (IvoryStruct sym, IvoryArea t) => Label sym t -> PackRep t -> PackLabel sym
packLabel' label rep = PackLabel $ PackRep
  { packGetLE = \ buf offs str -> packGetLE rep buf offs (str ~> label)
  , packGetBE = \ buf offs str -> packGetBE rep buf offs (str ~> label)
  , packSetLE = \ buf offs str -> packSetLE rep buf offs (str ~> label)
  , packSetBE = \ buf offs str -> packSetBE rep buf offs (str ~> label)
  , packSize = packSize rep
  }

packStruct :: [PackLabel sym] -> PackRep (Struct sym)
packStruct labels = PackRep
  { packGetLE = foldPackLabels packGetLE labels
  , packGetBE = foldPackLabels packGetBE labels
  , packSetLE = foldPackLabels packSetLE labels
  , packSetBE = foldPackLabels packSetBE labels
  , packSize = sum $ map (\ (PackLabel rep) -> packSize rep) labels
  }

foldPackLabels :: Monad m => (PackRep (Struct str) -> buf -> Uint32 -> strref -> m ()) -> [PackLabel str] -> buf -> Uint32 -> strref -> m ()
foldPackLabels f labels buf base str = foldl once (return 0) labels >> return ()
  where
  once m (PackLabel rep) = do
    offs <- m
    f rep buf (base + fromInteger offs) str
    return $! offs + packSize rep
