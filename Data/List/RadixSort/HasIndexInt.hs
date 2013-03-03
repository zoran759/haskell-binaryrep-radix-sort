{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexInt (HasIndexInt(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Int

-- | class to implement
class HasIndexInt t where
  indexInt :: t -> Int

instance HasIndexInt t => RadixRep t where
  toWordRep = toWordRep . indexInt
  sizeOf = sizeOf . indexInt
  signedQual = signedQual . indexInt
