{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexWord (HasIndexWord(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Word

-- | class to implement
class HasIndexWord t where
  indexWord :: t -> Word

instance HasIndexWord t => RadixRep t where
  toWordRep = toWordRep . indexWord
  sizeOf = sizeOf . indexWord
  signedQual = signedQual . indexWord
