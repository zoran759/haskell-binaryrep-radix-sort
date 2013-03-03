{-# LANGUAGE PackageImports #-}

-- {-# LANGUAGE PackageImports, FlexibleContexts, FlexibleInstances, UndecidableInstances, RankNTypes #-} -- , OverlappingInstances
{- | Least significant digit radix sort (O(k n) where k= #digits) of lists of floats (based on its IEEE754 representation) or Int<N> or Word<N> based on their representation

  Here we partition numbers by sign and sort both lists in parallel (you should link with -threaded)

  The digit size is set to 8 bits for lists > 512 elements, else 4, in order to save space when sorting small lists

  digit value queues are appended as difference lists (DList from package dlist, O(1) on append)

  The instance for 'Int' (machine word size) must be used with reserve because it may have reserved bits for compiler use.
  Check "The word size story." at <http://www.haskell.org/ghc/docs/7.2.2/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html#g:1>

  A quickcheck test-suite has been added.

  See <http://en.wikipedia.org/wiki/Radix_sort>
-}
-- @author: Gabriel Riba Faura
module Data.List.RadixSort.Base (
  sortInts, sortFloats, sortNats,
  floatToWord, doubleToWord,
  RadixRep(..), SignedQual(..)
) where

import Data.List.RadixSort.Common
import Data.List.RadixSort.InternalLSD (radixSort)

import qualified Data.List as L
import "parallel" Control.Parallel (par, pseq)

------------------------------------------
        
------------------------------------------

-- | sortFloats partitions between positive and negative and sort by binary repr. (exponent:mantissa) for each set in parallel,
-- reversing the negatives list after radixSort.
--
-- O(k n) where k= #digits, for each signed bag, plus sign partition, negatives reversing and reassembling
--
-- use this for Floats and Doubles
                      
sortFloats :: (RadixRep a) => [a] -> [a]
sortFloats [] = []
sortFloats list = sortedNegs `par` (sortedPoss `pseq` (sortedNegs L.++ sortedPoss))
  where
    (poss, negs) = partBySign [] [] list
    sortedPoss = radixSort poss
    sortedNegs = negs .$ radixSort 
                      .$ L.reverse

-- | sortInts partitions between positive and negative and sort each set in parallel
-- 
-- O(k n) where k= #digits, for each signed bag, plus sign partition and reassembling
-- 
-- use this for Int<N> types
sortInts :: (RadixRep a) => [a] -> [a]
sortInts [] = []
sortInts list = sortedNegs `par` (sortedPoss `pseq` (sortedNegs L.++ sortedPoss))
  where
    (poss, negs) = partBySign [] [] list
    sortedPoss = radixSort poss
    sortedNegs = radixSort negs

-- | sortNats, O(k n) where k= #digits
-- 
-- use this for Word<N> types
sortNats :: (RadixRep a) => [a] -> [a]
sortNats [] = []
sortNats list = radixSort list

        