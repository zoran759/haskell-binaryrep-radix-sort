{-# LANGUAGE PackageImports #-}
-- | Most significant digit radix sort
module Data.List.RadixSort.Internal.MSD (msdRadixSort) where

import Data.List.RadixSort.Internal.Common
import Data.List.RadixSort.Internal.Util

import Data.Bits
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import "dlist" Data.DList (DList)
import qualified "dlist" Data.DList as D
import qualified "vector" Data.Vector as V

import GHC.ST (runST)
import Control.Exception (assert)
import "parallel" Control.Parallel.Strategies


------------------------------------------

sortByDigit :: (RadixRep a) => Int -> Int -> [a] -> DList a
sortByDigit _bitsPerDigit _digit [] = D.empty
sortByDigit _bitsPerDigit _digit [x] = D.singleton x

sortByDigit bitsPerDigit digit list = runST $ do
        mvec <- V.thaw emptyVecOfSeqs
        -- partition by digit
        partListByDigit bitsPerDigit topDigit digit list mvec
        vec <- V.freeze mvec
        if digit == 0
           then do
                return $ collectVecToDList vec topDigitVal D.empty
                
           else do
                let dlists = (V.toList vec) .$ map F.toList
                                      .$ parMap rseq (sortByDigit bitsPerDigit (digit-1))
                return $ D.concat dlists                      
  where
    emptyVecOfSeqs = V.replicate (topDigitVal+1) S.empty
    topDigitVal = bit bitsPerDigit -1
    topDigit = (sizeOf $ L.head list) `div` bitsPerDigit - 1
    
------------------------------------------
       
msdRadixSort :: (RadixRep a) => [a] -> [a]
msdRadixSort [] = []
msdRadixSort [x] = [x]
msdRadixSort list = assert (sizeOf (head list) `mod` bitsPerDigit == 0) $
   ( list .$ sortByDigit bitsPerDigit topDigit
          .$ D.toList
   )
  where
    topDigit = (sizeOf $ L.head list) `div` bitsPerDigit - 1
    bitsPerDigit = calcDigitSize list
          
        
