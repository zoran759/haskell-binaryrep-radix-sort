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

sortByDigit :: (RadixRep a) => SortData -> Int -> [a] -> DList a
sortByDigit _sortData _digit [] = D.empty
sortByDigit _sortData _digit [x] = D.singleton x

sortByDigit sortData digit list = runST $ do
        mvec <- V.thaw emptyVecOfSeqs
        -- partition by digit
        partListByDigit sortData digit list mvec
        vec <- V.freeze mvec
        if digit == 0
           then do
                return $ collectVecToDList vec topDigitVal D.empty
                
           else do
                let dlists = vec .$ V.toList
                                 .$ map F.toList
                                 .$ parMap rseq (sortByDigit sortData (digit-1))
                                 
                return $ D.concat dlists                      
  where
    emptyVecOfSeqs = V.replicate (topDigitVal+1) S.empty
    topDigitVal = sdTopDigitVal sortData
    
------------------------------------------
       
msdRadixSort :: (RadixRep a) => [a] -> [a]
msdRadixSort [] = []
msdRadixSort [x] = [x]
msdRadixSort list = assert (sizeOf (head list) `mod` bitsPerDigit == 0) $
   ( list .$ sortByDigit sortData topDigit
          .$ D.toList
   )
  where
    sortData = SortData {sdDigitSize = bitsPerDigit,
                         sdTopDigit = topDigit,
                         sdTopDigitVal = topDigitVal
                         }
    topDigit = (sizeOf $ L.head list) `div` bitsPerDigit - 1
    topDigitVal = bit bitsPerDigit -1
    bitsPerDigit = calcDigitSize list
          
        
