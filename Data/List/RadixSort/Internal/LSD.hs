{-# LANGUAGE PackageImports #-}
-- | Least significant digit radix sort
module Data.List.RadixSort.Internal.LSD (lsdRadixSort) where

import Data.List.RadixSort.Internal.Common
import Data.List.RadixSort.Internal.Util

import Data.Bits
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified "dlist" Data.DList as D
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Mutable as VM

import GHC.ST (runST)
import Control.Exception (assert)
import qualified Control.Monad as M
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)

------------------------------------------

lsdRadixSort :: (RadixRep a) => [a] -> [a]
lsdRadixSort [] = []
lsdRadixSort [x] = [x]
lsdRadixSort list = assert (sizeOf (head list) `mod` bitsPerDigit == 0) $ runST $ do
        vecIni <- V.thaw emptyVecOfSeqs
        -- partition by digit 0
        partListByDigit sortData 0 vecIni list
        refVecFrom <- newSTRef vecIni

        M.when (topDigit > 0) $
           M.forM_ [1..topDigit] $ \digit -> do
                -- sort by digit
                vecFrom <- readSTRef refVecFrom
                vecTo <- V.thaw emptyVecOfSeqs

                M.forM_ [0..topDigitVal] $ \digitVal -> do
                    -- read vecFrom queue
                    s <- VM.read vecFrom digitVal
                    -- partition to vecTo queues
                    partListByDigit sortData digit vecTo (F.toList s)

                writeSTRef refVecFrom vecTo

        lastDigitSortedMVec <- readSTRef refVecFrom
        lastDigitSortedVec <- V.freeze lastDigitSortedMVec
        let dlist = collectVecToDList lastDigitSortedVec topDigitVal D.empty
        return $ D.toList dlist
  where

    emptyVecOfSeqs = V.replicate (topDigitVal+1) S.empty
    
    sortData = SortData {sdDigitSize = bitsPerDigit,
                         sdTopDigit = topDigit,
                         sdTopDigitVal = topDigitVal
                         }
    topDigitVal = bit bitsPerDigit -1
    topDigit = (sizeOf $ L.head list) `div` bitsPerDigit - 1
    bitsPerDigit = calcDigitSize list
