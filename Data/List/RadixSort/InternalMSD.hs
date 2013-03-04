{-# LANGUAGE PackageImports #-}
-- | Most significant digit radix sort
module Data.List.RadixSort.InternalMSD (msdRadixSort) where

import Data.List.RadixSort.Common
import Data.List.RadixSort.Util

import qualified Data.List as L
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import "dlist" Data.DList (DList)
import qualified "dlist" Data.DList as D
import "vector" Data.Vector (Vector)
import qualified "vector" Data.Vector as V
import "vector" Data.Vector.Mutable (MVector)
import qualified "vector" Data.Vector.Mutable as VM

-- import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Text.Printf (printf)

import GHC.ST (runST, ST)
import Control.Exception (assert)
-- import qualified Control.Monad as M
import "parallel" Control.Parallel.Strategies



partListByDigit :: (RadixRep a) => Int -> Int -> Int -> [a] -> MVector s (Seq a) -> ST s ()
partListByDigit _bitsPerDigit _topDigit _digit [] _vec = return ()
partListByDigit bitsPerDigit topDigit digit (x:xs) vec = do
        s <- VM.read vec digitVal
        VM.write vec digitVal (s S.|> x)
        partListByDigit bitsPerDigit topDigit digit xs vec
        return ()
      where
        digitVal = case sizeOf x of
                        64 -> wordGetDigitVal bitsPerDigit topDigit signedQ digit $ (toWordRep x :: Word64)
                        32 -> wordGetDigitVal bitsPerDigit topDigit signedQ digit $ (toWordRep x :: Word32)
                        16 -> wordGetDigitVal bitsPerDigit topDigit signedQ digit $ (toWordRep x :: Word16)
                        8 -> wordGetDigitVal bitsPerDigit topDigit signedQ digit $ (toWordRep x :: Word8)
                        other -> error $ printf "size %d not supported!" other

        signedQ = signedQual x

------------------------------------------

collectVecToDList :: Vector (Seq a) -> Int -> DList a -> DList a
collectVecToDList vec n dl =
        if n == 0
           then new_accum_dl
           else collectVecToDList vec (n-1) new_accum_dl
      where
        dlFromSeq s = D.fromList $ F.toList s
        new_accum_dl = dln `D.append` dl
        dln = dlFromSeq $ vec V.! n

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
    topDigitVal = 2 ^ bitsPerDigit -1
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
    bitsPerDigit = let (_prefix, postfix) = L.splitAt 512 list in
                   if null postfix
                      then 4  -- use small vectors
                      else 8  -- use bigger vectors
          
        
