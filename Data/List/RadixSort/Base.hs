{-# LANGUAGE PackageImports, FlexibleContexts, FlexibleInstances, UndecidableInstances, RankNTypes #-} -- , OverlappingInstances
{- | Radix sort of lists of floats (based on its IEEE754 representation) or Int<N> based on their representation

  Here we partition numbers by sign and sort both lists in parallel (you should link with -threaded)

  The digit size is set to 8 bits for lists > 512 elements, else 4, in order to save space when sorting small lists

  digit value queues are appended as difference lists (DList from package dlist)
-}
-- @author: Gabriel Riba Faura
module Data.List.RadixSort.Base (
  sortInts, sortFloats, sortNats,
  floatToWord, doubleToWord,
  RadixRep(..), SignedQual(..)
) where

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import Data.Bits
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

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Text.Printf (printf)

import Control.Exception (assert)
import qualified Control.Monad as M
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)
import "parallel" Control.Parallel (par, pseq)

(.$) :: forall b c. b -> (b -> c) -> c
(.$) = flip ($)

---------------------------------------------------------
-- floatToWord type conversion from Jacob Stanley answer in
-- question http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

-----------------------------------------------------------------

data SignedQual = Signed | Unsigned deriving (Eq, Show)

-- | class to instanciate for a type to be used in radix sorts
class RadixRep t where
        
  -- | obtain a word<N> representation, as for Float: toWordRep = fromIntegral . floatToWord      
  toWordRep :: (Bits a, Integral a) => t -> a

  -- | size of the type in bits
  sizeOf :: t -> Int

  -- | Signed / Unsigned
  signedQual :: t -> SignedQual
  
instance RadixRep Float where
  toWordRep = fromIntegral . floatToWord
  sizeOf _ = 32
  signedQual _ = Signed
  
instance RadixRep Double where
  toWordRep = fromIntegral . doubleToWord
  sizeOf _ = 64
  signedQual _ = Signed

-------------------------------

instance RadixRep Int8 where
  toWordRep = fromIntegral
  sizeOf _ = 8
  signedQual _ = Signed

instance RadixRep Int16 where
  toWordRep = fromIntegral
  sizeOf _ = 16
  signedQual _ = Signed

instance RadixRep Int32 where
  toWordRep = fromIntegral
  sizeOf _ = 32
  signedQual _ = Signed

instance RadixRep Int64 where
  toWordRep = fromIntegral
  sizeOf _ = 64
  signedQual _ = Signed

-------------------------------

instance RadixRep Word8 where
  toWordRep = fromIntegral
  sizeOf _ = 8
  signedQual _ = Unsigned

instance RadixRep Word16 where
  toWordRep = fromIntegral
  sizeOf _ = 16
  signedQual _ = Unsigned

instance RadixRep Word32 where
  toWordRep = fromIntegral
  sizeOf _ = 32
  signedQual _ = Unsigned

instance RadixRep Word64 where
  toWordRep = fromIntegral
  sizeOf _ = 64
  signedQual _ = Unsigned

------------------------------------------

wordGetDigitVal :: (Bits a, Integral a) => Int -> Int -> SignedQual -> Int -> a -> Int
wordGetDigitVal bitsPerDigit topDigit signed digit bits =
      assert (digit >= 0 && digit <= topDigit) $ fromIntegral digitVal
    where
      bitsToShift = digit * bitsPerDigit
      mask = if digit == topDigit && signed == Signed
              then shiftL (fromIntegral digitMaskSignExcl) bitsToShift
              else shiftL (fromIntegral digitMask) bitsToShift

      digitVal = shiftR (bits .&. mask) bitsToShift
      digitMask = bit bitsPerDigit -1 :: Word
      digitMaskSignExcl = (bit (bitsPerDigit-1) -1) :: Word   -- sign excluded

------------------------------------------

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

radixSort :: (RadixRep a) => [a] -> [a]
radixSort [] = []
radixSort list = assert (sizeOf (head list) `mod` bitsPerDigit == 0) $ runST $ do
        vecIni <- V.thaw emptyVecOfSeqs
        -- partition by digit 0
        partListByDigit bitsPerDigit topDigit 0 list vecIni
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
                    partListByDigit bitsPerDigit topDigit digit (F.toList s) vecTo
                    
                writeSTRef refVecFrom vecTo
                
        lastDigitSortedMVec <- readSTRef refVecFrom
        lastDigitSortedVec <- V.freeze lastDigitSortedMVec
        let dlist = collectVecToDList lastDigitSortedVec topDigitVal D.empty
        return $ D.toList dlist
  where
          
    emptyVecOfSeqs = V.replicate (topDigitVal+1) S.empty      
    topDigitVal = 2 ^ bitsPerDigit -1
    topDigit = (sizeOf $ L.head list) `div` bitsPerDigit - 1
    bitsPerDigit = let (_prefix, postfix) = L.splitAt 512 list in
                   if null postfix
                      then 4  -- use tiny vectors
                      else 8  -- use bigger vectors
        
------------------------------------------

-- partition by sign
partSign :: (RadixRep a) => [a] -> [a] -> [a] -> ([a], [a])
partSign poss negs [] = (poss, negs)
partSign poss negs (x:xs) = if isNeg x
                               then partSign poss (x:negs) xs
                               else partSign (x:poss) negs xs
  where                               
        isNeg y = case sizeOf y of
                        64 -> testBit (toWordRep y :: Word64) 63
                        32 -> testBit (toWordRep y :: Word32) 31
                        16 -> testBit (toWordRep y :: Word16) 15
                        8 ->  testBit (toWordRep y :: Word8) 7
                        other -> error $ printf "size %d not supported!" other

-- | sortFloats partitions between positive and negative and sort by binary repr. (exponent:mantissa) for each set in parallel,
-- reversing the negatives list after radixSort.
--
-- O(n), for each signed bag, plus sign partition, negatives reversing and reassembling
--
-- use this for Floats and Doubles
                      
sortFloats :: (RadixRep a) => [a] -> [a]
sortFloats [] = []
sortFloats list = sortedNegs `par` (sortedPoss `pseq` (sortedNegs L.++ sortedPoss))
  where
    (poss, negs) = partSign [] [] list
    sortedPoss = radixSort poss
    sortedNegs = negs .$ radixSort 
                      .$ L.reverse

-- | sortInts partitions between positive and negative and sort each set in parallel
-- 
-- O(n), for each signed bag, plus sign partition and reassembling
-- 
-- use this for Int<N> types
sortInts :: (RadixRep a) => [a] -> [a]
sortInts [] = []
sortInts list = sortedNegs `par` (sortedPoss `pseq` (sortedNegs L.++ sortedPoss))
  where
    (poss, negs) = partSign [] [] list
    sortedPoss = radixSort poss
    sortedNegs = radixSort negs

-- | sortNats, O(n)
-- 
-- use this for Word<N> types
sortNats :: (RadixRep a) => [a] -> [a]
sortNats [] = []
sortNats list = radixSort list

        