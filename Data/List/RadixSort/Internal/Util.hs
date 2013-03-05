{-# LANGUAGE PackageImports #-}
module Data.List.RadixSort.Internal.Util (
  partBySign,
  calcDigitSize,
  partListByDigit,
  collectVecToDList
) where

import Data.Bits
import Data.List.RadixSort.Internal.Common

import Data.Word (Word, Word8, Word16, Word32, Word64)
import Control.Exception (assert)
import Text.Printf (printf)

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

import GHC.ST (ST)

------------------------------------------

------------------------------------------

-- partition by sign
partBySign :: (RadixRep a) => [a] -> [a] -> [a] -> ([a], [a])
partBySign poss negs [] = (poss, negs)
partBySign poss negs (x:xs) = if isNeg x
                               then partBySign poss (x:negs) xs
                               else partBySign (x:poss) negs xs
  where
        isNeg y = case sizeOf y of
                        64 -> testBit (toWordRep y :: Word64) 63
                        32 -> testBit (toWordRep y :: Word32) 31
                        16 -> testBit (toWordRep y :: Word16) 15
                        8 ->  testBit (toWordRep y :: Word8) 7
                        other -> error $ printf "size %d not supported!" other
                        
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

calcDigitSize :: [a] -> Int
calcDigitSize list =
  let (_prefix, postfix1) = L.splitAt 16 list
  in if null postfix1
     then 2  -- use shorter vectors
     else let (_prefix, postfix2) = L.splitAt 512 list
          in if null postfix2
                then 4  -- use small vectors
                else 8  -- use bigger vectors
                        
