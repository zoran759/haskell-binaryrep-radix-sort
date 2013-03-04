{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Data.List.RadixSort.Common (
  RadixRep(..), SignedQual(..),
  floatToWord, doubleToWord,
  (.$)
) where

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Bits

-- import Control.Exception (assert)

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

instance RadixRep Int where
  toWordRep = fromIntegral
  sizeOf x = bitSize (fromIntegral x ::Word)
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

instance RadixRep Word where
  toWordRep = fromIntegral
  sizeOf = bitSize
  signedQual _ = Unsigned

------------------------------------------
{-
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
                        -}

