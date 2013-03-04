module Data.List.RadixSort.Internal.Util where

import Data.Bits
import Data.List.RadixSort.Internal.Common
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Control.Exception (assert)
import Text.Printf (printf)

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
