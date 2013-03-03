
import Data.List.RadixSort.Base (sortInts, sortFloats, sortNats, RadixRep)
import Data.List.RadixSort.HasIndexFloat (HasIndexFloat(..))
import Data.List as L
import Data.Int
import Data.Word
import Data.Ord
import Test.QuickCheck
import System.Random

data Rec = Rec {fieldA:: Float} deriving (Eq, Show)

instance HasIndexFloat Rec where
  indexFloat = fieldA

instance Ord Rec where
  compare = comparing indexFloat

instance Random Rec where
  randomR (Rec lo, Rec hi)  g = (Rec x, g')
    where
      (x, g') = randomR (lo, hi) g
      
  random g = (Rec x, g')
    where
      (x, g') = random g
            

instance Arbitrary Rec where
  arbitrary = choose (Rec lo, Rec hi)
    where
      (lo, hi) = (2^^(loExp-1), 2^^(hiExp-1))
      (loExp, hiExp) = floatRange (1::Float)
            

checkOrdered :: (Ord a, RadixRep a) => [a] -> Bool    -- Eq a, Num a, 
checkOrdered [] = True
checkOrdered [_] = True
checkOrdered list = L.all orderedPair $ L.zip list (L.tail list)
  where
    orderedPair = uncurry (<=)

main :: IO ()
main = do
        let deepCheck p = quickCheckWith (stdArgs { maxSuccess = 100, maxSize = 50}) p

        putStrLn "sorting [Float]"
        deepCheck ((\s -> checkOrdered $ sortFloats s) :: [Float] -> Bool)
        putStrLn "sorting [Double]"
        deepCheck ((\s -> checkOrdered $ sortFloats s) :: [Double] -> Bool)

        putStrLn "sorting [Rec Float]"
        deepCheck ((\s -> checkOrdered $ sortFloats s) :: [Rec] -> Bool)
        
        
        putStrLn "sorting [Int8]"
        deepCheck ((\s -> checkOrdered $ sortInts s) :: [Int8] -> Bool)
        putStrLn "sorting [Int16]"
        deepCheck ((\s -> checkOrdered $ sortInts s) :: [Int16] -> Bool)
        putStrLn "sorting [Int32]"
        deepCheck ((\s -> checkOrdered $ sortInts s) :: [Int32] -> Bool)
        putStrLn "sorting [Int64]"
        deepCheck ((\s -> checkOrdered $ sortInts s) :: [Int64] -> Bool)

        putStrLn "sorting [Word8]"
        deepCheck ((\s -> checkOrdered $ sortNats s) :: [Word8] -> Bool)
        putStrLn "sorting [Word16]"
        deepCheck ((\s -> checkOrdered $ sortNats s) :: [Word16] -> Bool)
        putStrLn "sorting [Word32]"
        deepCheck ((\s -> checkOrdered $ sortNats s) :: [Word32] -> Bool)
        putStrLn "sorting [Word64]"
        deepCheck ((\s -> checkOrdered $ sortNats s) :: [Word64] -> Bool)
        
