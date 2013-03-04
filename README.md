# Radix sort based on binary representation - how-to

With msd prefix uses parallel (shorter time, higher space) Most significant digit radix sort
With lsd prefix uses (shorter space, higher time) Least significant digit radix sort

Sorting floats (Float, Double):

    {-# LANGUAGE PackageImports #-}

    import Data.List.RadixSort.Base (msdSortFloats)
    import Data.List.RadixSort.HasIndexFloat (HasIndexFloat(..))
    import Data.List as L
    import "random" System.Random
    import Control.Monad

    data FRec = FRec {campF:: Float} deriving (Eq, Show)

    instance HasIndexFloat FRec where
        indexFloat = campF

    floatExample = do

        llistaF1 <- replicateM 10 $ getStdRandom (randomR ((-100)::Float,100))
        let llistaF2 = L.map FRec llistaF1

        print llistaF1

        putStrLn "\n"
        print $ msdSortFloats llistaF1

        putStrLn "\n"
        print $ msdSortFloats llistaF2

    main = floatExample

Sorting integers (type IntN):

    {-# LANGUAGE PackageImports #-}

    import Data.List.RadixSort.Base (msdSortInts)
    import Data.List.RadixSort.HasIndexInt32 (HasIndexInt32(..))
    import Data.List as L
    import "random" System.Random
    import Control.Monad
    import Data.Int

    data IRec = IRec {campI:: Int32} deriving (Eq, Show)

    instance HasIndexInt32 IRec where
        indexInt32 = campI

    intExample = do

        llistaI1 <- replicateM 10 $ getStdRandom (randomR ((-100)::Int32,100))
        let llistaI2 = L.map IRec llistaI1

        print llistaI1

        putStrLn "\n"
        print $ msdSortInts llistaI1

        putStrLn "\n"
        print $ msdSortInts llistaI2


    main = intExample

Sorting naturals (type WordN):

    {-# LANGUAGE PackageImports #-}

    import Data.List.RadixSort.Base (msdSortNats)
    import Data.List.RadixSort.HasIndexWord32 (HasIndexWord32(..))
    import Data.List as L
    import "random" System.Random
    import Control.Monad
    import Data.Word

    data WRec = WRec {campW:: Word32} deriving (Eq, Show)

    instance HasIndexWord32 WRec where
        indexWord32 = campW

    wordExample = do

        llistaN1 <- replicateM 10 $ getStdRandom (randomR (0::Word32,100))
        let llistaN2 = L.map WRec llistaN1

        print llistaN1
        putStrLn "\n"
        print $ msdSortNats llistaN1

        putStrLn "\n"
        print $ msdSortNats llistaN2

    main = wordExample

