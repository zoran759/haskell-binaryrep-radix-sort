# Radix sort how-to


    {-#LANGUAGE PackageImports #-}
    
    import Data.List.RadixSort.Base
    import Data.List.RadixSort.HasIndexFloat
    import Data.List as L
    import "random" System.Random
    import Control.Monad
    import Data.Int
    import Data.Word
    
    data FRec = FRec {campF:: Float} deriving (Eq, Show)
    
    instance HasIndexFloat FRec where
        indexFloat = campF
    
    floatExample = do
    
        llistaF1 <- replicateM 10 $ getStdRandom (randomR ((-100)::Float,100))
        let llistaF2 = L.map FRec llistaF1
    
        print llistaF1
        putStrLn "\n"
        print $ sortFloats llistaF2
    
    ----------------------------------------------------
    data IRec = IRec {campI:: Int32} deriving (Eq, Show)
    
    instance HasIndexInt32 IRec where
        indexInt32 = campI
    
    intExample = do
    
        llistaI1 <- replicateM 10 $ getStdRandom (randomR ((-100)::Int32,100))
        let llistaI2 = L.map IRec llistaI1
    
        print llistaI1
        putStrLn "\n"
        print $ sortInts llistaI2
    
    ----------------------------------------------------
    data WRec = WRec {campW:: Word32} deriving (Eq, Show)
    
    instance HasIndexWord32 WRec where
        indexWord32 = campW
    
    wordExample = do
    
        llistaN1 <- replicateM 10 $ getStdRandom (randomR (0::Int32,100))
        let llistaN2 = L.map WRec llistaN1
    
        print llistaN1
        putStrLn "\n"
        print $ sortNats llistaN2
    
    ----------------------------------------------------
    main = do
        floatExample
        intExample
        wordExample 

