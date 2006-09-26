{- Classifying handwritten digits using Mahalanobis distance.
   To download the data file:
        $ wget http://dis.um.es/~alberto/material/sp/mnist.txt.gz
        $ gunzip mnist.txt.gz
-}

import GSL
import Vision

work (train,test) method = do
    let  c = fst $ method train
    putStr "Estimated error probability: "
    print $ errorRate test c
    putStrLn "Confusion matrix: "
    print $ confusion test c

comparedist (train,test) codec = do
    let problem = (preprocess codec train, preprocess codec test)
    putStr "Reduced dimension: "
    print $ size $ fst $ head $ fst problem
    putStrLn "-- with Mahalanobis distance --"
    work problem (distance mahalanobis)
    putStrLn "-- with Mahalanobis distance + log det sigma --"
    work problem (distance mahalanobis')
    putStrLn "-- with ordinary distance --"
    work problem (distance ordinary)

main = do
    m <- gslReadMatrix "mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let rawproblem = splitAt 4000 mnist

    putStr "Original dimension: "
    print $ size $ fst $ head $ fst rawproblem

    let st = stat (fromRows $ map fst (fst rawproblem))

    let codec = pca (ReconstructionQuality 0.8) st
    putStrLn "---- ReconstructionQuality 0.8 ----"
    comparedist rawproblem codec

    let codec = pca (NewDimension 20) st
    putStrLn "---- NewDimension 20 ----"
    comparedist rawproblem codec

    let codec = pca (ReconstructionQuality 0.5) st
    putStrLn "---- ReconstructionQuality 0.5 ----"
    comparedist rawproblem codec
