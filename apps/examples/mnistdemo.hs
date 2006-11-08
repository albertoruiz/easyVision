{- Classifying handwritten digits using Mahalanobis distance.
   To download the data file:
        $ wget http://dis.um.es/~alberto/material/sp/mnist.txt.gz
        $ gunzip mnist.txt.gz
-}

import GSL
import Vision

shErr d c = putStrLn $ (show $ 100 * errorRate d c) ++ " %"
shConf d c = putStrLn $ format " " (show.round) (confusion d c)

work (train,test) method = do
    let  c = fst $ method train
    putStr "Estimated error probability: "
    shErr test c
    putStrLn "Confusion matrix: "
    shConf test c

comparedist (train,test) codec = do
    let t = encodeVector codec
    let problem = (preprocess t train, preprocess t test)
    putStr "Reduced dimension: "
    print $ size $ fst $ head $ fst problem
    putStrLn "-- with Mahalanobis distance --"
    work problem (distance mahalanobis)
    putStrLn "-- with Mahalanobis distance + log det sigma --"
    work problem (distance mahalanobis')
    putStrLn "-- with ordinary distance --"
    work problem (distance ordinary)

main = do
    m <- fromFile "mnist.txt" (5000,785)
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

withPCA rq = withPreprocess (mef rq)

-- a multilayer perceptron
main' = do
    m <- fromFile "mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let (train, test) = splitAt 4000 mnist
    --let g = normalizeAttr train
    --let (r,e) = neural' 0.001 0.01 10 [20] (take 1000 (preprocess g train))
    --mplot [fromList e]
    --let c = fst$ withPCA (NewDimension 10) (withPreprocess normalizeAttr (neural 0.01 0.1 10 [20])) (train)
    let c = fst$ (withPreprocess normalizeAttr (neural 0.001 0.01 50 [30,20])) (train)
    putStr "Estimated error probability: "
    shErr test c
    putStrLn "Confusion matrix: "
    shConf test c
    shErr train c

{-
9.700000000000001 %
Confusion matrix:
97   0  0  0   0  0  1   0  0  2
 0 114  3  0   0  1  1   0  0  0
 1   2 79  1   0  0  4   0  4  0
 0   2  1 89   0  3  0   4  1  1
 0   1  0  0 100  0  0   0  0  4
 4   0  0  4   1 70  3   0  1  1
 2   0  3  0   0  3 85   0  1  2
 1   0  0  0   2  0  0 109  0  5
 2   0  5  3   6  1  0   1 69  2
 0   0  0  1   3  0  1   2  0 91

1.15 %
-}

-- distance to pca subspace
main'' = do
    m <- fromFile "mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let (train, test) = splitAt 4000 mnist
    let c = fst$ distance (subspace (ReconstructionQuality 0.8)) (train)
    putStr "Estimated error probability: "
    shErr test c
    putStrLn "Confusion matrix: "
    shConf test c
    shErr train c

partit _ [] = []
partit n l  = take n l : partit n (drop n l)

-- show eigenvectors
showeig k = do
    m <- fromFile "mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let (train, test) = splitAt 4000 mnist
    let gs = fst $ group train
    let g = gs!!k
    let images = map (reshape 28) g
    imshow $ fromBlocks $ partit 4 $ take 16 images
    let Stat {meanVector = m, eigenvectors = v'} = stat (fromRows g)
    let v = toRows v'
    imshow $ fromBlocks $ map (map (reshape 28)) $ partit 4 $ take 16 v
    imshow $ fromBlocks $ map (map (reshape 28)) $  
        [[ m + 1000*v!!k, m - 1000*v!!k] | k <- [0..5]]
