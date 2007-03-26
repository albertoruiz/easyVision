{- Classifying handwritten digits using Mahalanobis distance.
   To download the data file:
        $ wget http://dis.um.es/~alberto/material/sp/mnist.txt.gz
        $ gunzip mnist.txt.gz
-}

import GSL
import Classifier
import Classifier.ToyProblems(mnistraw)
import Debug.Trace

debug x = trace (show x) x
debug' msg x = trace (msg ++ show x) x

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
    work problem (distance gaussian)
    putStrLn "-- with ordinary distance --"
    work problem (distance ordinary)

main = do
    m <- fromFile "../data/mnist.txt" (5000,785)
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
main' = study $ withPreprocess normalizeAttr (neural 0.001 0.01 50 [30,20])

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


horiz x =  (reshape 28 x) <> constant (1/28::Double) 28
vert x = constant (1/28::Double) 28 <> (reshape 28 x)

both x = join [horiz x,vert x]

-- use withPreprocess (const both) (or const horiz, etc.)
-- the results with projections are clearly worse
{-
study (withPreprocess (const both)$ withPCA (ReconstructionQuality 0.9) $ distance mahalanobis')
Estimated error probability: 13.4 %
Confusion matrix:
98   1  0  0  0  0  1   0  0  0
 1 110  1  1  0  3  0   1  2  0
 0   0 72  5  1  6  4   0  3  0
 0   0  2 79  0 11  0   2  6  1
 0   0  2  0 98  0  0   0  0  5
 0   0  2  9  1 57  2   4  6  3
 1   1  2  0  0  052.5 91   0  1  0
 1   0  1  2  0  1  0 109  0  3
 0   6  2  1  0  4  1   2 72  1
 0   0  3  1  9  1  0   4  0 80

12.6 %

mahalanobis (basic) 20%, ordinary 34%, nearestNeighbour -> 16.8, subspace ( ReconstructionQuality 0.9) -> 32%

with gaussian: horiz -> 20.7, vert -> 51.4
-}

study method = do
    (train, test) <- mnistraw 4000
    let c = fst$ method train
    putStr "Estimated error probability: "
    shErr test c
    putStrLn "Confusion matrix: "
    shConf test c
    shErr train c

-- distance to pca subspace
main'' = study $ distance (subspace (ReconstructionQuality 0.8))


-- show eigenvectors
showeig k = do
    (train, test) <- mnistraw 4000
    let gs = fst $ group train
    let g = gs!!k
    let images = map (reshape 28) g
    imshow $ fromBlocks $ partit 4 $ take 16 images
    let Stat {meanVector = m, eigenvectors = v'} = stat (fromRows g)
    let v = toRows v'
    imshow $ fromBlocks $ map (map (reshape 28)) $ partit 4 $ take 16 v
    imshow $ fromBlocks $ map (map (reshape 28)) $  
        [[ m + 1000*v!!k, m - 1000*v!!k] | k <- [0..5]]
