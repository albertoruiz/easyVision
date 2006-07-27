{- Classifying handwritten digits using Mahalanobis distance.
   To download the data file:
        $ wget http://dis.um.es/~alberto/material/sp/mnist.txt.gz
        $ gunzip mnist.txt.gz
-}

import GSL
import Vision


test problem distfun = do
    let (f,e,m) = prepareClasif distfun problem
    putStr "Estimated error probability: "
    print e
    putStrLn "Confusion matrix: "
    print m                                                      
                                                                                         
comparedist rawproblem codec = do 
    let problem = preprocessProblem codec rawproblem
    putStr "Reduced dimension: "
    print $ cols $ designSamples problem
    putStrLn "-- with Mahalanobis distance --"
    test problem (mahalanobisDist.stat)
    putStrLn "-- with ordinary distance --"
    test problem (ordinaryDist.stat)
    
                                                                                         
                                                                                         
main = do
    m <- gslReadMatrix "mnist.txt" (5000,785)
    let rawproblem = prepareProblem (takeRows 4000 m) (dropRows 4000 m)
    putStr "Labels found: "
    print (labels rawproblem)
    putStr "Original dimension: "
    print $ cols $ designSamples rawproblem 
    
    let st = stat (designSamples rawproblem)
    
    let codec = pca (ReconstructionQuality 0.8) st
    putStrLn "---- ReconstructionQuality 0.8 ----"
    comparedist rawproblem codec
    
    let codec = pca (NewDimension 20) st
    putStrLn "---- NewDimension 20 ----"
    comparedist rawproblem codec
    
    let codec = pca (ReconstructionQuality 0.5) st
    putStrLn "---- ReconstructionQuality 0.5 ----"
    comparedist rawproblem codec
