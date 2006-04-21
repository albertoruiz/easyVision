{- Classifying handwritten digits using Mahalanobis distance.
   To download the data file:
        $ wget http://dis.um.es/~alberto/material/sp/mnist.txt.gz
        $ gunzip mnist.txt.gz
-}

import GSL
import Stat

import Data.List
import Data.Array


-- creates data structures useful for building a classifier
data ClassificationProblem 
    = CP { designSamples :: Matrix
         , designLabels :: [Int]
         , testSamples :: [Vector]
         , testLabels :: [Int]
         , labels :: [Int]
         , protos :: [Matrix]
         }
            
-- creates a ClassificationProblem from matrices containing design and test vectors,
-- and the last column containing the class labels            
prepareProblem mat1 mat2 = p where
    p = CP { designSamples = takeColumns (cols mat1 -1) mat1
           , designLabels = map round $ toList $ flatten $ dropColumns (cols mat1 -1) mat1
           , testSamples = toRows $ takeColumns (cols mat2 -1) mat2
           , testLabels = map round $ toList $ flatten $ dropColumns (cols mat2 -1) mat2
           , labels = sort (nub (designLabels p))
           , protos = map proto (labels p)
           }
    proto k = extractRows (indicesOf (==k) (designLabels p)) (designSamples p)
    indicesOf pre list = [i | (i, l) <- zip [0 .. ] list, pre l ]
       
       
-- applies some preprocessing function to the vector in a ClassificationProblem
preprocessProblem (Codec {encodeVector = f1 , encodeMatrix = f}) prob 
    = prob { designSamples = f (designSamples prob)
           , testSamples = map f1 (testSamples prob)
           , protos = map f (protos prob)
           }       


-- Mahalanobis' distance
mahalanobisDist (Stat {meanVector = m, invCov = ic}) x 
    = (x-m) <> ic <> (x-m)

-- distance to the mean value
ordinaryDist (Stat {meanVector = m}) x 
    = norm (x-m)

-- apply several functions to one object
mapf fs x = map ($x) fs

-- obtains the index of the function in the list lfuns with minimum value on x
classify lfuns x = c where
	dists = mapf lfuns x
	dm = minimum dists
	Just c = elemIndex dm dists

-- given a certain distance function to a population (e.g., dist.stat) and a 
-- ClassificationProblem this function creates a classifier fun and the estimation
-- of the misclassification probability and confusion matrix of the method
prepareClasif f prob = (fun, errorRate, confusionMatrix) where
    distfuns = map f (protos prob)	
    fun = classify distfuns
    estimatedLabels = map fun (testSamples prob)
    te = zip estimatedLabels (testLabels prob)
    nc = length (labels prob)
    confusionMatrix = fromArray $ 
        accumArray (+) (0::Double) ((0::Int,0::Int),(nc-1,nc-1)) (zip te [1,1 ..])
    errorRate = 100 *(1 - (sum $ toList $ diag confusionMatrix) 
                     / genericLength (testSamples prob)) 


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
