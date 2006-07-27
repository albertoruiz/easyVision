-----------------------------------------------------------------------------
{- |
Module      :  Vision.Classifier
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Basic Statistical Pattern Recognition algorithms.

-}
-----------------------------------------------------------------------------

module Vision.Classifier where

import GSL
import Vision.Stat

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

-- given a certain distance function to a population (e.g., mahalanobisDist.stat) and a 
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
