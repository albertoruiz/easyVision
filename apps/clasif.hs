{- Classifying handwritten digits using Mahalanobis distance.
   To download the data file:
        $ wget http://dis.um.es/~alberto/material/sp/mnist.txt.gz
        $ gunzip mnist.txt.gz
-}

import GSL
import Data.List
import Data.Array
   
sumColumns m = constant 1 (rows m) <> m   
   
-- vector with the mean value of the columns of a matrix
mean x = sumColumns x / fromIntegral (rows x)

-- covariance matrix of a list of observations as rows of a matrix
cov x = (trans xc <> xc) / fromIntegral (rows x -1) 
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m
     
-- 1st and 2nd order statistics of a dataset (mean, eigenvalues and eigenvectors of cov)  
data Stat = Stat { meanVec :: Vector
                 , covMat :: Matrix
                 , eigenvalues :: [Double]
                 , eigenvectors :: Matrix
                 , invCov :: Matrix
                 }     
     
stat :: Matrix -> Stat   
stat x = s where   
    (l,v) = eig (covMat s)  
    s = Stat { meanVec = mean x
             , covMat = cov x
             , eigenvalues = toList l
             , eigenvectors = trans v
             , invCov = covMat s <\> ident (cols x)
             } 
    
mat |-| vec = mat - constant 1 (rows mat) `outer` vec
mat |+| vec = mat + constant 1 (rows mat) `outer` vec
        
-- creates the compression and decompression functions from the desired reconstruction 
-- quality and the statistics of a data set
pca :: Double -> Stat -> ((Vector -> Vector , Vector -> Vector),
                          (Matrix -> Matrix , Matrix -> Matrix))          
pca prec st = ((encode1, decode1), (encode,decode))    
  where    
    encode1 x = vp <> (x - m)        -- for a single vector
    decode1 x = x <> vp + m
    encode x = (x |-| m) <> trans vp -- for many vectors as rows of a matrix
    decode y = y <> vp |+| m
    vp = takeRows n (eigenvectors st)
    m = meanVec st    
    s = eigenvalues st
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0     
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the precision in pca must be 0<prec<1"
                else prec   

-- creates data structures useful for building a classifier
data ClassifficationProblem 
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
preprocessProblem (f1,f) prob 
    = prob { designSamples = f (designSamples prob)
           , testSamples = map f1 (testSamples prob)
           , protos = map f (protos prob)
           }       


-- Mahalanobis' distance
dist (Stat {meanVec = m, invCov = ic}) x 
    = (x-m) <> ic <> (x-m)

-- distance to the mean value
dist' (Stat {meanVec = m}) x 
    = norm (x-m)

-- apply several functions to one object
mapf :: [t -> a] -> t -> [a]
mapf [] _ = []
mapf (f:fs) a = f a: mapf fs a

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

                                                  
main = do
    m <- gslReadMatrix "mnist.txt" (5000,785)
    let rawproblem = prepareProblem (takeRows 4000 m) (dropRows 4000 m)
    let st = stat (designSamples rawproblem)
    
    let ((enc1,_),(enc,_)) = pca 0.8 st
    let problem = preprocessProblem (enc1, enc) rawproblem
    print (labels problem)
    print $ cols $ designSamples problem 
    let (f,e,m) = prepareClasif (dist.stat) problem
    print e
    print m
    let (f,e,m) = prepareClasif (dist'.stat) problem
    print e
    print m
    
    let ((enc1,_),(enc,_)) = pca 0.5 st
    let problem = preprocessProblem (enc1, enc) rawproblem
    print (labels problem)
    print $ cols $ designSamples problem 
    let (f,e,m) = prepareClasif (dist.stat) problem
    print e
    print m
    let (f,e,m) = prepareClasif (dist'.stat) problem
    print e
    print m
