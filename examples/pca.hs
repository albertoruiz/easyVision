-- Principal component analysis

module Main where

import GSL
import GSL.Util

-- division by an integer
m ./. r = m <> (1/fromIntegral r ::Double)

-- vector with the mean value of the columns of a matrix
mean x = sumCols x ./. (rows x)

-- covariance matrix of a list of observations as rows of a matrix
cov x = (trans xc <> xc) ./. (rows x -1) 
    where xc = center x
          center m = m |-| constant 1 (rows m) `outer` mean m
    
-- by now, check only the largest eigenvalues
test :: M -> [Double]
test x = take 10 (toList ls) where
    mc = cov x
    (ls, _)  = eig mc
    
main = do 
    m <- gslReadMatrix "/home/alberto/space/data/mnist.txt" (5000,785)
    let x = subMatrix 0 (rows m-1) 0 (cols m -2) m
    print (test x) 