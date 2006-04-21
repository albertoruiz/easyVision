module Stat where

import GSL

     
-- 1st and 2nd order statistics of a dataset 
-- the observations are the rows of a matrix
-- and a set of derived useful information  
data Stat = Stat { meanVector :: Vector
                 , covarianceMatrix :: Matrix
                 , eigenvalues :: Vector
                 , eigenvectors :: Matrix
                 , invCov :: Matrix
                 , normalizedData :: Matrix
                 , whiteningTransformation :: Matrix
                 }     
     
stat :: Matrix -> Stat   
stat x = s where   
    m = sumColumns x / fromIntegral (rows x)
    xc = x |-| m
    c = (trans xc <> xc) / fromIntegral (rows x -1)
    (l,v) = eig c 
    lastrow = realMatrix [replicate (cols x) 0 ++[1]]
    w = diag (1/sqrt l) <> v
    s = Stat { meanVector = m
             , covarianceMatrix = c
             , eigenvalues = l
             , eigenvectors = trans v
             , invCov = c <\> ident (cols x)
             , whiteningTransformation = fromBlocks [[w, reshape 1 $ - w <> m], 
                                                     [lastrow]]
             , normalizedData = xc <> trans w                                     
} 
    
sumColumns m = constant 1 (rows m) <> m   
   
mat |-| vec = mat - constant 1 (rows mat) `outer` vec
mat |+| vec = mat + constant 1 (rows mat) `outer` vec
mat |/| vec = mat / constant 1 (rows mat) `outer` vec
mat |*| vec = mat * constant 1 (rows mat) `outer` vec
