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
--mat |/| vec = mat / constant 1 (rows mat) `outer` vec
--mat |*| vec = mat * constant 1 (rows mat) `outer` vec


----------------------------------------------------------------------

data Codec = 
    Codec { encodeVector :: Vector -> Vector
          , decodeVector :: Vector -> Vector
          , encodeMatrix :: Matrix -> Matrix
          , decodeMatrix :: Matrix -> Matrix
          , encodeList   :: [Double] -> [Double]
          , decodeList   :: [Double] -> [Double]
}   
   
data PCARequest = ReconstructionQuality Double | NewDimension Int    
   
-- creates the compression and decompression functions 
-- from the desired reconstruction quality or new dimension 
-- and the statistics of a data set
pca :: PCARequest -> Stat -> Codec

pca (NewDimension n) st = 
    Codec { encodeVector = encv
          , decodeVector = decv
          , encodeMatrix = \x -> (x |-| m) <> trans vp
          , decodeMatrix = \y -> y <> vp |+| m
          , encodeList = toList . encv . fromList
          , decodeList = toList . decv . fromList
} where    
    encv x = vp <> (x - m)
    decv x = x <> vp + m
    vp = takeRows n (eigenvectors st)
    m = meanVector st    

pca (ReconstructionQuality prec) st = pca (NewDimension n) st where    
    s = toList (eigenvalues st)
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0     
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the reconstruction quality must be 0<prec<1"
                else prec   

