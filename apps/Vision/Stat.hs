-----------------------------------------------------------------------------
{- |
Module      :  Vision.Stat
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Statistical characterization of multivariate samples and Principal Component Analysis.

-}
-----------------------------------------------------------------------------

module Vision.Stat
( Stat(..)
, stat

, Codec(..)

, PCARequest(..)
, pca
) where

import GSL

-- | 1st and 2nd order statistics and other useful information extracted from a multivariate sample, where observations are given as rows of a matrix.

data Stat = Stat { meanVector :: Vector
                 , covarianceMatrix :: Matrix
                 , eigenvalues :: Vector
                 , eigenvectors :: Matrix
                 , invCov :: Matrix
                 , normalizedData :: Matrix
                 , whiteningTransformation :: Matrix
                 }

-- | Creates a 'Stat' structure from a matrix. Of course, due to lazyness, only the fields required by the particular application will be actually computed.
stat :: Matrix -> Stat
stat x = s where
    m = sumColumns x / fromIntegral (rows x)
    xc = x |-| m
    c = (trans xc <> xc) / fromIntegral (rows x -1)
    (l,v) = eig c 
    lastrow = realVector (replicate (cols x) 0 ++[1])
    w = diag (1/sqrt l) <> v
    s = Stat { meanVector = m
             , covarianceMatrix = c
             , eigenvalues = l
             , eigenvectors = trans v
             , invCov = inv c
             , whiteningTransformation = w <|> -w<>m <-> lastrow  
             , normalizedData = xc <> trans w
}

sumColumns m = constant 1 (rows m) <> m   

mat |-| vec = mat - constant 1 (rows mat) `outer` vec
mat |+| vec = mat + constant 1 (rows mat) `outer` vec
--mat |/| vec = mat / constant 1 (rows mat) `outer` vec
--mat |*| vec = mat * constant 1 (rows mat) `outer` vec

----------------------------------------------------------------------

-- | This structure contains functions to encode and decode individual vectors (or collection of vectors packed into a matrix) obtained by some suitable criterion (e.g. 'pca').

data Codec = 
    Codec { encodeVector :: Vector -> Vector
          , decodeVector :: Vector -> Vector
          , encodeMatrix :: Matrix -> Matrix
          , decodeMatrix :: Matrix -> Matrix
          , encodeList   :: [Double] -> [Double]
          , decodeList   :: [Double] -> [Double]
}

-- | Two ways to specify the amount of 'pca' compression.
data PCARequest = ReconstructionQuality Double -- ^ ratio of spectral power
                | NewDimension Int             -- ^ number of desired components

-- | Given the desired compression criterion, from the 'Stat'istics of a dataset it creates a linear 'Codec' based on principal component analysis  (which maximizes mean squared reconstruction error).
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

