-----------------------------------------------------------------------------
{- |
Module      :  Util.Stat
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Deprecated data analysis tools.

-}
-----------------------------------------------------------------------------

module Util.Stat (
  Stat(..) , stat
, Codec(..)

, PCARequest(..)
, pca

) where

import Numeric.LinearAlgebra.HMatrix hiding (eigenvalues)
import Util.Misc(Vec,Mat)

meanRow :: Mat -> Vec
meanRow m = tr m #> ones
    where r = rows m
          k = 1 / fromIntegral r
          ones = konst k r

-- | 1st and 2nd order statistics and other useful information extracted from a multivariate sample, where observations are given as rows of a matrix.

data Stat = Stat
    { meanVector              :: Vec
    , covarianceMatrix        :: Mat
    , eigenvalues             :: Vec
    , eigenvectors            :: Mat
    , invCov                  :: Mat
    , whitenedData            :: Mat
    , whiteningTransformation :: Mat -- ^ homogeneous transformation which includes centering
    , whitener                :: Mat
    , varianceVector          :: Vec
    , normalizedData          :: Mat  -- ^ 0 mean and 1 std each var
    , normalize               :: Vec -> Vec
    , unnormalize             :: Vec -> Vec
    }

-- | Creates a 'Stat' structure from a matrix. Of course, due to laziness, only the fields required by the particular application will be actually computed.
stat :: Mat -> Stat
stat x = s where
    m = meanRow x
    xc = x - asRow m
    c = (tr xc <> xc) / fromIntegral (rows x -1)
    (l,v') = eigSH' c
    v = tr v'
    lastrow = asRow $ fromList $ replicate (cols x) 0 ++[1.0::Double]
    w = diag (1/sqrt l) <> v
    n = rows x
    n' = fromIntegral n / fromIntegral (n-1)
    vars = scalar n' * meanRow (xc*xc)
    d = sqrt vars
    f z = (z-m)/d
    g z = m + z*d
    s = Stat { meanVector = m
             , covarianceMatrix = c
             , eigenvalues = l
             , eigenvectors = v
             , invCov = inv c
             , whitener = w
             , whiteningTransformation = w ||| asColumn (-w #> m)
                                       === lastrow
             , whitenedData = xc <> tr w
             , varianceVector = vars
             , normalizedData = (x - asRow m) / asRow (sqrt vars)
             , normalize = f
             , unnormalize = g
             }



----------------------------------------------------------------------


-- | This structure contains functions to encode and decode individual vectors (or collection of vectors packed into a matrix) obtained by some suitable criterion (e.g. 'pca').
data Codec = Codec
    { encodeVector :: Vec -> Vec
    , decodeVector :: Vec -> Vec
    , encodeMatrix :: Mat -> Mat
    , decodeMatrix :: Mat -> Mat
    }

-- | Several ways to specify the amount of 'pca' compression.
data PCARequest = ReconstructionQuality Double -- ^ ratio of variance
                | SigmaPercent Double          -- ^ ratio of standard deviation
                | NewDimension Int             -- ^ number of desired components

-- | Given the desired compression criterion, from the 'Stat'istics of a dataset it creates a linear 'Codec' based on principal component analysis  (which minimizes mean squared reconstruction error).
pca :: PCARequest -> Stat -> Codec

pca (NewDimension n) st =
    Codec { encodeVector = encv
          , decodeVector = decv
          , encodeMatrix = \x -> (x - asRow m) <> tr vp
          , decodeMatrix = \y -> (y <> vp) + asRow m

} where
    encv x = vp #> (x - m)
    decv x = (tr vp #> x) + m
    vp = takeRows n (eigenvectors st)
    m = meanVector st

pca (ReconstructionQuality prec) st = pca (NewDimension n) st where
    s = toList (eigenvalues st)
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the reconstruction quality must be 0<prec<1"
                else prec

pca (SigmaPercent p) st = pca (ReconstructionQuality $ 1-(1-p/100)**2) st

