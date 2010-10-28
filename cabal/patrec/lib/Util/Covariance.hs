-----------------------------------------------------------------------------
{- |
Module      :  Util.Covariance
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Data analysis tools based on the structure of the covariance matrix.

-}
-----------------------------------------------------------------------------

module Util.Covariance (
  CovStr(..) , covStr

, Codec(..), encode, decode, autoencode

, PCARequest(..)
, pca

, whitener, whiteningTrans, whiteningHomog

, normalizer

) where

import Numeric.LinearAlgebra hiding (eigenvalues)
import Util.Misc(Vec,Mat,sqr, unliftRow)

meanRow :: Mat -> Vec
meanRow m = ones <> m
    where r = rows m
          k = 1 / fromIntegral r
          ones = constant k r

covar :: Mat -> Mat -> Mat
covar a b = flip scale (trans a <> b) (recip $ fromIntegral (rows a-1))

----------------------------------------------------------------------

-- | covariance structure of multivariate observations
data CovStr = CovStr
    { meanV        :: Vec
    , varV         :: Vec
    , covarM       :: Mat
    , eigvalCov    :: Vec
    , eigvecCov    :: Mat
    }

-- | compute the covariance structure of matrix rows
covStr :: Mat -> CovStr
covStr x = s where
    n = rows x
    n' = fromIntegral n / fromIntegral (n-1)
    m = meanRow x
    xc = x - asRow m
    vars = scalar n' * meanRow (xc*xc)
    c = covar xc xc
    (l,v') = eigSH' c
    s = CovStr { meanV  = m
               , varV   = vars
               , covarM = c
               , eigvalCov = l
               , eigvecCov = trans v'
               }

----------------------------------------------------------------------

-- | This structure contains functions to encode and decode individual vectors (or collection of vectors packed into a matrix) obtained by some suitable criterion (e.g. 'pca').
data Codec = Codec
    { encodeRows :: Mat -> Mat
    , decodeRows :: Mat -> Mat
    }

encode :: Codec -> Vector Double -> Vector Double
encode c = unliftRow (encodeRows c)

decode :: Codec -> Vector Double -> Vector Double
decode c = unliftRow (decodeRows c)

autoencode :: Mat -> (Mat -> Codec) -> Mat
autoencode m met = encodeRows c m where c = met m

----------------------------------------------------------------------

-- | Several ways to specify the amount of 'pca' compression.
data PCARequest = ReconstructionQuality Double -- ^ ratio of variance
                | SigmaPercent Double          -- ^ ratio of standard deviation
                | NewDimension Int             -- ^ number of desired components

-- | Given the desired compression criterion, from the 'Stat'istics of a dataset it creates a linear 'Codec' based on principal component analysis  (which minimizes mean squared reconstruction error).
pca :: PCARequest -> CovStr -> Codec
pca (NewDimension n) st = Codec
    { encodeRows = \x -> (x - asRow m) <> trans vp
    , decodeRows = \y -> (y <> vp) + asRow m }
  where
    vp = takeRows n (eigvecCov st)
    m = meanV st

pca (ReconstructionQuality prec) st = pca (NewDimension n) st where
    s = toList (eigvalCov st)
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the reconstruction quality must be 0<prec<1"
                else prec

pca (SigmaPercent p) st = pca (ReconstructionQuality $ 1-sqr(1-p/100)) st

--------------------------------------------------------------

whiteningTrans :: CovStr -> Matrix Double
whiteningTrans st = diag (1/sqrt l) <> v
  where
    l = eigvalCov st
    v = eigvecCov st

whiteningHomog :: CovStr -> Matrix Double
whiteningHomog st = fromBlocks [[ w, -wm]
                               ,[ 0,  1 ]]
  where
    m = meanV st
    w = whiteningTrans st
    wm = asColumn (w <> m)

whitener :: CovStr -> Codec
whitener st = Codec { encodeRows = \x -> (x - m) <> trans w
                    , decodeRows = \y -> (y <> w) + m }
  where
    m = asRow $ meanV st
    w = whiteningTrans st

--------------------------------------------------------------

normalizer :: CovStr -> Codec
normalizer st = Codec { encodeRows = \x -> (x - m) / d
                      , decodeRows = \y -> (y * d) + m }
  where
    m = asRow $ meanV st
    d = asRow $ sqrt (varV st)
