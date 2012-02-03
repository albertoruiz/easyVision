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

, whitener, whiteningTransf

, isoDist, isoDistTransf

, normalizer

, correlation

) where

import Numeric.LinearAlgebra hiding (eigenvalues)
import Util.Misc(Vec,Mat,sqr, unliftRow, diagl,mean,norm)

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

encode :: Codec -> Vec -> Vec
encode c = unliftRow (encodeRows c)

decode :: Codec -> Vec -> Vec
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

whitening :: CovStr -> Mat
whitening st = diag (1/sqrt l) <> v
  where
    l = eigvalCov st
    v = eigvecCov st

-- | homogeneous transformation (take subMatrix (0,0) (3,3) for the inhomogeneous part on centered data)
whiteningTransf :: CovStr -> Mat
whiteningTransf st = fromBlocks [[ w, -wm]
                               ,[ 0,  1 ]]
  where
    m = meanV st
    w = whitening st
    wm = asColumn (w <> m)

whitener :: CovStr -> Codec
whitener st = Codec { encodeRows = \x -> (x - m) <> trans w
                    , decodeRows = \y -> (y <> w) + m }
  where
    m = asRow $ meanV st
    w = whitening st


----------------------------------------------------------------------

isoDistTransf :: Mat -> CovStr -> Mat
isoDistTransf z st = fromBlocks [[ w, -wm]
                                ,[ 0,  1 ]]
  where
    m = meanV st
    w = diagl [d,d]
    wm = asColumn (w <> m)
    
    d = recip . (/sqrt 2) . mean . map norm . toRows $ z- asRow m



isoDist :: Mat -> CovStr -> Codec
isoDist z st = Codec { encodeRows = \x -> (x - m) / scalar d
                     , decodeRows = \y -> (y * scalar d) + m }
  where
    m = asRow $ meanV st
    d = (/sqrt 2) . mean . map norm . toRows $ z-m

--------------------------------------------------------------

normalizer :: CovStr -> Codec
normalizer st = Codec { encodeRows = \x -> (x - m) / d
                      , decodeRows = \y -> (y * d) + m }
  where
    m = asRow $ meanV st
    d = asRow $ sqrt (varV st)

---------------------------------------------------------------

-- correlation coefficients from a covariance matrix
correlation :: Mat -> Mat
correlation c = c / outer s s where
    s = sqrt (takeDiag c)

