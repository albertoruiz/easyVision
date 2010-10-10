-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Regression
Copyright   :  (c) Alberto Ruiz 20010
License     :  GPL-

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Experimental methods for multivariate linear regression.

-}
-----------------------------------------------------------------------------


module Classifier.Regression (
    normalizer, withNormalized, latent,
    mlr, mlrReduced, pls, pcr,
    msError
) where


import Numeric.LinearAlgebra
import Util.Stat
import Numeric.LinearAlgebra.LAPACK
import Util.Misc(Mat,Vec,randomPermutation,Seed,mean)

vec x = Numeric.LinearAlgebra.fromList x :: Vec

---------------------------------------------

-- to work with variables with zero mean and unit deviation

normalizer :: [Vec] -> (Mat, Vec -> Vec, Vec -> Vec)
normalizer vs = (x,f,g) where
    s = stat (fromRows vs)
    m = meanVector s
    d = sqrt (varianceVector s)
    f x = (x-m)/d
    g x = m + x*d
    x = normalizedData s

withNormalized method prob = gy . method nx ny . fx
    where
    (xs,ys) = unzip prob
    (nx,fx,_) = normalizer (map vec xs)
    (ny,_,gy) = normalizer (map vec ys)

--------------------------------------------------

-- | naive multivariate linear regression
mlr xc yc = (<>m)
    where m = linearSolveSVD xc yc

-- | naive multivariate linear regression
latent p = g $ singularValues m
    where (xs,ys) = unzip p
          (xc,_,_) = normalizer (map vec xs)
          (yc,_,_) = normalizer (map vec ys)
          m = linearSolveSVD xc yc
          g x = x / scalar (sumElements x)

---------------------------------------------

-- | multivariate linear regression through a reduced subspace
mlrReduced n xc yc = (<>q) . (<>w)
    where m = linearSolveSVD xc yc
          (u,_,_) = svd m
          w = takeColumns n u
          t = xc <> w
          q = linearSolveSVD t yc

----------------------------------------------

pls n xc yc = (<>q) . (<>w)
    where
    (_,_,v) = svd (trans yc <> xc)
    w = takeColumns n v
    t = xc <> w
    q = linearSolveSVD t yc


----------------------------------------------

pcr n xc yc = (<>q) . (<>w) where
    w = takeColumns n v
    (_,v) = eigSH (trans xc <> xc)
    t = xc <> w
    q = linearSolveSVD t yc

----------------------------------------------

mse f prob = mean [ norm (vec y - f (vec x) ) ^ 2 | (x,y) <- prob ]

msError f prob = sqrt (k * mse f prob)
    where k = recip $ fromIntegral $ length (snd (head prob))

norm :: Vector Double -> Double
norm = pnorm PNorm2
