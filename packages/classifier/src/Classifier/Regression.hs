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
    Regressor, DataPairs, RegressionMethod,
    withNormalized,
    mlr, mlrReduced, pls, pcr,
    latent,
    msError
) where

import Numeric.LinearAlgebra.HMatrix
import Util.Stat
import Util.Misc(Mat,Vec)
import Util.Statistics(mean)

-- working with variables with zero mean and unit deviation

type Regressor = Vec -> Vec
type DataPairs = [(Vec, Vec)] -- ?
type RegressionMethod = Mat -> Mat -> Regressor

withNormalized :: RegressionMethod
               -> DataPairs
               -> Regressor
withNormalized method prob = gy . method nx ny . fx where
    (xs,ys) = unzip prob
    (nx,fx,_) = normalizer xs
    (ny,_,gy) = normalizer ys

normalizer :: [Vec] -> (Mat, Vec -> Vec, Vec -> Vec)
normalizer dat = (nz,f,g) where
        st = stat (fromRows dat)
        nz = normalizedData st
        f =   normalize st
        g = unnormalize st

--------------------------------------------------

-- | naive multivariate linear regression
mlr :: RegressionMethod
mlr xc yc = (tr m #>)
    where m = linearSolveSVD xc yc

--------------------------------------------------

-- | singular values of the linear regression function, the
-- decrease rate suggests number of latent components
latent :: DataPairs -> Vec
latent p = g $ singularValues m
    where (xs,ys) = unzip p
          (xc,_,_) = normalizer xs
          (yc,_,_) = normalizer ys
          m = linearSolveSVD xc yc
          g x = x / scalar (sumElements x)

---------------------------------------------

-- | multivariate linear regression through a reduced subspace
mlrReduced :: Int -> RegressionMethod
mlrReduced n xc yc = (<#q) . (<#w)
    where m = linearSolveSVD xc yc
          (u,_,_) = svd m
          w = takeColumns n u
          t = xc <> w
          q = linearSolveSVD t yc

----------------------------------------------

-- | a simple form of partial least squares
pls :: Int -> RegressionMethod
pls n xc yc = (<#q) . (<#w)
    where
    (_,_,v) = svd (tr yc <> xc)
    w = takeColumns n v
    t = xc <> w
    q = linearSolveSVD t yc

----------------------------------------------

-- | regression from the principal components of the input vector
pcr :: Int -> RegressionMethod
pcr n xc yc = (<#q) . (<#w) where
    w = takeColumns n v
    (_,v) = eigSH (xTx xc)
    t = xc <> w
    q = linearSolveSVD t yc

----------------------------------------------

-- | in sigma per component
msError :: Regressor -> DataPairs -> Double
msError f prob = sqrt (k * mse)
    where k = recip $ fromIntegral $ size (snd (head prob))
          mse = mean [ norm_2 (y - f x )**2 | (x,y) <- prob ]

