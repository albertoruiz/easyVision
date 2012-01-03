-----------------------------------------------------------------------------
{- |
Module      :  Util.ICA
Copyright   :  (c) Alberto Ruiz 20010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Experimental implementation of Independend Component Analysis

-}
-----------------------------------------------------------------------------

module Util.ICA (
  icaTrans,
  ica1, ica2
) where

import Numeric.LinearAlgebra hiding (eigenvalues)
import Data.List(sortBy)
import Data.Function(on)
import Util.Misc(Vec,Mat,sqr)
import Util.Optimize(optimize)

meanRow :: Mat -> Vec
meanRow m = ones <> m
    where r = rows m
          k = 1 / fromIntegral r
          ones = constant k r

covar :: Mat -> Mat -> Mat
covar a b = flip scale (trans a <> b) (recip $ fromIntegral (rows a-1))


-- | Independent component analysis (preliminary experiments)
icaTrans :: Int  -- ^ maximum number of iterations
         -> (Mat -> Mat -> Mat) -- ^ update method (e.g. ica1 or ica2 0.01
         -> Mat -- ^ whitened data set
         -> (Mat, [Double]) -- ^ transformation matrix to independent components and negentropy evolution)
icaTrans nmax met d = (fromRows . sortBy (compare `on` q) . toRows $ w, errs)
  where
    cost w' = negentropy' kurt2 (d <> trans w')
    update w' = met w' d
    (w,errs) = optimize 0 0.001 nmax update cost (ident (cols d))
    q wi = - dif kurt2 (d <> wi)
    negentropy' fun  = sum . map (dif fun) . toColumns
    dif (f,k) x = sqr(sumElements (f x) / fromIntegral (dim x) - k)
    kurt2 = ((** 4),3)


-- | (taken from Vivian McPhail's fastICA in hstatistics)
ica1 :: Mat -> Mat -> Mat
ica1 w d = ortho (w + dw) where
    g' u  = 8*u**3*(u**4-3)
    g'' u = 8*u**2*(7*u**4-9)
    y = d <> trans w
    gy = g' y
    mg'y = meanRow (g'' y)
    b = -meanRow (y * gy)
    a = -1/(b-mg'y)
    cov = covar gy y
    dw = diag a <> (diag b + cov) <> w
    ortho m = let (u,_,v) = svd m in u <> trans v

-- | simple gradient optimization
ica2 :: Double -> Mat -> Mat -> Mat
ica2 alpha w d = ortho (w + scalar alpha*dw)
  where
    g x = x**4
    g' x = 4*x**3
    k = 3
    y = d <> trans w
    dw = diag (2*(meanRow (g y) - k)) <> covar (g' y) d
    ortho m = let (u,_,v) = svd m in u <> trans v
