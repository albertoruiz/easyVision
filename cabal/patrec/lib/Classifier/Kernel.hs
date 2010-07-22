-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Kernel
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Simple kernel machine based on MSE estimation in feature space (Ruiz & Lopez-de-Teruel, 2001).

-}
-----------------------------------------------------------------------------

module Classifier.Kernel (
-- * Typical kernels
     Kernel, polyK, gaussK,
-- * Algorithms
     kernelMSE
) where

import Numeric.LinearAlgebra
import Classifier.Base
import Util.Misc(norm)

-- | Generalized inner product, corresponding to the ordinary dot product in an implicit feature space.
type Kernel = (Vector Double -> Vector Double -> Double)
-- (it should be more general)

--delta f l1 l2 = matrix $ [[f x y | y <- l2] | x<- l1]
delta :: (a -> t -> Double) -> [a] -> [t] -> Matrix Double
delta f l1 l2 = reshape (length l1) $ vector [f x y | x <- l1, y <- l2]

-- | Minimum squared error linear machine in the feature space induced by the given kernel.
kernelMSE :: Double  -- ^ Numeric tolerance for the pseudoinverse (1 = machine precision).
                      --  Larger values have a regularization effect.
          -> Kernel
          -> Dicotomizer
kernelMSE tol kernel (g1,g2) = fun where
    fun z = expan z `dot` a
    expan z = vector $ map (kernel z) objs
    a = pinvTol tol (delta kernel objs objs) <> labels'
    objs = g1 ++ g2
    labels' = join [constant 1 (length g1), constant (-1) (length g2)]

-- | polynomial 'Kernel' of order n
polyK :: Int -> Kernel
polyK n x y = (x `dot` y + 1)^n

-- | gaussian 'Kernel' of with width sigma
gaussK :: Double -> Kernel
gaussK s x y = exp (-(norm (x-y) / s)^(2::Int))
