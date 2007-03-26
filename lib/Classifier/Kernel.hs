-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Kernel
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Simple kernel machine based on MSE estimation in feature space.

-}
-----------------------------------------------------------------------------

module Classifier.Kernel (
     Kernel, polyK, gaussK, kernelMSE, kernelMSE'

) where

import GSL
import Classifier.Base

matrix = fromLists :: [[Double]] -> Matrix Double


-- our nice kernel MSE algorithm

-- | Generalized inner product, corresponding to the ordinary dot product in an implicit feature space.
type Kernel = (Vector Double -> Vector Double -> Double)
-- (it should be more general)


-- FIXME: remove partit
delta f l1 l2 = matrix $ partit (length l1) $ [f x y | x <- l1, y <- l2]

-- FIXME: leave only one version
-- | minimum squared error linear machine in the implicit feature space induced by the given 'Kernel'
kernelMSE :: Kernel -> Dicotomizer
kernelMSE kernel (g1,g2) = fun where
    fun z = expan z `dot` a
    expan z = vector $ map (kernel z) objs
    a = pinvTolg 1 (delta kernel objs objs) <> labels
    objs = g1 ++ g2
    labels = join [constant 1 (length g1), constant (-1) (length g2)]

-- | the same as 'kernelMSE'' with control of the numeric tolerance of the pseudoinverse.
kernelMSE' :: Double -> Kernel -> Dicotomizer
kernelMSE' tol kernel (g1,g2) = fun where
    fun z = expan z `dot` a
    expan z = vector $ map (kernel z) objs
    a = pinvTolg tol (delta kernel objs objs) <> labels
    objs = g1 ++ g2
    labels = join [constant 1 (length g1), constant (-1) (length g2)]

-- | polynomial 'Kernel' of order n
polyK :: Int -> Kernel
polyK n x y = (x `dot` y + 1)^n

-- | gaussian 'Kernel' of with width sigma
gaussK :: Double -> Kernel
gaussK s x y = exp (-(norm (x-y) / s)^2)
