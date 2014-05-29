-----------------------------------------------------------------------------
{- |
Module      :  Classifier.GP
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Gaussian Processes (provisional)

-}
-----------------------------------------------------------------------------

module Classifier.GP (
     GKernel, gaussK,
     lik, matData,
     gp, gp1
) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ( pairwiseD2 )
import Classifier.Base (Sample, TwoGroups, Dicotomizer, group )
import Util.Misc ( Mat, Vec )

--dbg = debug "K" (\m->(rows m, cols m, rank m))

type GKernel = Mat -> Mat -> Mat

{-
-- | polynomial 'Kernel' of order n
polyK :: Int -> Kernel
polyK n x y = (x `dot` y + 1)^n
-}

-- | gaussian 'Kernel' of with width sigma
gaussK :: Double -> GKernel
gaussK s x y = exp (pairwiseD2 x y / scalar (-s**2))


gp :: Double -> GKernel -> TwoGroups -> (Mat -> Vec)
gp noise kernel (g1,g2) = fun
  where
    objs = fromRows $ g1 ++ g2
    vlabels = vjoin [constant 1 (length g1), constant (-1) (length g2)]
    k = kernel objs objs + diag (constant (noise**2) (dim vlabels))
    a = {-dbg-} k <\> vlabels
    -----
    fun z = kernel z objs <> a

gp1 :: Double -> GKernel -> Dicotomizer
gp1 n k p = (@>0) . f . asRow
  where f = gp n k p


lik :: (Matrix Double, Vector Double) -> Double -> Double -> Double
lik (x,y) s n = - 1/2 * ladm -1/2 * (y <> im <.> y)
  where (im,(ladm,_)) = invlndet (gaussK s x x +  diag (constant (n**2) (dim y)))

matData :: Sample (Vector Double) -> (Matrix Double, Vector Double)
matData prob = (x, y)
  where
    [g1,g2] = fst (group prob)
    x = fromRows $ g1 ++ g2
    y = vjoin [constant 1 (length g1), constant (-1) (length g2)]

