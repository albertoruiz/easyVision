-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Simple
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Very simple methods.

-}
-----------------------------------------------------------------------------


module Classifier.Simple (
     Distance, distance, ordinary, mahalanobis, gaussian, nearestNeighbour, subspace, robustLoc,
     mse, mseWeighted, distWeighted,
) where

import Numeric.LinearAlgebra
import Classifier.Base

import Data.List(sortBy, sort, nub, elemIndex, intersperse, transpose, partition, delete)
import qualified Data.Map as Map
import System.Random
import Data.Array
import Classifier.Stat
import Debug.Trace

-- | mse linear discriminant using the pseudoinverse
mse :: Dicotomizer
mse (g1,g2) = f where
    m = (fromRows g1 <-> fromRows g2) <|> constant 1 (dim b)
    b = join [constant 1 (length g1), constant (-1) (length g2)]
    w = pinv m <> b
    f v = tanh (join [v,1] <.> w)

--------------------------------------------------------------------------------

-- | A measure of the disimilarity or distance from an attribute vector to a sample of vectors of a certain class
type Distance =  [Attributes] -> Attributes -> Double

-- | Mahalanobis's distance to a population.
mahalanobis :: Distance
mahalanobis vs = f where
    Stat {meanVector = m, invCov = ic} = stat (fromRows vs)
    f x = (x-m) <> ic <.> (x-m)

-- | gaussian -log likelihood (mahalanobis + 1\/2 log sqrt det cov)
gaussian :: Distance
gaussian vs = f where
    Stat {meanVector = m, invCov = ic} = stat (fromRows vs)
    k = -log (sqrt (abs( det ic)))
    f x = k + 0.5*((x-m) <> ic <.> (x-m))

-- | Distance to the mean value of the population.
ordinary :: Distance
ordinary vs = f where
    Stat {meanVector = m} = stat (fromRows vs)
    f x = norm (x-m)

-- | distance to the nearest neighbour
nearestNeighbour :: Distance
nearestNeighbour vs v = minimum (map (dist v) vs)
    where dist x y = norm (x-y)

-- | distance to the pca subspace of each class
subspace :: PCARequest -> Distance
subspace rq vs = f where
    Codec {encodeVector = e, decodeVector = d} = pca rq (stat (fromRows vs))
    f v = norm (v - (d.e) v)

-- | distance to the robust location (with proportion prop)
robustLoc :: Double -> Distance
robustLoc prop l = f where
    dist x y = norm (x-y)
    f = dist m
    m = fst (ds!!k)
    ds = robustLocation dist l
    k = round (prop*fromIntegral(length l)) -- wrong

-- | A generic distance-based learning machine.
distance :: Distance -> Learner
distance d exs = (c,f) where
    (gs,lbs) = group exs
    distfuns = map d gs
    f x = map (negate.($x)) distfuns
    c = createClassifier lbs f

------------------------------------------------------------------------------------

-- more complex weak learners, rather bad

-- | mse with weighted examples
mseWeighted :: WeightedDicotomizer
mseWeighted (g1,g2) d = f where
    m = (fromRows g1 <-> fromRows g2) <|> constant 1 (dim b)
    b = join [constant 1 (length g1), constant (-1) (length g2)]
    rd  = sqrt d
    rd' = outer rd (constant 1 (cols m))
    w = pinv (m*rd') <> (b*rd)
    f v = tanh (join [v,1] <.> w)




-- | a minimum distance dicotomizer using weighted examples
distWeighted :: WeightedDicotomizer
distWeighted (g1,g2) d = f where
    n1 = length g1
    n2 = length g2
    d1 = subVector  0 n1 d
    d2 = subVector n1 n2 d
    ones = constant 1 (dim (head g1))
    a1 = outer d1 ones * fromRows g1
    a2 = outer d2 ones * fromRows g2
    m1 = sumColumns a1 */ (pnorm PNorm1 d1)
    m2 = sumColumns a2 */ (pnorm PNorm1 d2)
    f x = norm (x-m2) - norm (x-m1)
    sumColumns m = constant 1 (rows m) <> m


-- just to check that they are not completely wrong

--mse' = multiclass (unweight mseWeighted)

--dist' = multiclass (unweight distWeighted)

