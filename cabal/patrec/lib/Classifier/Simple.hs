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
     Distance, distance,
     ordinary, mahalanobis, gaussian,
     naiveGaussian, naive01, ferns,
     nearestNeighbour, subspace,
     robustLoc,
     mse,
) where

import Numeric.LinearAlgebra
import Classifier.Base

import Data.List(sortBy, sort, nub, elemIndex, intersperse, transpose, partition, delete)
import qualified Data.Map as Map
import System.Random
import Data.Array hiding((//))
import Util.Stat
import Util.Misc(norm,(&),(//),(#))
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import qualified Data.List as L
import Control.Arrow((&&&))

-- | mse linear discriminant using the pseudoinverse
mse :: Dicotomizer
mse (g1,g2) = f where
    m = (fromRows g1 // fromRows g2) & konst 1 (dim b,1)
    b = constant 1 (length g1) # constant (-1) (length g2)
    w = m <\> b
    f v = tanh (v # 1 <.> w)

--------------------------------------------------------------------------------

-- | A measure of the disimilarity or distance from an attribute vector to a sample of vectors of a certain class
type Distance a =  [a] -> a -> Double


-- | Mahalanobis's distance to a population.
mahalanobis :: Distance (Vector Double)
mahalanobis vs = f where
    Stat {meanVector = m, invCov = ic} = stat (fromRows vs)
    f x = (x-m) <> ic <.> (x-m)

-- | gaussian -log likelihood (mahalanobis + 1\/2 log sqrt det cov)
gaussian :: Distance  (Vector Double)
gaussian vs = f where
    Stat {meanVector = m, invCov = ic} = stat (fromRows vs)
    k = -log (sqrt (abs( det ic)))
    f x = k + 0.5*((x-m) <> ic <.> (x-m))

-- | Distance to the mean value of the population.
ordinary :: Distance  (Vector Double)
ordinary vs = f where
    Stat {meanVector = m} = stat (fromRows vs)
    f x = norm (x-m)

-- | distance to the nearest neighbour
nearestNeighbour :: Distance (Vector Double)
nearestNeighbour vs v = minimum (map (dist v) vs)
    where dist x y = norm (x-y)

-- | distance to the pca subspace of each class
subspace :: PCARequest -> Distance (Vector Double)
subspace rq vs = f where
    Codec {encodeVector = e, decodeVector = d} = pca rq (stat (fromRows vs))
    f v = norm (v - (d.e) v)

-- | distance to the robust location (with proportion prop)
robustLoc :: Double -> Distance (Vector Double)
robustLoc prop l = f where
    dist x y = norm (x-y)
    f = dist m
    m = fst (ds!!k)
    ds = robustLocation dist l
    k = round (prop*fromIntegral(length l)) -- wrong

-- | A generic distance-based learning machine.
distance :: Distance a -> Learner a
distance d exs = (c,f) where
    (gs,lbs) = group exs
    distfuns = map d gs
    f x = map (negate.($x)) distfuns
    c = createClassifier lbs f


-- | Naive Bayes with gaussian pdf
naiveGaussian :: Distance (Vector Double)
naiveGaussian vs = f where
    x = fromRows vs
    m = meanVector (stat x)
    m2 = meanVector (stat (x*x))
    s = sqrt (m2 - m*m)
    k = sumElements (log s)
    norm x = pnorm PNorm2 x
    f v = k + 0.5*norm ((v-m)/s) ^2

--     maxv = maximum (toList s)
--     tol = eps * maxv
--     s' = cmap pr s
--     d = cmap pr' s
--     pr x = if x < tol then maxv else x
--     pr' x = if x < tol then 0 else 1::Double
--     k = sum (map log (toList s'))
--     f v = k + 0.5*(norm (d*(v-m)/s'))^2
--     norm x = pnorm PNorm2 x

-- | a bayesian classifier based on the estimated probabilities
-- of assumed independent binary (0/1) features in a Vector Double
naive01 :: Distance (Vector Double)
naive01 vs = f where
    Stat {meanVector = p} = stat (fromRows vs)
    f x = - (sumElements $ log $ x*p + (1-x)*(1-p))


-- | A bayesian classifier based on estimated probabilities of assumed
-- independent groups of dependendent binary features
ferns :: Distance [[Bool]]
ferns vs = f where
    hs = map histog (transpose vs)
    histog = M.fromList .map (head &&& lr) . L.group . sort
        where lr l = fromIntegral (length l) / t :: Double
    t = fromIntegral (length vs)
    f x = negate $ sum $ map log $ zipWith get hs x
        where get m v = fromMaybe (0.1/t) (M.lookup v m)
