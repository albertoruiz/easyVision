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
     euclidean, mahalanobis,
     Likelihood, bayes,
     gaussian,
     naiveGaussian, naive01, ferns,
     nearestNeighbour, subspace,
     robustLoc,
     mse,
     multiclass, dicodist
) where

import Numeric.LinearAlgebra
import Classifier.Base
import Util.Probability(weighted)

import Data.List(sort, transpose)
import qualified Data.Map as M
import Util.Stat
import Util.Misc(norm,(&),(//),(#),sqr,Vec)
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

-- | creates a function to compute the - log likelihood of x from a sample
type Likelihood a = [a] -> a -> Double

-- | A generic distance-based learning machine.
-- (The output probabilities are not real,
-- they are a simple heuristic approximation of relative distances)
distance :: Distance a -> Learner a
distance ds exs = c where
    (gs,lbs) = group exs
    distfuns = map ds gs
    liks x = heur $ map ($x) distfuns
    c x = weighted $ zip (labels lbs) (liks x)
    heur xs = map f xs
        where t = minimum xs
              f x | x == t    = 1
                  | otherwise = t/x

-- | Bayesian classifier using a -log p(x|c) function (equal priors)
bayes :: Likelihood a -> Learner a
bayes ds exs = c where
    (gs,lbs) = group exs
    distfuns = map ds gs
    liks x = map (w.($x)) distfuns
    w d = exp (-d)
    c x = weighted $ zip (labels lbs) (liks x)

-- normSafe :: (Ord a, Fractional a) => a -> [a] -> [a]
-- normSafe h ws = if t > h then map (/t) ws else map (const 1) ws
--     where t = maximum ws

-- | Mahalanobis's distance to a population.
mahalanobis :: Distance Vec
mahalanobis vs = f where
    Stat {meanVector = m, invCov = ic} = stat (fromRows vs)
    f x = (x-m) <> ic <.> (x-m)

-- | gaussian -log likelihood (mahalanobis + 1\/2 log sqrt det cov)
gaussian :: Likelihood Vec
gaussian vs = f where
    Stat {meanVector = m, invCov = ic} = stat (fromRows vs)
    k = -log (sqrt (abs( det ic)))
    f x = k + 0.5*((x-m) <> ic <.> (x-m))

-- | Distance to the mean value of the population.
euclidean :: Distance Vec
euclidean vs = f where
    Stat {meanVector = m} = stat (fromRows vs)
    f x = norm (x-m)

-- | distance to the nearest neighbour
nearestNeighbour :: Distance Vec
nearestNeighbour vs v = minimum (map (dist v) vs)
    where dist x y = norm (x-y)

-- | distance to the pca subspace of each class
subspace :: PCARequest -> Distance Vec
subspace req vs = f where
    Codec {encodeVector = e, decodeVector = d} = pca req (stat (fromRows vs))
    f v = norm (v - (d.e) v)

-- | distance to the robust location (with proportion prop)
robustLoc :: Double -> Distance Vec
robustLoc prop l = f where
    dist x y = norm (x-y)
    f = dist m
    m = fst (ds!!k)
    ds = robustLocation dist l
    k = round (prop*fromIntegral(length l)) -- wrong


-- | Naive Bayes with gaussian pdf
naiveGaussian :: Likelihood Vec
naiveGaussian vs = f where
    x = fromRows vs
    m = meanVector (stat x)
    m2 = meanVector (stat (x*x))
    s = sqrt (m2 - m*m)
    k = sumElements (log s)
    f v = k + 0.5*sqr(norm ((v-m)/s))

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
naive01 :: Likelihood Vec
naive01 vs = f where
    Stat {meanVector = p} = stat (fromRows vs)
    f x = - (sumElements $ log $ x*p + (1-x)*(1-p))


-- | A bayesian classifier based on estimated probabilities of assumed
-- independent groups of dependendent binary features
ferns :: Likelihood [[Bool]]
ferns vs = f where
    hs = map histog (transpose vs)
    histog = M.fromList .map (head &&& lr) . L.group . sort
        where lr l = fromIntegral (length l) / t :: Double
    t = fromIntegral (length vs)
    f x = negate $ sum $ map log $ zipWith get hs x
        where get m v = fromMaybe (0.1/t) (M.lookup v m)

---------------------------------------------------


-- | Constructs a (multiclass) 'Learner given any 'Dicotomizer' (by creating n features to discriminate each class against the rest)
multiclass :: Dicotomizer -> Learner Vec
multiclass bin exs = c where
    (gs,lbs) = group exs
    f = multiclass' bin gs
    c x = weighted $ zip (labels lbs) (map exp (f x))


multiclass' :: (Num b) => (([x], [x]) -> a -> b) -> [[x]] -> a -> [b]
multiclass' _ [] = error "multiclass applied to 0 classes"
multiclass' _ [_] = error "multiclass applied to 1 class"
multiclass' bin [g1,g2] = (\x -> [x,-x]) . bin (g1,g2)
multiclass' bin l = f where
    fs = map bin (auxgroup l)
    f v = map ($v) fs


auxgroup :: [[a]] -> [([a], [a])]
auxgroup l = map (\(x:xs) -> (x, concat xs)) (rots l) where
    rot _ [] = []
    rot 0 xs = xs
    rot k (x:xs) = rot (k-1) (xs++[x])
    rots xs = map ((flip rot) xs) [0 .. length xs - 1]

-- -- | to do
-- detailed machine prob = classify where
--     (_,f) = machine prob
--     info = snd (group prob)
--     classify v = sortBy (compare `on` snd) (zip (labels info) nordsts)
--         where dsts = map negate $ f v
--               nordsts = map (/minimum (map abs dsts)) dsts
--               on f g = \x y -> f (g x) (g y)


dicodist :: Distance Vec -> Dicotomizer
dicodist d (g1,g2) = f where
    [d1,d2] = map d [g1,g2]
    f x = d2 x - d1 x