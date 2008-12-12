-----------------------------------------------------------------------------
{- |
Module      :  Vision.Estimation
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Tools for robust estimation of homogeneous transformations.

-}
-----------------------------------------------------------------------------

module Vision.Estimation
( -- * General methods and utilities
  homogSystem
, ransac
, ransac'
, withNormalization
  -- * 2D Homography estimation
, estimateHomographyRaw
, estimateHomography
, estimateHomographyRansac
) where

import Numeric.LinearAlgebra
import Util.Stat
import Vision.Geometry
import Data.List(transpose,nub,maximumBy,genericLength,elemIndex)
import System.Random 
import Debug.Trace(trace)

debug x = trace (show x) x

matrix = fromLists :: [[Double]] -> Matrix Double
vector = fromList ::  [Double] -> Vector Double

norm x = pnorm PNorm2 x

-- FIXME: use Matrix Double for the coefficients
-- | Minimum squares solution of a (possibly overconstrained) homogeneous linear system.
--   (We assume that the solution is 1D).
homogSystem :: [[Double]]    -- ^ elements in the coefficient matrix
            -> (Vector Double, Double) -- ^ (Solution, error)
homogSystem coeffs = (sol,err) where
    r = length coeffs
    c = length (head coeffs)
    mat | r >= c   = matrix coeffs
        | r == c-1 = matrix (head coeffs : coeffs)
        | otherwise = error "homogSystem with rows<cols-1"
    (_,s,v) = svd mat
    sol = flatten $ dropColumns (c-1) v
    err = s @> (c-1)

-- FIXME: use a list of tuples insted of two arguments
-- | estimates a homography using the basic linear method
estimateHomographyRaw :: [[Double]] -> [[Double]] -> Matrix Double
estimateHomographyRaw dest orig = h where
    eqs = concat (zipWith eq dest orig)
    h = reshape 3 $ fst $ homogSystem eqs
    eq [bx,by] [ax,ay] = 
        [[  0,  0,  0,t14,t15,t16,t17,t18,t19],
         [t21,t22,t23,  0,  0,  0,t27,t28,t29],
         [t31,t32,t33,t34,t35,t36,  0,  0,  0]] where
            t14=(-ax)
            t15=(-ay)
            t16=(-1)
            t17=by*ax 
            t18=by*ay 
            t19=by
            t21=ax 
            t22=ay 
            t23=1
            t27=(-bx*ax) 
            t28=(-bx*ay) 
            t29=(-bx)
            t31=(-by*ax) 
            t32=(-by*ay) 
            t33=(-by)
            t34=bx*ax 
            t35=bx*ay
            t36=bx

-- | combinator to estimate models with normalized (whitened) coordinates
withNormalization :: (Matrix Double -> Matrix Double) -- ^ left modifier (inv for homographies, trans for fundamental matrices)
                  -> ([[Double]] -> [[Double]] -> Matrix Double) -- ^ estimator
                  -> ([[Double]] -> [[Double]] -> Matrix Double) -- ^ estimator with normalized coordinates
withNormalization lt estimateRelation dest orig = lt wd <> h <> wo where
    std = stat (matrix dest)
    sto = stat (matrix orig)
    nd = toLists (whitenedData std)
    no = toLists (whitenedData sto)
    h = estimateRelation nd no
    wd = whiteningTransformation std
    wo = whiteningTransformation sto

-- | withNormalization inv estimateHomographyRaw
estimateHomography :: [[Double]] -> [[Double]] -> Matrix Double
estimateHomography = withNormalization inv estimateHomographyRaw

------------------------------ RANSAC -------------------------------

partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)
-- take (length l `quot`n) $ unfoldr (\a -> Just (splitAt n a)) l   

on f g = \x y -> f (g x) (g y)

-- | Basic RANSAC, trying a fixed number of models.
--   The samples are pseudorandom using mkStdGen 0.
ransac' :: ([a]->t)         -- ^ estimator (from a sample obtains a model)
       -> (t -> a -> Bool)  -- ^ inlier test
       -> Int               -- ^ minimum number of samples required by the estimator to compute a model
       -> Int               -- ^ number of random models to check
       -> ([a] -> (t,[a]))  -- ^ resulting ransac estimator, from a sample obtains a model and the inliers
ransac' estimator isInlier n t dat = (result, goodData) where
    result = estimator goodData
    goodData = inliers bestModel
    bestModel = maximumBy (compare `on` (length.inliers)) models
    models = take t (map estimator (samples n dat))
    inliers model = filter (isInlier model) dat

-- | @samples n list@ creates an infinite list of pseudorandom (using mkStdGen 0) subsets of n different elements taken from list
samples :: Int -> [a] -> [[a]]
samples n dat = map (map (dat!!)) goodsubsets where
    goodsubsets = filter ((==n).length) $ map nub $ partit n randomIndices
    randomIndices = randomRs (0, length dat -1) (mkStdGen 0)

ransacSize s p eps = 1 + (floor $ log (1-p) / log (1-(1-eps)^s)) ::Int

position fun l = k where Just k = elemIndex (fun l) l


-- | Adaptive RANSAC (see Hartley and Zisserman 2nd ed. sec. 4.7.1, page 117).
--   The samples are pseudorandom using mkStdGen 0.
ransac :: ([a]->t)         -- ^ estimator (from a sample obtains a model)
       -> (t -> a -> Bool) -- ^ inlier test
       -> Int              -- ^ minimum number of samples required by the estimator to compute a model
       -> Double           -- ^ probability to get a sample free from outliers
       -> ([a] -> (t,[a])) -- ^ resulting ransac estimator, from a sample obtains a model and the inliers
ransac estimator isInlier n prob dat = (bestModel,inliers) where 
    models = map estimator (samples n dat)
    inls = map inliers models where inliers model = filter (isInlier model) dat 
    eps = map prop inls where prop l = 1 - genericLength l / genericLength dat
    ns = scanl1 min $ map (ransacSize n prob) eps 
    k = fst $ head $ dropWhile (\(k,n) -> k<n) (zip [1 ..] ns)
    p = position maximum (map length (take k inls))
    bestModel = models!!p
    inliers = inls!!p

--------------------------

isInlierTrans t h (dst,src) = norm (vd - vde) < t 
    where vd  = vector dst
          vde = inHomog $ h <> homog (vector src)

estimateHomographyRansac
    :: Double  -- ^ probability to get a sample free from outliers
    -> Double  -- ^ distance threshold for inliers
    -> ([[Double]]
    -> [[Double]]
    -> (Matrix Double, [([Double], [Double])]))  -- ^ adaptive ransac estimator, obtains the homography and the inliers
estimateHomographyRansac prob dist dst orig = (h,inliers) where 
    h = estimateHomography a b where (a,b) = unzip inliers
    (_,inliers) = ransac estimator (isInlierTrans dist) 4 prob (zip dst orig)
    estimator l = estimateHomographyRaw a b where (a,b) = unzip l

--------------------------

-- TODO: estimateHomographyMinimal (from 4 points, using linearSolve instead of homogSystem)
