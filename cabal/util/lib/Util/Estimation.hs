-----------------------------------------------------------------------------
{- |
Module      :  Util.Estimation
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Tools for robust estimation of homogeneous transformations.

-}
-----------------------------------------------------------------------------

module Util.Estimation
( -- * Homogeneous system
  homogSolve
, homogSolveG
, compact
  -- * RANSAC
, ransac
, ransac'
-- * Other
, robustLocation
, withNormalization
  -- * 2D Homography estimation
, estimateHomographyRaw
, estimateHomography
, estimateHomographyRansac
) where

import Numeric.LinearAlgebra hiding (eps)
import Util.Covariance
import Util.Homogeneous
import Data.List(transpose,nub,maximumBy,genericLength,sortBy,minimumBy)
import System.Random
import Util.Misc(norm,mat,vec,Mat,Vec,splitEvery,impossible,posMax)
import Data.Function(on)


-- | Minimum squared error solution of a (possibly overconstrained) homogeneous linear system.
--   (We assume that the solution is 1D).
homogSolve :: Mat            -- ^ coefficient matrix
           -> (Vec, Double) -- ^ (Solution, error)
homogSolve = homogSolveG rightSV

homogSolveG :: (Mat -> (Vec, Mat))-> Mat -> (Vec,Double)
homogSolveG meth coeffs = (sol,err) where
    r = rows coeffs
    c = cols coeffs
    rs@(r1:_) = toRows coeffs
    m | r >= c   = coeffs
      | r == c-1 = fromRows (r1 : rs)
      | otherwise = error "homogSolve with rows<cols-1"
    (s,v) = meth m
    sol = flatten $ dropColumns (c-1) v
    err = s @> (c-1)


-- | reduced coefficient matrix for a homogeneous linear system
compact :: Int -> Mat -> Mat
compact nr x = takeRows (rows x `min` nr) c
    where (l,v) = eigSH' (trans x <> x)
          c = diag (sqrt (abs l)) <> trans v

-- FIXME: use a list of tuples insted of two arguments
-- | estimates a homography using the basic linear method
estimateHomographyRaw :: [[Double]] -> [[Double]] -> Mat
estimateHomographyRaw dest orig = h where
    eqs = concat (zipWith eq dest orig)
    h = reshape 3 $ fst $ homogSolve (mat eqs)
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
    eq _ _ = impossible "eq in estimateHomographyRaw"

-- | combinator to estimate models with normalized (whitened) coordinates
withNormalization :: (Mat -> Mat) -- ^ left modifier (inv for homographies, trans for fundamental matrices)
                  -> ([[Double]] -> [[Double]] -> Mat) -- ^ estimator
                  -> ([[Double]] -> [[Double]] -> Mat) -- ^ estimator with normalized coordinates
withNormalization lt estimateRelation dest orig = lt wd <> h <> wo
  where
    xd = mat dest
    xo = mat orig
    std = covStr xd
    sto = covStr xo
    nd = toLists $ encodeRows (isoDist xd std) xd
    no = toLists $ encodeRows (isoDist xo sto) xo
    h = estimateRelation nd no
    wd = isoDistTransf xd std
    wo = isoDistTransf xo sto

-- | withNormalization inv estimateHomographyRaw
estimateHomography :: [[Double]] -> [[Double]] -> Mat
estimateHomography = withNormalization inv estimateHomographyRaw

------------------------------ RANSAC -------------------------------



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
    goodsubsets = filter ((==n).length) $ map nub $ splitEvery n randomIndices
    randomIndices = randomRs (0, length dat -1) (mkStdGen 0)

ransacSize :: Int -> Double -> Double -> Int
ransacSize s p eps = 1 + (floor $ log (1-p) / log (1-(1-eps)^s)) ::Int


-- | Adaptive RANSAC (see Hartley and Zisserman 2nd ed. sec. 4.7.1, page 117).
--   The samples are pseudorandom using mkStdGen 0.
ransac :: ([a]->t)         -- ^ estimator (from a sample obtains a model)
       -> (t -> a -> Bool) -- ^ inlier test
       -> Int              -- ^ minimum number of samples required by the estimator to compute a model
       -> Double           -- ^ desired probability to get a sample free from outliers. Recommended = 0.99
       -> ([a] -> (t,[a])) -- ^ resulting ransac estimator, from a sample obtains a model and the inliers
ransac estimator isInlier n prob dat = (bestModel,okinliers) where 
    models = map estimator (samples n dat)
    inls = map inliers models where inliers model = filter (isInlier model) dat 
    eps = map prop inls where prop l = 1 - genericLength l / genericLength dat
    ns = scanl1 min $ map (ransacSize n prob) eps 
    k = fst $ head $ dropWhile (\(j,m) -> j<m) (zip [1 ..] ns)
    p = posMax (map length (take k inls))
    bestModel = models!!p
    okinliers = inls!!p

--------------------------

isInlierTrans :: Double -> Mat -> ([Double], [Double]) -> Bool
isInlierTrans t h (dst,src) = norm (vd - vde) < t 
    where vd  = vec dst
          vde = inHomog $ h <> homog (vec src)

estimateHomographyRansac
    :: Double  -- ^ distance threshold for inliers
    -> ([[Double]]
    -> [[Double]]
    -> (Mat, [([Double], [Double])]))  -- ^ adaptive ransac estimator, obtains the homography and the inliers
estimateHomographyRansac dist dst orig = (h,inliers) where
    h = estimateHomography a b where (a,b) = unzip inliers
    (_,inliers) = ransac estimator (isInlierTrans dist) 4 0.99 (zip dst orig)
    estimator l = estimateHomographyRaw a b where (a,b) = unzip l

-------------------------------------------------

{- | PedroE's algorithm. For example, with @dist x y = abs (x-y)@ we have:

@> robustLocation dist [1,2,3,11,12,13,14,15::Double]
[(1.0,0.0),(1.0,1.0),(2.0,1.0),(12.0,2.0),(13.0,2.0),(11.0,8.0),(11.0,9.0),(11.0,10.0)]@

-}
robustLocation :: Ord b => (a -> a -> b) -> [a] -> [(a,b)]
robustLocation dis l = mins where
    mins = map (minimumBy (compare `on` snd)) dst
    dst = transpose ds
    ds = map getdis l
    getdis p = sortBy (compare `on` snd) [(p, dis p y) | y<-l]

