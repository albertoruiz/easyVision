{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 704
{-# LANGUAGE TypeFamilies #-}
#endif
-----------------------------------------------------------------------------
{- |
Module      :  Util.Estimation
Copyright   :  (c) Alberto Ruiz 2006-12
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
, ransacSizeLimit
, ransac
, ransac'
-- * Other
, robustLocation
, withNormalization, withNormalization'
, intersectionManyLines
, mseLine
, procrustes
  -- * 2D Homography estimation
, estimateHomographyRaw
, estimateHomography
, estimateHomographyRansac
-- * groups of transformations
--, estimateEuclidean
, estimateSimilar
, estimateAffine
-- , estimateProjective
) where

import Numeric.LinearAlgebra.Compat hiding (eps)
import Numeric.LinearAlgebra.Util(norm,unitary,diagl,row,(#),(¦))
import Util.Covariance
import Util.Homogeneous
import Data.List(transpose,nub,maximumBy,genericLength,sortBy,minimumBy)
import System.Random
import Util.Misc(mat,vec,Mat,Vec,splitEvery,impossible,posMax,debug,median)
import Data.Function(on)
import Util.Geometry hiding (homog)
#if __GLASGOW_HASKELL__ < 704
import Util.Small(Shaped(..),DArray)
#endif


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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ < 704
withNormalization' :: (Shaped x, Vectorlike x, Shaped y, Vectorlike y,
                       DArray (Shape x) ~ Vec, DArray (Shape y) ~ Vec)
#else
-- | combinator to estimate models with normalized (whitened) coordinates
withNormalization' :: (Vectorlike x, Vectorlike y)
#endif
                   => (Mat -> Mat) -- ^ left modifier (inv for homographies, trans for fundamental matrices)
                   -> ([x] -> [y] -> Mat) -- ^ estimator
                   -> ([x] -> [y] -> Mat) -- ^ estimator with normalized coordinates
withNormalization' lt estimateRelation dest orig = lt wd <> h <> wo
  where
    xd = datMat dest
    xo = datMat orig
    std = covStr xd
    sto = covStr xo
    nd = unsafeMatDat $ encodeRows (isoDist xd std) xd
    no = unsafeMatDat $ encodeRows (isoDist xo sto) xo
    h = estimateRelation nd no
    wd = isoDistTransf xd std
    wo = isoDistTransf xo sto

------------------------------ RANSAC ------------------------------------------


ransacSizeLimit :: Int     -- ^ maximum number of samples
                -> Int     -- ^ elements in each sample
                -> Double  -- ^ confidence (e.g. 0.99)
                -> Double  -- ^ outlier proportion (e.g. 0.2)
                -> Int
ransacSizeLimit mx s p eps | ok = 1 + floor (n/d)
                           | otherwise = mx
  where
    n = log (1-p)
    x = (1-eps)^s
    d = log (1-x)
    r = d/n
    rmx = recip (fromIntegral mx)
    ok = rmx < r



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
    p = debug "ransac" id $ posMax (map length (take k inls))
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

----------------------------------------------------------------------

-- hmm, algebraic error
-- [Vec3]->HPoint2
intersectionManyLines :: [Vec] -> Vec
intersectionManyLines ls | length ls > 1 = fst (homogSolve a)
                         | otherwise = vec [0,0,1]
  where a = fromRows $ map unitary ls

-- [Point2] -> Line2
-- | line passing through several points
mseLine :: [Point] -> [Double]
mseLine ps
    | length ps < 2 = error "mseLine with < 2 points"
    | otherwise = (toList . fst) (homogSolve a)
  where
    a = fromRows (map pt2hv ps)

--------------------------------------------------------------------------------

procrustes :: DMat -> DMat -> (Double,Mat,Vec)
-- ^ orthogonal procustes
--
-- procustes x y = (s,r,t) where x = s (r y + t)
procrustes x y = (s,r,t)
  where
    (mx,_) = meanCov x
    (my,_) = meanCov y
    xc = x - asRow mx
    yc = y - asRow my
    xcs = toRows xc
    ycs = toRows yc
    s = median $ zipWith (\x' y' -> norm x' / norm y') xcs ycs
    syc = scalar s * yc
    k = trans syc <> xc
    (u,_,v) = svd k
    r = v <> diagl (replicate (cols x -1) 1 ++[w]) <> trans u
    w = det (v <> trans u)
    t = scalar (recip s) * mx - r <> my

{-
checkpro1 = do
  y <- randn 10 3
  let r = debug "R" id $ rot2 (30*degree)
      t = fromList [1,2,3]
      s = 0.7
      x = scalar s * (y <> trans r + asRow t)
  print $ procrustes x y

checkpro2 = do
  y <- randn 10 2
  let r = debug "R" id $ subMatrix (0,0) (2,2) (rot3 (30*degree))
      t = fromList [1,2]
      s = 0.7
      x = scalar s * (y <> trans r + asRow t)
  print $ procrustes x y
-}

--------------------------------------------------------------------------------

estimateAffine :: DMat -> DMat -> Matrix Double
estimateAffine y x = trans h # v
  where
    h = (x ¦ 1) <\> y
    d = cols x
    v = row (replicate d 0 ++ [1])

estimateSimilar :: DMat -> DMat -> Matrix Double
estimateSimilar y x = h # v
  where
    (s,r,t') = procrustes y x
    t = asColumn t'
    rh = fromBlocks [[r,t]
                    ,[0,1]]
    h = diagl (replicate d s ++[1]) <> rh
    d = cols x
    v = row (replicate d 0 ++ [1])

