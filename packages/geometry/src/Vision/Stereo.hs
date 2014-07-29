-----------------------------------------------------------------------------
{- |
Module      :  Vision.Stereo
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Two-view geometry. (Provisional data types.)

-}
-----------------------------------------------------------------------------

module Vision.Stereo
( -- * Fundamental Matrix
  estimateFundamentalRaw
, estimateFundamental
, estimateFundamentalRansac
, epipolarQuality
, epipoles
, fundamentalFromCameras
, canonicalCameras
  -- * Essential Matrix
, estimateEssential
, estimateEssential'
, bougnoux
, sturm
, qualityOfEssential, qEssen
, camerasFromEssential
, selectCamera, selectCamera'
  -- * 3D reconstruction
, triangulate, triangulate1
, triangulateH
, stereoRectifiers
  -- * Misc
, depthOfPoint
, depthsOfInducedPoint
) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(unitary,norm,null1)
import Numeric.GSL
import Util.Homogeneous
import Util.Estimation
import Util.Rotation
import Vision.Camera(kgen,cameraAtOrigin)
import Util.Stat
import Data.List(transpose,nub,maximumBy,genericLength,elemIndex, genericTake)
import System.Random
import Debug.Trace(trace)
import Util.Misc(vec,Vec, mat,Mat)
import Util.Debug(debugMat)


--------------------- Basic Stereo -----------------------

estimateFundamentalRaw :: [[Double]] -> [[Double]] -> Mat
estimateFundamentalRaw l r = f where
    f = reshape 3 $ fst $ homogSolve (mat eqs)
    eqs = zipWith eq l r
    eq [xl, yl] [xr, yr] = [xl*xr, xl*yr, xl, xr*yl, yl*yr, yl, xr, yr, 1.0]

correctFundamental :: Mat -> Mat
correctFundamental f = f' where
    (u,s,v) = svd f
    s1:s2:_ = toList s
    f' = u <> diag (fromList [s1,s2,0.0]) <> trans v

estimateFundamental :: [[Double]] -> [[Double]] -> Mat
estimateFundamental = withNormalization trans f where
    f l r = correctFundamental (estimateFundamentalRaw l r)

distPointLine [x,y,w] [a,b,c] = sqrt $ (a*x + b*y + c*w)**2 / (a**2+b**2) / w**2

epipolarQuality :: Mat -> [[Double]] -> [[Double]] -> [Double]
epipolarQuality f l r = zipWith fun l r where
    fun [a1,a2] [b1,b2] = distPointLine [a1,a2,1.0] epb where
        epb = toList (f <> vec [b1,b2,1.0])

qualityOfEssential :: Mat -> Double
qualityOfEssential e = (s1-s2)/(s1+s2) where
    s1:s2:_ = toList s
    (_,s,_) = svd e

qEssen :: Mat -> (Double, Double)
qEssen e = (s2/s1,s3/s1) where
    [s1,s2,s3] = toList $ singularValues e

estimateEssential :: Double  -- ^ initial estimate of common focal dist
                  -> Mat -- ^ fundamental matrix
                  -> (Mat, Double, Double) -- ^ (essential matrix, focal dist, error)
estimateEssential f0 fund = (esen,f,err) where
    minimizeS fun xi = minimize NMSimplex2  1e-2 100 (replicate (length xi) 1) fun xi
    cost [x] = qualityOfEssential (kgen x <> fund <> kgen x)
    ([f],_) = minimizeS cost [f0]
    err = cost [f]
    esen' = kgen f <> fund <> kgen f
    (u,s,v) = svd esen'
    esen = u <> diag (vec [1,1,0]) <> trans v

estimateEssential' :: (Double, Double)  -- ^ initial estimates of focal and focal' dists
                   -> Matrix Double     -- ^ fundamental matrix
                   -> (Mat, (Double, Double), Double) -- ^ (essential matrix, (focal, focal'), error)
estimateEssential' (f0,f0') fund = (esen,(f,f'),err) where
    minimizeS fun xi = minimize NMSimplex2 1e-2 100 (replicate (length xi) 1) fun xi
    cost [x,x'] = qualityOfEssential (kgen x' <> fund <> kgen x)
    ([f,f'],_) = minimizeS cost [f0,f0']
    err = cost [f,f']
    esen' = kgen f' <> fund <> kgen f
    (u,s,v) = svd esen'
    esen = u <> diag (vec [1,1,0]) <> trans v

bougnoux :: Mat -> Double
bougnoux fun = sqrt (- a / b) where
    a = (p' <> asMat e' <> i' <> fun <.> p) * (p <> trans fun <.> p')
    b = (p' <> asMat e' <> i' <> fun <> i' <> trans fun <.> p')
    (_,e') = epipoles fun
    i' = diag $ vec [1,1,0]
    p = vec [0,0,1]
    p' = vec [0,0,1]

sturm :: Mat -> [Double]
sturm fun = fs where    
    (u,s,v) = svd fun
    [a,b,_] = toList s
    [[u11, u12, u13],
     [u21, u22, u23],
     [  _,   _,   _]] = toLists (trans u)
    [[v11, v12, v13],
     [v21, v22, v23],
     [  _,   _,   _]] = toLists (trans v) 
    k = a*u13*v13+b*u23*v23
    p0 = u23*v13*k
    q0 = u13*v23*k
    p1 = a*u13*u23*(1-v13^2) + b*v13*v23*(1-u23^2)
    q1 = a*v13*v23*(1-u13^2) + b*u13*u23*(1-v23^2)
    r0 = a^2*u13^2*v13^2 - b^2*u23^2*v23^2
    r1 = a^2*(u13^2+v13^2-2*u13^2*v13^2) - b^2*(u23^2+v23^2-2*u23^2*v23^2)
    r2 = a^2*(1-u13^2)*(1-v13^2) - b^2*(1-u23^2)*(1-v23^2)
    f1 = sqrt (-p0/p1)
    f2 = sqrt (-q0/q1)
    d = sqrt (r1^2 - 4*r0*r2)
    f3 = sqrt $ (-r1 + d)/2/r2
    f4 = sqrt $ (-r1 - d)/2/r2
    fs = filter (>0) [f1,f2,f3,f4]

camerasFromEssential :: Mat -> [Mat]
camerasFromEssential e = [m1,m2,m3,m4] where
    (u,_,v) = svd e
    [_,_,u3] = toColumns u
    w = mat [[ 0,1,0],
             [-1,0,0],
             [ 0,0,1]]
    m1 = u <>       w <> trans v &  u3
    m2 = u <>       w <> trans v & -u3
    m3 = u <> trans w <> trans v &  u3
    m4 = u <> trans w <> trans v & -u3

infixl 1 &
m & v = fromBlocks [[m, asColumn v]]

triangulate1 ms ps = x3d where
    eq [[m11, m12, m13, m14],
        [m21, m22, m23, m24],
        [m31, m32, m33, m34]] [x,y] = [[ m21-y*m31,   m22-y*m32,    m23-y*m33,   m24-y*m34],
                                       [-m11+x*m31,  -m12+x*m32,   -m13+x*m33,  -m14+x*m34],
                                       [y*m11-x*m21, y*m12-x*m22, y*m13-x*m23, y*m14-x*m24]]
    eqs = concat $ zipWith eq (map toLists ms) ps
    a = mat eqs
    (_,_,v) = svd a
    x3d = toList $ inHomog $ flatten $ dropColumns 3 v

triangulate :: [(Mat, [[Double]])] -> [[Double]]
triangulate mps = xs where
    ms = map fst mps
    ps = transpose (map snd mps)
    xs = map (triangulate1 ms) ps

-- many cams and many points per view
triangulateH :: [(Matrix Double, Matrix Double)] -> Matrix Double
triangulateH cms = ps
  where
    eq (c,vs) = [w2-y3,x3-w1,y1-x2]
      where
        [_,x2,x3,y1,_,y3,w1,w2,_] = toRows (trans vs `kronecker` c)
    eqs = fromRows $ concatMap eq cms
    ps = fromRows . map null1 . concat . toBlocksEvery (rows eqs) 4 $ eqs


cameraDirection :: Mat -> Vec
cameraDirection m = unitary (scalar (det a) * m3) where
    a = takeColumns 3 m
    [_,_,m3] = toRows a

depthOfPoint :: [Double] -> Mat -> Double
depthOfPoint p m = (signum (det a) / norm m3) * w3 where
    a = takeColumns 3 m
    [_,_,m3] = toRows a
    w = m <> homog (vec p)
    [_,_,w3] = toList w

depthsOfInducedPoint :: [Double] -> [Double] -> Mat -> Mat -> (Double,Double)
depthsOfInducedPoint p p' m m' = (d,d') where
    d  = depthOfPoint x m
    d' = depthOfPoint x m'
    x = triangulate1 [m,m'] [p,p']

{-# DEPRECATED selectCamera "use selectCamera'" #-}
selectCamera :: [Double] -> [Double] -> Mat -> [Mat] -> Mat
selectCamera p p' m ms = m' where
    [m'] = filter f ms 
    f m' = a > 0 && b > 0 where
        (a,b) = depthsOfInducedPoint p p' m m'


selectCamera' :: [Double] -> [Double] -> Mat -> [Mat] -> Maybe Mat
selectCamera' p p' m ms = m' where
    m's = filter f ms
    f m' = a>0 && b>0 where (a,b) = depthsOfInducedPoint p p' m m'
    m' | null m's  = Nothing
       | otherwise = Just (head m's)


epipoles :: Mat -> (Vec, Vec)
epipoles f = (nullspace f, nullspace (trans f)) where
    nullspace f = flatten $ dropColumns 2 v where (_,_,v) = svd f

canonicalCameras :: Mat -> (Mat, Mat)
canonicalCameras f = (cameraAtOrigin, m2) where
    (_,e') = epipoles f
    m2 = asMat e' <> f  & e'

fundamentalFromCameras :: Mat -> Mat -> Mat
fundamentalFromCameras p p' = asMat e' <> p' <> pinv p where
    e' = p' <> c
    c = flatten $ dropColumns 3 v 
    (_,_,v) = svd $ fromBlocks [[p],
                                [0]]

stereoRectifiers :: Mat -> [[Double]] -> [[Double]] -> (Mat, Mat)
stereoRectifiers fund pts pts' = (h,h') where    -- HZ p.307
    (e,e') = epipoles fund
    [x,y,w] = toList (unitary e')
    x' = x/w
    y' = y/w
    roll = if abs w < 1E-6
        then atan2 y x
        else atan2 y' x'
    r = rot3 (roll)
    q = sqrt (x'^2 + y'^2)
    g = mat [[1,0,0],  -- HZ p.305, better than a conjugate rotation
             [0,1,0],
             [a,0,1]] where a = -1.0/q
    h' = g <> r

    h = ha <> h0
    h0 = h' <> m
    (_,m') = canonicalCameras fund
    m = takeColumns 3 m' + outer e' (vec [1,1,1]) -- hmmm
    ha = mat [[a,b,c],
              [0,1,0],
              [0,0,1]]
    t' = ht h' pts'
    t =  ht h0 pts
    eq [x,y] [x',_] = [x,y,1,x']
    eqs = mat $ zipWith eq t t'
    coef = takeColumns 3 eqs
    term = flatten $ dropColumns 3 eqs
    [a,b,c] = toList $ pinv coef <> term

isInlierFund :: Double -> Mat -> ([Double], [Double]) -> Bool
isInlierFund t f (x',x) = head (epipolarQuality f [x'] [x]) < t

estimateFundamentalRansac
    :: Double     -- ^ probability of obtaining a sample free from outliers
    -> Double     -- ^ distance threshold for inliers
    -> [[Double]] -- ^ pts'
    -> [[Double]] -- ^ pts
    -> (Mat, [([Double], [Double])]) -- ^ (fundamental matrix, inliers)
estimateFundamentalRansac prob dist pts' pts = (f,inliers) where 
    f = estimateFundamental a b where (a,b) = unzip inliers
    (_,inliers) = ransac estimator (isInlierFund dist) 8 prob (zip pts' pts)
    estimator l = estimateFundamentalRaw a b where (a,b) = unzip l

