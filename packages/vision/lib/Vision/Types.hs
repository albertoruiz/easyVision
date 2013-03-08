{-# LANGUAGE EmptyDataDecls #-}

module Vision.Types(
    -- * Data Structures for Multiview Reconstruction
    Calibrated, Uncalibrated,
    Structure, Motion, Calibration, Proj,
    Projections(..),
    mkProjections,
    fastAccess, rangeCams, rangePts, commonPoints, ptsVisibleBy,
    trackLengths, viewedPoints,
    -- * Utilities
    calibrateProjections,
    linearTriangulation,
    rms
) where

import Data.Array
import Data.List(foldl',foldl1',group,sort)
import Numeric.LinearAlgebra hiding (i)
import Util.Misc(Vec,Mat,arrayOf,vec,sqr,unitary,intersectSorted)
import qualified Data.Map as M
import Vision.Stereo(triangulate1)
import Util.Homogeneous(inHomog)
import Util.Geometry(Point(..))

type Structure = [Vec]   -- [HPoint3]

type Motion = [Mat]      -- [Rigid3x4]

type Calibration = [Mat] -- [Homography3x3]

-- | the image of a point in a camera
type Proj = ((Int,Int), Point)

data Calibrated
data Uncalibrated

data Projections t = Projections { nCam    :: Int
                                 , nPts    :: Int
                                 , projs   :: [Proj]
                                 , v_of_p  :: Int -> [Int]
                                 , p_of_v  :: Int -> [Int]
                                 , ako     :: (Int,Int) -> Vec
                                 }

mkProjections :: [Proj] -> Projections t
mkProjections p = Projections { nCam = nc
                              , nPts = np
                              , projs = p
                              , v_of_p = (vop!)
                              , p_of_v = (pov!)
                              , ako = fastAccess p
                              }
  where
    pairs = map fst p
    np = maximum (map fst pairs) + 1
    nc = maximum (map snd pairs) + 1
    vop = accumArray (flip (:)) [] (0,np-1) pairs
    pov = accumArray (flip (:)) [] (0,nc-1) (map swap pairs) where swap (a,b) = (b,a)

----------------------------------------------------------------------

calibrateProjections :: Calibration -> [Proj] -> [Proj]
calibrateProjections ks = map f where
    k = arrayOf (map inv ks)
    f ((i,j), Point x y) = ((i,j), Point (x'/w) (y'/w))
        where [x',y',w] = toList (k j <> vec [x,y,1])

----------------------------------------------------------------------

rangeCams :: Projections t -> [Int]
rangeCams p = [0 .. nCam p -1]

rangePts :: Projections t -> [Int]
rangePts p = [0.. nPts p - 1]

-- | prepare an efficient function to get the homogeneous projection of point i in view j with [0,0,0] for missing data.
fastAccess :: [Proj] -> (Int,Int) -> Vec
fastAccess p = f where
    m = M.map (\(Point x y) -> unitary (vec [x,y,1])) (M.fromList p)
    f ij = M.findWithDefault z ij m
    z = vec [0,0,0]

-- | points simultaneously visible in a set of views
commonPoints :: Projections t -> [Int] -> [Int]
commonPoints s = foldl1' intersectSorted . map (sort . (p_of_v s))

-- | union of points visible (in at least two views) in a set of views
ptsVisibleBy :: Projections t -> [Int] -> [Int]
ptsVisibleBy s = map head . filter ((>1).length) . group . sort . concatMap (p_of_v s)

trackLengths :: Projections t -> [Int]
trackLengths s = map (length . v_of_p s) . rangePts $ s

-- | number of points visible in each image
viewedPoints :: Projections t -> [Int]
viewedPoints s = map (length . p_of_v s) $ rangeCams s

----------------------------------------------------------------------

-- | geometric reprojection error of a sparse visual problem in pixel units
rms :: [Proj] -> Calibration -> Motion -> Structure -> Double
rms obs ks cs ps = sqrt (e2/2) where
    n = fromIntegral (length obs)
    c = arrayOf (zipWith (<>) ks cs)
    p = arrayOf ps
    g ac ((i,j), Point x y) = ac + sqr(x-a/w) + sqr(y-b/w)
        where [a,b,w] = toList $ c j <> p i
    e2 = foldl' g 0 obs / n


-- | triangulation of 3d points
linearTriangulation :: Projections Calibrated -> Motion -> Structure
linearTriangulation s cs = newps where
    c = arrayOf cs
    ako' (i,j) = toList . inHomog . ako s $ (i,j)
    newps = map (fromList.(++[1]).f) (rangePts s)
    f p = triangulate1 ms ps where
        (ms,ps) = unzip $ map g (v_of_p s p)
            where g j = (c j, ako' (p,j))

