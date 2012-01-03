module Vision.IO(
    loadQCams, saveQCams,
    loadRawTracks, saveRawTracks
) where

import Vision.Types
import Numeric.LinearAlgebra
import Util.Misc(Vec,Mat,impossible,splitEvery)

import Vision.Camera(factorizeCamera)
import Util.Homogeneous(Point(..),inHomog)
import Util.Quaternion

import Text.Printf
import Data.List(groupBy,sortBy)
import Data.Function(on)


-- | load cameras with SBA file format
loadQCams :: FilePath -> IO [Mat]
loadQCams file = (map (camFromQuat . map read . words) . lines) `fmap` readFile file
  where
    camFromQuat [s, a, b, c, x, y, z] = camera
        where q = Quat s (fromList [a,b,c])
              r = quatToRot q
              cen = fromList [x,y,z]
              camera = fromBlocks [[r, asColumn cen]]
    camFromQuat _ = impossible "loadQCams"

-------------------------------------------------------

-- | save cameras with SBA file format
saveQCams :: FilePath -> [Mat] -> IO ()
saveQCams file cams = writeFile file (unlines $ map f cams)
  where
    f cam = printf "%f %f %f %f %f %f %f" s a b c x y z
      where
         [x,y,z] = toList (- rot <> cen)
         [a,b,c] = toList v
         Quat s v = rotToQuat rot
         (_,rot,cen) = factorizeCamera cam

-------------------------------------------------------

-- | load points and projections in SBA file format
loadRawTracks :: FilePath -> IO ([Vec], [Proj])
loadRawTracks file = do
    s <- (filter (('#'/=).head.head) . map words . lines) `fmap` readFile file
    let pts = map ((fromList . (++[1])) . map read . take 3) s :: [Vector Double]
        g n [v,x,y] = ((n ::Int,read v :: Int),Point (read x) (read y))
        g _ _ = impossible "loadRawTracks"
        obs = zipWith (map . g) [0..] $ map (splitEvery 3 . drop 4) s
    return (pts, concat obs)


-- | save points and projections in SBA file format
saveRawTracks :: FilePath -> ([Vec], [Proj]) -> IO ()
saveRawTracks file (pts,p) = writeFile file (unlines $ map f tracks)
  where
    f (pt,views) = printf "%f %f %f %d" x y z n ++ vs where
       [x,y,z] = toList $ inHomog pt
       n = length views
       vs = concatMap g views
       g ((_,v),Point x' y') = printf " %d %f %f" v x' y'
    tracks' = groupBy ((==) `on` fst.fst) $ sortBy (compare `on` fst) $ p
    tracks = zip pts tracks'

