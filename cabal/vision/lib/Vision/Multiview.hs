module Vision.Multiview(
    VisualProblem(..),
    selectSol,
    krms,
    loadProblem', loadData', saveData'
) where

import Vision.Types
import Vision.Epipolar
import Vision.Stereo
import Util.Homogeneous
import Numeric.LinearAlgebra
import Vision.IO

data VisualProblem = VP { rawviews :: Projections Uncalibrated
                        , views    :: Projections Calibrated
                        , kal      :: Calibration
                        , cams     :: Motion
                        , pts      :: Structure
                        , epi      :: EpiPairs
                        }

krms :: VisualProblem -> Double
krms p = 1000 * rms (projs (views p)) ks (cams p) (pts p)
  where
    ks = replicate (nCam (views p)) (ident 3)
    

-- | points in front of cameras
looksRight :: VisualProblem -> Bool
looksRight s = depthOfPoint p c1 > 0 && depthOfPoint p c2 > 0
  where
    v1:v2:_ = v_of_p (views s) 0
    p = toList . inHomog $ pts s !! 0
    c1 = cams s !! v1
    c2 = cams s !! v2

selectSol :: (Motion, Motion) -> VisualProblem -> VisualProblem
selectSol (cams1,cams2) p = if looksRight sol1 then sol1 else sol2 where
    sol1 = recompPts p { cams = cams1 }
    sol2 = recompPts p { cams = cams2 }
    recompPts q = q { pts = linearTriangulation (views q) (cams q) }

----------------------------------------------------------------------

loadData' :: FilePath -> String -> IO ([Proj],Structure,Motion,Calibration)
loadData' path pname = do
    let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
            where f n = path++pname++"/"++n
    (p3,p2) <- loadRawTracks fp
    cs <- loadQCams fc
    k <- loadMatrix fk
    let one = rows k == 3
        ks | one = replicate (length cs) k
           | otherwise = map head (toBlocksEvery 3 3 k)
    return (p2, p3, cs, ks)

-- | with \"s-\" prefix in the name 
saveData' :: FilePath -> String -> [Proj] -> Structure -> Motion -> Calibration -> IO ()
saveData' path pname p2 p3 cs ks = do
    let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
            where f n = path++pname++"/s-"++n
    saveRawTracks fp (p3,p2)
    saveQCams fc cs
    saveMatrix fk "%f" (fromBlocks $ map return ks)

loadProblem' :: FilePath -> String -> IO VisualProblem
loadProblem' path name = do
    (p2,p3,cs,ks) <- loadData' path name
    let kp2 = mkProjections (calibrateProjections ks p2)
    return VP { pts = p3
              , rawviews = mkProjections p2
              , views = kp2
              , cams = cs
              , kal = ks
              , epi = mkEpiObs kp2
              }

