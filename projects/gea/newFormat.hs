-- simpler format for multiview problems

import Numeric.LinearAlgebra
import Util.Misc(vec,Vec,Mat,unitary,(#),norm,impossible,debug,splitEvery,arrayOf)

import Vision.Types
import Vision.IO

import Vision
import Vision.Gea hiding (gea)
import Vision.LASBA
import qualified Vision.Gea2 as G
import qualified Vision.Epipolar as Epi

import Util.Quaternion

import Data.Array
import Foreign(unsafePerformIO)
import System.IO
import System.Process
import Text.Printf
import Data.List(groupBy,sortBy)
import Data.Function(on)
import Vision.Multiview
import Vision.Epipolar
import Vision.Bootstrap
import ShowReco


tracksPath = "../../data/tracks/"

----------------------------------------------------------------

flipProb name = do
    (p2,p3,cams,kal) <- loadData' tracksPath name
    saveData' tracksPath name (flipp p2) p3 (flipc cams) (flipk kal)

flipc = map (diag (fromList[1,-1,1]) <>)
flipk = map (diag (fromList[ 1,1,-1]) <>)
flipp = map (\(ij,Point x y) -> (ij,Point (-x) y))

----------------------------------------------------------------

loadProblem = loadProblem' tracksPath

----------------------------------------------------------------------

sba p = p { pts = rp, cams = rc }
  where
    (rp,rc) = laSBA (projs $ rawviews p) (pts p) (cams p) (kal p)

----------------------------------------------------------------------

gea p = p { pts = rp, cams = rc }
  where
    rc = G.geaFull (epi p) (cams p)
    rp = linearTriangulation (views p) rc

----------------------------------------------------------------------

linPoints p = p { pts = rp }
  where
    rp = linearTriangulation (views p) (cams p)

----------------------------------------------------------------------

bootFromRots rots p = selectSol camsol p
  where
    camsol = solveCams rots (epi p)


bootFromRots' n rots p = selectSol camsol p
  where
    camsol = estimatePointsCenters n rots (views p)

----------------------------------------------------------------------

shRecos = shRecosG cams pts

relocate p = p { pts = newPts, cams = newCams } 
  where (newCams, newPts) = relocateReco (cams p, pts p)

----------------------------------------------------------------------

test name = do
    p <- loadProblem name
    printf "Initial mse: %.2f\n" $ rms (projs $ rawviews p) (kal p) (cams p) (pts p) **2 * 2
    printf "Initial calibrated rmse (x1000): %.2f\n" $ krms p
    let s = sba p
    printf "LASBA calibrated rmse (x1000): %.2f\n" $ krms s
    let lp = linPoints s
    printf "lin points with optimal cams rmse (x1000): %.2f\n" $ krms lp
    let s' = bootFromRots (map rotOfCam (cams s)) s
    printf "boot with optimal rots rmse (x1000): %.2f\n" $ krms s'
    let s'' = bootFromRots' 10 (map rotOfCam (cams s)) s
    printf "c+p 10 with optimal rots rmse (x1000): %.2f\n" $ krms s''
    let g = gea p
    printf "GEA calibrated rmse (x1000): %.2f\n" $ krms g
    runIt $ shRecos name (map relocate [p,s,s',s'',g])

