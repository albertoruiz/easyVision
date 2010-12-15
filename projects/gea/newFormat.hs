-- simpler format for multiview problems

import Numeric.LinearAlgebra
import Vision
import Vision.LASBA
import qualified Vision.Gea2 as G
import Vision.Types
import Vision.IO
import Vision.Multiview
import Vision.Epipolar
import Vision.Bootstrap
import ShowReco

import Text.Printf

----------------------------------------------------------------

tracksPath = "../../data/tracks/"

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


bootstrap p = bootFromRots rs p
  where
    rs = refineRots q (epi p) (initRots (epi p))
    q e = nEpi e > 30 && s2 e > 0.95

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
    printf "LASBA calibrated rmse (x1000): %.5f\n" $ krms s
    
    let lp = linPoints s
    printf "lin points with optimal cams rmse (x1000): %.2f\n" $ krms lp
    
    let s' = bootFromRots (map rotOfCam (cams s)) p
    printf "boot with optimal rots rmse (x1000): %.5f\n" $ krms s'
    
    let b0 = bootFromRots (initRots (epi p)) p
    printf "boot with init rots rmse (x1000): %.2f\n" $ krms b0
    
    let b = bootstrap p
    printf "boot rmse (x1000): %.2f\n" $ krms b
    
    let s'' = bootFromRots' 10 (map rotOfCam (cams s)) p
    printf "c+p 10 with optimal rots rmse (x1000): %.2f\n" $ krms s''
    
    let g = gea p
    printf "GEA calibrated rmse (x1000): %.2f\n" $ krms g
    runIt $ shRecos name (map relocate [s,s'])
   

test2 name = do
    p <- loadProblem name
    printf "Initial mse: %.2f\n" $ rms (projs $ rawviews p) (kal p) (cams p) (pts p) **2 * 2
    printf "Initial calibrated rmse (x1000): %.2f\n" $ krms p
    
    let tc0 = map cencam (cams p)
        tc = fromRows tc0 - asRow (head tc0)
        tcn = tc / scalar(pnorm Frobenius tc)
    
    let s' = bootFromRots (map rotOfCam (cams p)) p
    printf "boot with optimal rots rmse (x1000): %.5f\n" $ krms s'
    
    disp 6 $ fromBlocks [[tcn,(fromRows $ map cencam (cams s'))]]
    
    runIt $ shRecos name (map relocate [p,s'])


disp k = putStr . dispf k
cencam m = c where (_,_,c) = factorizeCamera m

----------------------------------------------------------------

flipProb name = do
    (p2,p3,cams,kal) <- loadData' tracksPath name
    saveData' tracksPath name (flipp p2) p3 (flipc cams) (flipk kal)

flipc = map (diag (fromList[1,-1,1]) <>)
flipk = map (diag (fromList[ 1,1,-1]) <>)
flipp = map (\(ij,Point x y) -> (ij,Point (-x) y))

