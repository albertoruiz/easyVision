-- simpler format for multiview problems

import Numeric.LinearAlgebra
import Vision hiding (selectSol)
import Vision.LASBA
import qualified Vision.Gea2 as G
import Vision.Types
import Vision.IO
import Vision.Multiview
import Vision.Epipolar
import Vision.Bootstrap
import ShowReco
import Util.Misc(diagl,mean,debug,dimString)
import Graphics.Plot

import Text.Printf
import Control.Applicative
import Control.Arrow

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


bootstrapFrom rs' p = bootFromRots rs p
  where
    rs = refineRots q (epi p) rs'
    q e = nEpi e > 30 && s2 e > 0.95

bootstrap p = bootFromRots rs p
  where
    rs = refineRots q (epi p) (initRots (epi p))
    q e = nEpi e > 30 && s2 e > 0.95

----------------------------------------------------------------------

shRecos = shRecosG cams pts

relocate p = p { pts = newPts, cams = newCams } 
  where (newCams, newPts) = relocateReco (cams p, pts p)

----------------------------------------------------------------------

test name = loadProblem name >>= testGo name

testCal name = do
    p <- loadProblem name
    f <- autoCal p
    let p' = recalibrate f p
    testGo name p'

testGo name p = do
    printf "Initial mse: %.2f\n" $ rms (projs $ rawviews p) (kal p) (cams p) (pts p) **2 * 2
    printf "Initial calibrated rmse (x1000): %.2f\n" $ krms p
    
    let s = sba p
    printf "LASBA calibrated rmse (x1000): %.5f\n" $ krms s
    
    let lp = linPoints s
    printf "lin points with optimal cams rmse (x1000): %.2f\n" $ krms lp
    
    let s' = bootFromRots (map rotOfCam (cams s)) p
    printf "boot with optimal rots rmse (x1000): %.5f\n" $ krms s'

    let s'test = bootstrapFrom  (map rotOfCam (cams s)) p
    printf "boot with refined optimal rots rmse (x1000): %.5f\n" $ krms s'test

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

----------------------------------------------------------------------

-- with aligned camera centers, given the rotations, the system for
-- the centers only is degenerate. The system for centers and points 
-- is ok, but of course much more costly.

-- A simple synthetic problem for testing
syntProb = r
  where
    mkcam x y z = syntheticCamera $ easyCamera 40 (x,y,z) (0,0,0) 0
    --cams = [mkcam 0 5 0, mkcam 0 5 1, mkcam 1 5 0, mkcam 1 5 1] `zip` [0..]
    cams = [mkcam 0 5 0, mkcam 0 5 1, mkcam 0 5 2, mkcam 0 5 3] `zip` [0..]
    pts = sequence (replicate 3 [-1,0,1]) `zip` [0..]
    f (x,i) (c,j) = ((i,j), l2p y) where [y] = ht c [x]
    p2 = f <$> pts <*> cams
    l2p [x,y] = Point x y
    ks = map (fst.sepCam.fst) cams
    kp2 = mkProjections (calibrateProjections ks p2)
    r = VP { pts = undefined
           , rawviews = mkProjections p2
           , views = kp2
           , cams = undefined
           , kal = ks
           , epi = mkEpiObs kp2
           }

exper = do
    let p = syntProb
    let b = bootstrap'' p
    printf "boot rmse (x1000): %.2f\n" $ krms b
    
    
    let g = gea b
    printf "GEA calibrated rmse (x1000): %.2f\n" $ krms g
    runIt $ shRecos "synth" (map relocate [b,g])
    
bootstrap' p = bootFromRots rs p
  where
    rs = refineRots q (epi p) (initRots (epi p))
    q e = nEpi e > 7 && s2 e > 0.95

bootstrap'' p = bootFromRots' 50 rs p
  where
    rs = refineRots q (epi p) (initRots (epi p))
    q e = nEpi e > 7 && s2 e > 0.95

qual p = \f -> mean (map (qq f) es)
  where
    es = debug "esens" length $ map snd (esens p)
    qq f e = fst $ qEssen $ diagl[f,f,1] <> e <> diagl [f,f,1]
    esens p = map (fst &&& esen . snd) (epi p)

autoCal p = do
    mplot [fs,qs]
    return f
  where
    g = qual p
    fs = linspace 100 (0.25,4) :: Vector Double
    qs = mapVector g fs
    f = fs @> maxIndex qs

recalibrate f p = p
    { views = kp2
    , kal = ks
    , epi = mkEpiObs kp2 }
  where
    p2 = rawviews p
    ks = map (diagl[f,f,1]<>) (kal p)
    kp2 = mkProjections (calibrateProjections ks (projs p2))

