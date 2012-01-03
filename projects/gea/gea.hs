-- SfM from clean tracks

import qualified Vision.Gea2 as G
import Vision.Types
import Vision.Multiview
import Vision.Epipolar
import Vision.Bootstrap
import ShowReco

import Text.Printf

import System.Environment(getArgs)

----------------------------------------------------------------------

main = do
   p:_ <- getArgs
   test p

----------------------------------------------------------------

tracksPath = "../../data/tracks/"

loadProblem = loadProblem' tracksPath

----------------------------------------------------------------------

gea p = p { pts = rp, cams = rc }
  where
    rc = G.geaFull (epi p) (cams p)
    rp = linearTriangulation (views p) rc

----------------------------------------------------------------------

bootFromRots rots p = selectSol camsol p
  where
    camsol = solveCams rots (epi p)

bootstrap p = bootFromRots rs p
  where
    rs = refineRots q (epi p) (initRots (epi p))
    q e = nEpi e > 30 && s2 e > 0.95

----------------------------------------------------------------------

shRecos = shRecosG cams pts

relocate p = p { pts = newPts, cams = newCams } 
  where (newCams, newPts) = relocateReco (cams p, pts p)

----------------------------------------------------------------------

test name = loadProblem name >>= testGoSmall name

testGoSmall name p = do
    let b = bootstrap p
    printf "boot rmse (x1000): %.2f\n" $ krms b
    
    let g = gea b
    printf "GEA calibrated rmse (x1000): %.2f\n" $ krms g
    runIt $ shRecos name (map relocate [g,b,p])

