import Vision hiding (degree,rot1,rot2,rot3,infoSProb)
import Vision.Multiview
import Vision.Gea

import Text.Printf
import Data.List
import Data.Function(on)
import Util.Misc
import Numeric.LinearAlgebra
import qualified Data.Array as A

--import EasyVision hiding (debug, numCams,(|+|), median)
import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale,triangulate)
import Data.Maybe(fromJust)
import Control.Arrow((&&&))

import LieSolve
import Initialize

someHelix = sparseFromTensor $ mkHelix stdprob
    { numPoints = 11
    , numCams = 30
    , pixelNoise = 2
    , fov = 50*degree
    , minDist = 1
    , maxDist = 3
    } 777

loadData pname = do
    let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
            where f n = "../../data/tracks/"++pname++"/"++n
    p <- loadSVP fp fc fk
    return p

main = do
    p <- loadData "maquette"
    infoSProb p

------------------------------------------------------------------
