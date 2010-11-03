module ShowReco where

import Vision hiding (degree,rot1,rot2,rot3,infoSProb)
-- import Vision.Multiview
-- import Vision.Gea
-- 
-- import Text.Printf
-- import Data.List
-- import Data.Function(on)
-- import Util.Misc
import Numeric.LinearAlgebra
-- import qualified Data.Array as A
-- 
import EasyVision hiding (debug, numCams,(|+|), median)
import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale,triangulate)
-- import Data.Maybe(fromJust)
-- import Control.Arrow((&&&))


runIt f = prepare >> f >> mainLoop

shRecoG fc fp name s = do
    evWin3D () name 500 (Just disp) (const kbdQuit)
    clearColor $= Color4 1 1 1 1
  where
    disp st = do
        setColor' black
        mapM_ (flip (drawCamera 0.1) Nothing) (fc s)
        pointSize $= 1
        setColor' blue
        renderPrimitive Points $ mapM_ (vertex.toList.inHomog) (fp s)

--------------------------------------------------------------

-- ok, but we must align the reconstructions

-- and precompute something?

shRecosG fc fp name ss = do
    evWin3D 0 name 500 (Just disp) (mouseGen acts kbdQuit)
    clearColor $= Color4 1 1 1 1
  where
    disp st = do
        k <- get st
        setColor' Col.gray
        mapM_ (flip (drawCamera 0.1) Nothing) (fc $ ss!!k)
        setColor' black
        sequence_ $ zipWith indexCam (fc $ ss!!k) [0..]
        pointSize $= 1
        setColor' blue
        renderPrimitive Points $ mapM_ (vertex.toList.inHomog) (fp $ ss!!k)
    acts = [((SpecialKey KeyUp,   Down, modif), \k -> min (k+1) n)
           ,((SpecialKey KeyDown, Down, modif), \k -> max (k-1) 0)]
    n = length ss - 1

shReco = shRecoG sCam sPts
shRecos = shRecosG sCam sPts

indexCam c s = rasterPos (Vertex3 x y z) >> renderString Helvetica12 (show s)
  where (_,_,cen) = factorizeCamera c
        [x,y,z] = toList cen

----------------------------------------------------

relocateSparse p = p { sPts = newPts, sCam = newCams } where
    (newCams, newPts) = relocateReco (sCam p, sPts p)

relocateReco (cams,pts) = (newCams, newPts) where
    newPts = toRows $ (fromRows pts) <> trans hi
    newCams = map (<>h) cams
    hi = inv h
    h = (4><4) [s,0,0,x,
                0,s,0,y,
                0,0,s,z,
                0,0,0,1]
    things = ihmat . fromRows $ pts
    (m,c) = meanCov things
    [x,y,z] = toList m
    s = sqrt $ maxElement $ eigenvaluesSH' c


hmat m = fromBlocks [[m,1]]
ihmat m = takeColumns c m / dropColumns c m where c = cols m  - 1