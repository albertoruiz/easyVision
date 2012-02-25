module ShowReco where

import Vision -- hiding (degree,rot1,rot2,rot3,infoSProb)
--import Vision.Multiview
--import Vision.Gea
-- import Text.Printf
-- import Data.List
-- import Data.Function(on)
-- import Util.Misc
import Numeric.LinearAlgebra
-- import qualified Data.Array as A
--


--import EasyVision hiding (numCams,(|+|), median)
--import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (color,Size,scale,triangulate,Matrix,clearColor)

import Vision.GUI


-- import Data.Maybe(fromJust)
-- import Control.Arrow((&&&))

import Control.Monad((>=>))

--runIt f = prepare >> f >> mainLoop
--mat2img' = fromListsF . toLists

--showMat :: String -> Matrix Float -> IO (EVWindow (Matrix Float))
--showMat name m = evWindow m name (Size (rows m) (cols m)) (Just (get>=>drawImage.mat2img')) (const kbdQuit)


shRecosG fc fp name ss = browser3D name (map x ss) (const id)
  where
    x s =  clearColor white [ color black $ (map $ flip (drawCamera 0.1) Nothing) (fc s)
                            , color blue $ Raw $ renderPrimitive Points $ mapM_ (vertex.toList.inHomog) (fp s) ] -- FIXME



{-
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
-}
--------------------------------------------------------------

-- ok, but we must align the reconstructions

-- and precompute something?

--shRecosG = undefined

{-
shRecosG fc fp name ss = do
    evWin3D 0 name 500 (Just disp) (mouseGen acts kbdQuit)
    clearColor $= Color4 1 1 1 1
  where
    disp st = do
        k <- get st
        setColor' black
        mapM_ (flip (drawCamera 0.1) Nothing) (fc $ ss!!k)
        setColor' Col.gray
        --sequence_ $ zipWith indexCam (fc $ ss!!k) [0..]
        pointSize $= 1
        setColor' blue
        renderPrimitive Points $ mapM_ (vertex.toList.inHomog) (fp $ ss!!k)
    acts = [((SpecialKey KeyUp,   Down, modif), \_ k -> min (k+1) n)
           ,((SpecialKey KeyDown, Down, modif), \_ k -> max (k-1) 0)]
    n = length ss - 1
-}

indexCam c s = rasterPos (Vertex3 x y z) >> renderString Helvetica12 (show s)
  where (_,_,cen) = factorizeCamera c
        [x,y,z] = toList cen

----------------------------------------------------

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
