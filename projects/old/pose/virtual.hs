{-# LANGUAGE DoRec #-}
-- testing 3ds models

import EasyVision
import Control.Arrow((***))
import Numeric.LinearAlgebra
import Data.List(sortBy,minimumBy)
import Vision
import Util.Misc(posMax,diagl,Mat,debug,slidingPeriodic,rotateLeft)
import Data.Traversable(traverse)
import Graphics.UI.GLUT as GL hiding (Point, Size)
import Data.Colour.Names
import Control.Monad(when)
import Data.Maybe(isJust)
import EasyVision.GUI.Model3DS
import Data.Function(on)

main = run $ camera ~> grayscale
        >>= wcontours id ~> (id *** contSel)
        >>= showPolygons
        ~> polygonalize defPolygonParam {sides = 6}
        >>= showPolygons
        ~>  id *** rectifier . sortBy (compare `on` negate.area)
        >>= showPose
--        >>= showThings
        >>= timeMonitor

----------------------------------------------------------------------

showPolygons = monitor "Polygon" (mpSize 10) sh
  where
    sh(im,cs) = do
        drawImage' im
        pointCoordinates (size im)
        mapM_ drawContourLabeled cs

----------------------------------------------------------------------

showPose = monitor3D "pose" 400 sh
  where
    sh (im,rectif) = do
        let okrec = diagl[1,1,1] <> (rectif::Mat)
            fim = float im
            floor = warp 1 (Size 256 256) okrec fim
        drawTexture floor $ map (++[-0.01]) [[1,1],[-1,1],[-1,-1],[1,-1]]
        let shCam = flip (drawCamera 0.2) (Just (extractSquare 128 fim))
            mbcam = cameraFromHomogZ0 Nothing (inv okrec)
        setColor' orange
        traverse shCam mbcam
        return ()

----------------------------------------------------------------------

showThings c = do rec
                    r <- monitorWheel (0,0,30) "misc" (mpSize 10) (sh f) c
                    depthFunc $= Just Less
                    f <- prepareModel "../../../data/models3ds/lunarlandernofoil_carbajal.3ds"
                    -- f <- prepareModel "../../data/models3ds/STS.3ds"
                    shadeModel $= Smooth
                    lighting $= Enabled
                    ambient (Light 0) $= Color4 0.1 0.1 0.1 ( 1::GLfloat)
                    diffuse (Light 0) $= Color4 0.9 0.9 0.9 ( 1::GLfloat)
                    position (Light 0) $= Vertex4 1 1 1 1
                    light (Light 0) $= Enabled
                  return r
  where
    sh renderIt k (img,rectif) = do
        clear [DepthBuffer]
        drawImage' img
        clear [DepthBuffer]
        let okrec = diagl[1,1,1] <> rectif
            mbcam = cameraFromHomogZ0 Nothing (inv okrec)
            Just cam = mbcam
        when (isJust mbcam) $ do
            clear [DepthBuffer]
            cameraView cam (4/3) 0.1 100
            preservingMatrix $ do
                GL.scale 0.1 0.1 (0.1::GLfloat)
                block 1 1 3
                block 3 1 1
                block 1 3 1
            translate $ Vector3 0 0 (fromIntegral k/5::GLfloat)
            GL.scale 0.1 0.1 (0.1::GLfloat)
            setColor' green
            renderIt

----------------------------------------------------------------------

ref = [[0,0]
      ,[1,0]
      ,[1,-1]
      ,[-1,-1]
      ,[-1,1]
      ,[0,1]]

----------------------------------------------------------------------

isConcave [Point x1 y1, Point x2 y2, Point x3 y3] = 
        (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

concav = fst . minimumBy (compare `on` snd) . zip [0..] . map isConcave . slidingPeriodic 3

fixConcav (Closed c) = rotateLeft (k+1) $ c
  where
    k = concav c

rectifier [] = ident 3 :: Mat
rectifier (c:_) = estimateHomography (ht (scaling 0.5) ref) (map p2l (fixConcav $ c))
  
p2l (Point x y) = [x,y]

