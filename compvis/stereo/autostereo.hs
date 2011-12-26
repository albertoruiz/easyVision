
import EasyVision
import EasyVision.GUI.OldGUI
import Graphics.UI.GLUT hiding (RGB,Size,Point)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import GHC.Float(isDoubleNaN)
import ImagProc.Ipp.Core
import Data.List(minimumBy)
import Numeric.LinearAlgebra
import Control.Monad(when)
import Vision
import GHC.Float(float2Double,double2Float)
import Util.Stat

--import Ipp.Core

import qualified Data.Map as Map

------------------------------------------------------------

main = do
    sz <- findSize

    (cam1,ctrl1) <- getCam 0 sz >>= withPause
    (cam2,ctrl2) <- getCam 1 sz >>= withPause

    state <- prepare' ()

    addWindow "left" sz Nothing  (const (kbdcam ctrl1)) state
    addWindow "right" sz Nothing (const (kbdcam ctrl2)) state
    addWindow "both" sz Nothing  (const (kbdcam ctrl1)) state
    addWindow "rectif" sz Nothing   (const (kbdcam ctrl1)) state
    addWindow "fixed" sz Nothing   (const (kbdcam ctrl1)) state

    opts <- createParameters       [("h",percent 20),
                                    ("locrad",intParam 3 1 20),
                                    ("smooth",intParam 3 0 10),
                                    ("umb",realParam 0.01 0 0.05),
                                    ("ranUmb", realParam 0.003 0 0.01),
                                    ("scale", realParam 0.5 0 2),
                                    ("rad", realParam 7 1 20),
                                    ("ranProb", realParam 0.9 0.5 1),
                                    ("seeRansac", intParam 0 0 1),
                                    ("dim", intParam 20 1 50)]

    sequence (replicate 25 cam1)
    --sequence (replicate 30 cam2)


    launch' state (worker cam1 cam2 opts)

-----------------------------------------------------------------


worker cam1 cam2 opts inWindow _ = do

    ph <- getParam opts "h" :: IO Int
    let h = fromIntegral ph / 100
    smooth <- getParam opts "smooth"
    umb <- getParam opts "umb"
    rad <- getParam opts "rad"
    dim <- getParam opts "dim"
    locrad <- getParam opts "locrad"
    let diam = 2*locrad+1


    im1 <- cam1 >>= return . scale8u32f 0 1 . yuvToGray
    im2 <- cam2 >>= return . scale8u32f 0 1 . yuvToGray


    let ips1 = getSaddlePoints smooth locrad h 300 dim rad im1

        ips2 = getSaddlePoints smooth locrad h 300 dim rad im2

        (good1, good2 , _,_,_) = basicMatches' (ips1, ips2) distFeat umb


    inWindow "left" $ do
        drawImage im1
        pointCoordinates (size im1)
        setColor 0 0 1
        pointSize $= 2
        renderPrimitive Points (mapM_ vertex (map ipPosition ips1))
        setColor 1 0 0
        pointSize $= 3
        text2D 0.9 0 (show $ length ips1)
        drawInterestPoints good1

    inWindow "right" $ do
        drawImage im2
        pointCoordinates (size im2)
        setColor 0 0 1
        pointSize $= 2
        renderPrimitive Points (mapM_ vertex (map ipPosition ips2))
        setColor 1 0 0
        pointSize $= 3
        text2D 0.9 0 (show $ length ips2)
        drawInterestPoints good2

    when (length good1 <= 10 || length good1 >=50) $ inWindow "both" $ combine ipPosition (im1,good1) (im2,good2)

    when (length good1 > 10 && length good1 <50) $ do
        ranumb <- getParam opts "ranUmb"
        see <- getParam opts "seeRansac"
        prob <- getParam opts "ranProb"
        let (f,inliers) = estimateFundamentalRansac prob ranumb (prep good2) (prep good1)
            (pts2,pts1) = unzip inliers
            (e,foc,err) = estimateEssential 3 f
        inWindow "both" $ do
            if see == (1::Int)
                then combine (\[x,y]->Point x y) (im1,pts1) (im2,pts2)
                else combine ipPosition (im1,good1) (im2,good2)
            text2D 0.9 (-0.7) (show ((foc,sturm f)))
        scale <- getParam opts "scale"
        let (rec1, rec2) = stereoRectifiers f pts1 pts2
        let w1 = warp 0 (size im1) (scaling scale <> rec1) im1
        let w2 = warp 0 (size im2) (scaling scale <> rec2) im2
        inWindow "rectif" $ do
            drawImage (0.5 .* w1 |+| 0.5 .* w2)
        let w3 = warp 0 (size im2) (inv rec1 <> rec2) im2
        inWindow "fixed" $ do
            drawImage (0.5 .* im1 |+| 0.5 .* w3)

    return ()

closestBy f [] p = p
closestBy f hp p = minimumBy (compare `on` f p) hp

norm x = pnorm PNorm2 x

distFeat = (dist `on` ipDescriptor)
    where dist u v = norm (u-v)/ fromIntegral (dim u)

distSpatial = (dist `on` ipPosition)
    where dist (Point a b) (Point x y) = sqrt $ abs ((a-x)^2+(b-y)^2)

distComb alpha p q = chk $ alpha*distFeat p q + (1-alpha)*distSpatial p q

chk x | isDoubleNaN x == 0 = x 
      | otherwise          = 500 -- error "NaN"
-----------------------------------------------------------

prep = map (g.ipPosition) where g (Point x y) = [x,y]

----------------------------------------------------------------------------

mean l = sum l / fromIntegral (length l)

combine f (img1,pts1) (img2,pts2) = do
    let img = 0.5 .* img1 |+| 0.5 .* img2
    drawImage img
    setColor 1 0 0
    lineWidth $= 1
    pointCoordinates (size img1)
    renderPrimitive Lines (mapM_ vertex (zipWith Segment (map f pts1) (map f pts2)))

