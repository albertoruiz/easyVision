
import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,Point)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import GHC.Float(isDoubleNaN)
import ImagProc.Ipp.Core
import Data.List(minimumBy)
import GSL hiding (size)
import qualified GSL
import Control.Monad(when)
import Vision
import GHC.Float(float2Double,double2Float)
import Classifier.Stat

--import Ipp.Core

import qualified Data.Map as Map

------------------------------------------------------------

main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)

    let sz = if Map.member "--size" opts
                 then mpSize $ read $ Map.findWithDefault "20" "--size" opts
                 else Size (read $ Map.findWithDefault "480" "--rows" opts)
                           (read $ Map.findWithDefault "640" "--cols" opts)


    (cam1,ctrl1) <- mplayer (args!!0) sz >>= withPause
    (cam2,ctrl2) <- mplayer (args!!1) sz >>= withPause

    state <- prepare ()

    addWindow "left" sz Nothing  (const (kbdcam ctrl1)) state
    addWindow "right" sz Nothing (const (kbdcam ctrl2)) state
    addWindow "both" sz Nothing  (const (kbdcam ctrl1)) state
    addWindow "rectif" sz Nothing   (const (kbdcam ctrl1)) state

    opts <- createParameters state [("h",percent 20),
                                    ("smooth",intParam 3 0 10),
                                    ("umb",realParam 0.01 0 0.05),
                                    ("ranumb", realParam 0.005 0 0.01),
                                    ("rad", realParam 7 1 20),
                                    ("dim", intParam 20 1 50)]

    sequence (replicate 25 cam1)
    --sequence (replicate 30 cam2)


    launch state (worker cam1 cam2 opts)

-----------------------------------------------------------------


worker cam1 cam2 opts inWindow _ = do

    ph <- getParam opts "h" :: IO Int
    let h = fromIntegral ph / 100
    smooth <- getParam opts "smooth"
    umb <- getParam opts "umb"
    rad <- getParam opts "rad"
    dim <- getParam opts "dim"


    im1 <- cam1 >>= yuvToGray >>= scale8u32f 0 1 
    im2 <- cam2 >>= yuvToGray >>= scale8u32f 0 1


    ips1  <- getSaddlePoints smooth 7 h 500 dim rad im1

    ips2 <- getSaddlePoints smooth 7 h 500 dim rad im2

    (good1, good2 , _) <- basicMatches (ips1, ips2) (distFeat) umb


    inWindow "left" $ do
        drawImage im1
        pointCoordinates (size im1)
        setColor 0 0 1
        pointSize $= 2
        renderPrimitive Points (mapM_ vertex (map ipPosition ips1))
        setColor 1 0 0
        pointSize $= 3
        text2D 0.9 0 (show $ length ips1)
        --text2D 0.9 0 (show $ length ips1)
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
        --text2D 0.9 0 (show $ length ips2)
        drawInterestPoints good2

    inWindow "both" $ do
        combine (im1,good1) (im2,good2)

    when (length good1 > 10 && length good1 <50) $ do
        --putStr "... "
        ranumb <- getParam opts "ranumb"
        let (f,inliers) = estimateFundamentalRansac' ranumb (prep good2) (prep good1)
            (pts2,pts1) = unzip inliers
        print (length inliers)
        --print $ mean $ epipolarQuality f pts2 pts1
        --print (sturm f)
        let (rec1, rec2) = stereoRectifiers f pts1 pts2
            [dx,dy] = toList $ inHomog $ rec1 <> fromList [0,0,1::Double]
        w1 <- warp (size im1) (scaling 0.5 <> rec1) im1
        let [dx,dy] = toList $ inHomog $ rec2 <> fromList [0,0,1::Double]
        w2 <- warp (size im2) (scaling 0.5 <> rec2) im2
        inWindow "rectif" $ do
            drawImage (0.5 .* w1 |+| 0.5 .* w2)

    return ()

on f g = \x y -> f (g x) (g y)

closestBy f [] p = p
closestBy f hp p = minimumBy (compare `on` f p) hp

distFeat = (dist `on` ipDescriptor)
    where dist u v = norm (u-v)/ fromIntegral (GSL.size u)

distSpatial = (dist `on` ipPosition)
    where dist (Point a b) (Point x y) = sqrt $ abs ((a-x)^2+(b-y)^2)

distComb alpha p q = chk $ alpha*distFeat p q + (1-alpha)*distSpatial p q

chk x | isDoubleNaN x == 0 = x 
      | otherwise          = 500 -- error "NaN"
-----------------------------------------------------------

prep = map (g.ipPosition) where g (Point x y) = [x,y]

----------------------------------------------------------------------------

isInlierFund t f (x',x) = head (epipolarQuality f [x'] [x]) < t

estimateFundamentalRansac' dist pts' pts = (f,inliers) where 
    f = estimateFundamental a b where (a,b) = unzip inliers
    (_,inliers) = ransacProb 0.9 estimator (isInlierFund dist) 8 (zip pts' pts)
    estimator l = estimateFundamentalRaw a b where (a,b) = unzip l

mean l = sum l / fromIntegral (length l)

combine (img1,pts1) (img2,pts2) = do
    let img = 0.5 .* img1 |+| 0.5 .* img2
    drawImage img
    setColor 1 0 0
    lineWidth $= 1
    pointCoordinates (size img1)
    renderPrimitive Lines (mapM_ drawSeg (zipWith Segment (map ipPosition pts1) (map ipPosition pts2)))

drawSeg s = do
    vertex $ (extreme1 s)
    vertex $ (extreme2 s)

drawInterestPoints ipts = do
    renderPrimitive Points (mapM_ drawPts ipts)
    renderPrimitive Lines  (mapM_ drawOris ipts)
  where 
    drawOris IP {ipPosition = p@(Point x y), ipOrientation = a} = do
        vertex $ p
        vertex $ Point (x+0.05*cos a) (y+0.05*sin a)

    drawPts IP {ipPosition = p@(Point x y)} = do
        vertex $ p

text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s
