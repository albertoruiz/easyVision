
import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,Point)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import GHC.Float(isDoubleNaN)
import ImagProc.Ipp.Core
import Data.List(minimumBy)
import Numeric.LinearAlgebra hiding ((.*))
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
        sz   = findSize args

    state <- prepare ()

    cam1 <- mplayer (args!!0) sz >>= pointMarker state "left" sz
    cam2 <- mplayer (args!!1) sz >>= pointMarker state "right" sz

    addWindow "both"   sz Nothing  undefined state
    addWindow "rectif" sz Nothing  undefined state
    addWindow "fixed"  sz Nothing  undefined state

    sequence (replicate 25 cam1)

    launch state (worker cam1 cam2)

-----------------------------------------------------------------


worker cam1 cam2 inWindow _ = do

    (im1,_,pts1) <- cam1
    (im2,_,pts2) <- cam2

    when (length pts1 <= 7 || length pts2 <= 7) $ do
        inWindow "both" $ combine id (im1,pts1) (im2,pts2)

    when (length pts1 > 7 && length pts2 > 7) $ do
        let f = estimateFundamental (prep pts2) (prep pts1)
            (e,foc,err) = estimateEssential 3 f
        inWindow "both" $ do
            combine id (im1,pts1) (im2,pts2)
            text2D 0.9 (-0.7) (show ((foc,sturm f)))
        let scale = 0.5
        let (rec1, rec2) = stereoRectifiers f (prep pts1) (prep pts2)
        w1 <- warp (size im1) (scaling scale <> rec1) im1
        w2 <- warp (size im2) (scaling scale <> rec2) im2
        inWindow "rectif" $ do
            drawImage (0.5 .* w1 |+| 0.5 .* w2)
        w3 <- warp (size im2) (inv rec1 <> rec2) im2
        inWindow "fixed" $ do
            drawImage (0.5 .* im1 |+| 0.5 .* w3)

{-

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
        w1 <- warp (size im1) (scaling scale <> rec1) im1
        w2 <- warp (size im2) (scaling scale <> rec2) im2
        inWindow "rectif" $ do
            drawImage (0.5 .* w1 |+| 0.5 .* w2)
        w3 <- warp (size im2) (inv rec1 <> rec2) im2
        inWindow "fixed" $ do
            drawImage (0.5 .* im1 |+| 0.5 .* w3)
-}
    return ()

closestBy f [] p = p
closestBy f hp p = minimumBy (compare `on` f p) hp

distFeat = (dist `on` ipDescriptor)
    where dist u v = norm (u-v)/ fromIntegral (dim u)

distSpatial = (dist `on` ipPosition)
    where dist (Point a b) (Point x y) = sqrt $ abs ((a-x)^2+(b-y)^2)

distComb alpha p q = chk $ alpha*distFeat p q + (1-alpha)*distSpatial p q

chk x | isDoubleNaN x == 0 = x 
      | otherwise          = 500 -- error "NaN"
-----------------------------------------------------------

--prep = map (g.ipPosition) where g (Point x y) = [x,y]

prep = map g where g (Point x y) = [x,y]
----------------------------------------------------------------------------

norm x = pnorm PNorm2 x

mean l = sum l / fromIntegral (length l)

combine f (img1,pts1) (img2,pts2) = do
    let img = 0.5 .* img1 |+| 0.5 .* img2
    drawImage img
    setColor 1 0 0
    lineWidth $= 1
    pointCoordinates (size img1)
    renderPrimitive Lines (mapM_ vertex (zipWith Segment (map f pts1) (map f pts2)))

-------------------------------------------------------------------------------

pointMarker app name sz cam = do
    (cam',ctrl) <- withPause cam
    ref <- newIORef Nothing
    rmarked <- newIORef []
    w <- addWindow name sz (Just (const $ drw ref rmarked)) (marker (rmarked,ref) (kbdcam ctrl)) app
    opts <- createParameters app [ ("h",percent 20)
                                 , ("locrad",intParam 7 1 30)
                                 , ("smooth",intParam 3 0 10)
                                 , ("median",intParam 0 0 10)
                                  --("rad", realParam 7 1 20),
                                  --("dim", intParam 20 1 50)
                                 ]
    return $ do
        ph <- getParam opts "h" :: IO Int
        let h = fromIntegral ph / 100
        smooth <- getParam opts "smooth"
        --rad <- getParam opts "rad"
        --dim <- getParam opts "dim"
        locrad <- getParam opts "locrad"
        meds <- getParam opts "median" :: IO Int
        let diam = 2*locrad+1
        img <- cam' >>= yuvToGray >>= meds `times` (median Mask3x3) >>= scale8u32f 0 1 
        --ips <- getSaddlePoints smooth locrad h 300 dim rad img
        ips <- getCorners smooth locrad h 300 img
        writeIORef ref (Just (img,ips))
        currentWindow $= Just w
        postRedisplay Nothing
        marked <- readIORef rmarked
        return $ (img,ips, if null marked then [] else pixelsToPoints (size img) marked)
  where drw ref rmarked = do
            mb <- readIORef ref
            case mb of
                Nothing -> return ()
                Just (img,ips) -> do
                    drawImage img
                    pixelCoordinates (size img)
                    pointSize $= 4
                    setColor 0.5 0.5 1
                    renderPrimitive Points $ mapM_ vertex ips
                    --drawInterestPoints (size img) ips
                    marked <- readIORef rmarked
                    pointSize $= 2
                    setColor 1 0 0
                    renderPrimitive Points $ mapM_ vertex marked

        marker (rmarked,ref) _ _ (MouseButton LeftButton) Down _ pos@(Position x y) = do
            marked <- readIORef rmarked
            let clicked = Pixel (fromIntegral y) (fromIntegral x)
            Just (_, pts) <- readIORef ref
            let new = closest pts clicked
            writeIORef rmarked (new:marked)
        marker (rmarked,_) _ _ (MouseButton RightButton) Down _ pos@(Position x y) = do
            marked <- readIORef rmarked
            when (not (null marked)) $ writeIORef rmarked (tail marked)
        marker _ def _ a b c d = def a b c d

        closest [] p = p
        closest hp p = minimumBy (compare `on` dist p) hp
            where dist (Pixel a b) (Pixel x y) = (a-x)^2+(b-y)^2

