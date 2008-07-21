{-# LANGUAGE CPP #-}

--experiment...

import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import GHC.Float(float2Double,double2Float)
import Control.Monad
--import Data.List(transpose)
-- import Foreign(unsafePerformIO)
-- import Vision(scaling)

-------------------------------

salience s1 s2 = gaussS s2 . sqrt32f . abs32f . hessian . gradients . gaussS s1

blobs sigma1 sigma2 thres rad imgs = pts where
    img = (salience sigma1 sigma2) imgs
    sz = size imgs
    s = sum32f img
    mx = double2Float $ 10*s/fromIntegral (h*w) where (Size h w) = sz
    pts = pixelsToPoints sz . getPoints32f 200 . thresholdVal32f (mx*thres) 0 IppCmpLess . localMax rad $ img

-------------------------------

main = do

    prepare

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    w <- evWindow [] "image" sz Nothing  (mouse (kbdcam ctrl))

    o <- createParameters [("rad"  ,intParam 20 1 30),
                           ("sigma1",realParam 6 0.1 10),
                           ("sigma2",realParam 6 0.1 20),
                           ("thres",realParam 0.4 0 1),
                           ("err",realParam 0.04 0 0.1),
                           ("what",intParam 0 0 3)]

#define PAR(S) S <- getParam o "S"

    launchFreq 25 $ do

        PAR(thres)
        PAR(sigma1)
        PAR(sigma2)
        PAR(what) :: IO Int
        PAR(rad)
        PAR(err)

        orig <- cam
        let img = float . gray $ orig

--        let mask = thresholdVal32f 0.1 0 IppCmpLess $ abs32f (img|-|bg)

        prevpts <- getW w

        let pts = blobs sigma1 sigma2 thres rad img 

        let (g1,g2,b1,b2,_) = basicMatches' (prevpts, pts) distPathPoint err
            matches = zip g1 g2
            ok = length prevpts > 0 && length pts > 0

        let --f1 (a,b) = b:a
            f1 (a,b) = [b,last a]
            f2 p     = [p]

        when (not ok) $ putW w (map f2 pts)

        inWin w $ do
            drawImage img
            pointCoordinates sz
            setColor 1 1 1
            mapM_ shPath prevpts
            text2D 0.9 0.7 $ show $ length $ prevpts

            pointSize $= 5
            when ok $ do
                putW w (map f1 matches ++ map f2 b2)
                setColor 1 0 0
                mapM_ shMatch matches
                renderPrimitive Points $ mapM_ vertex g2
                setColor 0 0 1
                renderPrimitive Points $ mapM_ vertex b2


distPathPoint [] _    = 1000
distPathPoint (p:_) q = distPoints p q

fst3 (a,_,_) = a

shcont c = do
    renderPrimitive LineLoop $ mapM_ vertex c

shMatch (a:_,b) = do
    renderPrimitive Lines $ mapM_ vertex [a,b]

shPath p = renderPrimitive LineStrip $ mapM_ vertex p

autoscale im = f im
    where (mn,mx) = minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx


binarize32f th = thresholdVal32f th 1 IppCmpGreater . thresholdVal32f th 0 IppCmpLess

mouse _ st (MouseButton LeftButton) Down _ _ = do
    return()
mouse def _ a b c d = def a b c d
