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

-------------------------------

main = do

    prepare

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    w <- evWindow () "image" sz Nothing  (const (kbdcam ctrl))

    o <- createParameters [("rad"  ,intParam 10 1 30),
                           ("sigma1",realParam 6 0.1 10),
                           ("sigma2",realParam 6 0.1 20),
                           ("thres",realParam 0.4 0 1),
                           ("what",intParam 0 0 3)]

#define PAR(S) S <- getParam o "S"

    launchFreq 25 $ do
        orig <- cam
        PAR(thres)
        PAR(sigma1)
        PAR(sigma2)
        PAR(what) :: IO Int
        PAR(rad)

        let imgs = float . gray $ orig

        let img = (salience sigma1 sigma2) imgs
            s = sum32f img
            mx = double2Float $ 10*s/fromIntegral (h*w) where (Size h w) = sz
            imgreg = thresholdVal32f (mx*thres) 1 IppCmpGreater $ thresholdVal32f (mx*thres) 0 IppCmpLess $ img

            area = 1
            fracpix = 2
            pixarea = h*w*area`div`10000 where (Size h w) = sz
            redu = douglasPeuckerClosed fracpix
            cs = map (redu.fst3) $ contours 100 pixarea 128 True (toGray imgreg)

            pts = getPoints32f 200 . thresholdVal32f (mx*thres) 0 IppCmpLess . localMax rad $ img

        inWin w $ do
            case what of
                0 -> drawImage (copyMask32f imgs (toGray imgreg))
                1 -> drawImage (autoscale img)
                2 -> do drawImage (rgb orig)
                        pixelCoordinates sz
                        setColor 1 0 0
                        lineWidth $= 2
                        mapM_ shcont cs
                3 -> do drawImage (rgb orig)
                        pointCoordinates sz
                        pointSize $= 5
                        setColor 1 0 0
                        when (not (null pts)) $ do
                            renderPrimitive Points (mapM_ vertex (pixelsToPoints sz pts))

fst3 (a,_,_) = a

shcont c = do
    renderPrimitive LineLoop $ mapM_ vertex c

autoscale im = f im
    where (mn,mx) = minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx


binarize32f th = thresholdVal32f th 1 IppCmpGreater . thresholdVal32f th 0 IppCmpLess
