--experiment...

import EasyVision
--import ImagProc.Ipp.Core(intersection)
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import GHC.Float(float2Double,double2Float)
--import Text.Printf
import Control.Monad
--import Data.List(transpose)


------------------------------------------------------------

main = do

    prepare

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    w <- evWindow () "image" sz Nothing  (const (kbdcam ctrl))
    w2 <- evWindow () "pru" sz Nothing  (const (kbdcam ctrl))

    o <- createParameters [("rad"  ,intParam 10 1 30),
                           ("sigma",realParam 6 0.1 10),
                           ("sigma2",realParam 6 0.1 20),
                           ("thres",realParam 0.4 0 1),
                           ("n",intParam 4 0 20),
                           ("what",intParam 0 0 1)]


    launch $ do
        orig <- cam
        th <- getParam o "thres"
        rad <- getParam o "rad"
        sigma <- getParam o "sigma"
        sigma2 <- getParam o "sigma2"
        --nmax <- getParam o "n"
        what <- getParam o "what" :: IO Int
        let img = (gaussS sigma2
                  . sqrt32f
                  . abs32f
                  -- . thresholdVal32f 0 0 IppCmpGreater
                  . hessian
                  . secondOrder
                  . gaussS sigma
                  . float . gray) orig
            s = sum32f img
            mx = double2Float $ 10*s/fromIntegral (h*w) where (Size h w) = sz
            imgreg = thresholdVal32f (mx*th) 1 IppCmpGreater $ thresholdVal32f (mx*th) 0 IppCmpLess $ img

            area = 1
            fracpix = 2
            pixarea = h*w*area`div`10000 where (Size h w) = sz
            redu = douglasPeuckerClosed fracpix
            cs = map (redu.fst3) $ contours 100 pixarea 128 True (toGray imgreg)

        inWin w2 $ do
            drawImage $ copyMask32f (float $ gray orig) (toGray imgreg)

        inWin w $ do
            let pts = getPoints32f 200 . thresholdVal32f (mx*th) 0 IppCmpLess . localMax (2*rad+1) $ img
            if what == 0
                then drawImage (gray $ orig)
                else drawImage (autoscale img)
            pointCoordinates sz
            pointSize $= 5
            setColor 1 0 0
            when (not (null pts)) $ do
                renderPrimitive Points (mapM_ vertex (pixelsToPoints sz pts))
            pixelCoordinates sz
            setColor 1 0 0
            lineWidth $= 2
            mapM_ shcont cs

fst3 (a,_,_) = a

shcont c = do
    renderPrimitive LineLoop $ mapM_ vertex c

autoscale im = f im
    where (mn,mx) = minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx
