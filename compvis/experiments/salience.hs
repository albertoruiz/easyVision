--experiment...

import EasyVision
import ImagProc.Ipp.Core(intersection)
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import GHC.Float(float2Double,double2Float)
--import Text.Printf
import Control.Monad
--import Data.List(transpose)
import Foreign(unsafePerformIO)
import Vision(scaling)

-------------------------------

salience s1 s2 = gaussS s2 . sqrt32f . abs32f . hessian . secondOrder . gaussS s1

salience' th n s1 s2 = gaussS s2 . thresholdVal32f th 1 IppCmpGreater . thresholdVal32f th 0 IppCmpLess
                       . filterMax n . sqrt32f . abs32f . hessian . secondOrder . gaussS s1

------------------------------------------------------------

main = do

    prepare

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    w <- evWindow () "image" sz Nothing  (const (kbdcam ctrl))
    w2 <- evWindow () "pru" sz Nothing  (const (kbdcam ctrl))

    let imgzero = constImage 0 sz

    wn <- evWindow () "norm" sz Nothing  (const (kbdcam ctrl))

    ws <- evWindow imgzero "acum" sz Nothing  (const (kbdcam ctrl))

    o <- createParameters [("rad"  ,intParam 10 1 30),
                           ("sigma",realParam 6 0.1 10),
                           ("sigma2",realParam 6 0.1 20),
                           ("thres",realParam 0.4 0 1),
                           ("n",intParam 5 1 20),
                           ("sc",realParam 1 0.2 5),
                           ("what",intParam 0 0 1)]


    launchFreq 25 $ do
        orig <- cam
        th <- getParam o "thres"
        rad <- getParam o "rad"
        sigma <- getParam o "sigma"
        sigma2 <- getParam o "sigma2"
        sc <- getParam o "sc"
        --n <- getParam o "n"
        what <- getParam o "what" :: IO Int

        as <- getW ws

        let imgs = float . warp 0 sz (scaling sc) . gray $ orig

        let --img = (salience' (10*th) n sigma sigma2) imgs
            img = (salience sigma sigma2) imgs
            s = sum32f img
            mx = double2Float $ 10*s/fromIntegral (h*w) where (Size h w) = sz
            imgreg = thresholdVal32f (mx*th) 1 IppCmpGreater $ thresholdVal32f (mx*th) 0 IppCmpLess $ img

            area = 1
            fracpix = 2
            pixarea = h*w*area`div`10000 where (Size h w) = sz
            redu = douglasPeuckerClosed fracpix
            cs = map (redu.fst3) $ contours 100 pixarea 128 True (toGray imgreg)

            acs = as |+| 0.1 .* img
            (m,p) = maxIndx32f acs
            roi = roiFromPixel 40 p
            ns = if m > 1 then g (setROI 0 roi acs) else acs

        inWin wn $ do
            let x = ((sigma^2/10) .*) . sqrt32f . thresholdVal32f (th) 0 IppCmpLess. hessian . secondOrder . gaussS sigma $ imgs
                --(_,m) = minmax x
            drawImage x
            --text2D 40 40 (show m)

        inWin w2 $ do
            drawImage $ copyMask32f (imgs) (toGray imgreg)
            drawROI roi

        inWin w $ do
            let pts = getPoints32f 200 . thresholdVal32f (mx*th) 0 IppCmpLess . localMax rad $ img
            if what == 0
                then drawImage (imgs)
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
            setColor 1 1 1
            drawROI roi

        inWin ws $ do
            if what == 0 then drawImage (rgb orig) else drawImage ns
            drawROI roi

        putW ws ns

fst3 (a,_,_) = a

shcont c = do
    renderPrimitive LineLoop $ mapM_ vertex c

autoscale im = f im
    where (mn,mx) = minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx

roiFromPixel rad (Pixel r c) = ROI (r-rad) (r+rad) (c-rad)  (c+rad)

setROI val roi im = unsafePerformIO $ do
    r <- clone im
    set val (intersection (theROI r) roi) r
    return r

improveROI roi im = im |*| setROI 1 roi (constImage 0.9 (size im))

g im = improveROI roi im where
    (m,p) = maxIndx32f im
    roi = roiFromPixel 40 p

binarize32f th = thresholdVal32f th 1 IppCmpGreater . thresholdVal32f th 0 IppCmpLess
