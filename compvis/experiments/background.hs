-- contour of something different from a background image

import EasyVision
import Graphics.UI.GLUT hiding (Size,Point)
import Control.Monad(when)
import Numeric.GSL
import Numeric.LinearAlgebra

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz ~> channels >>= withPause
    w <- evWindow (True,undefined) "contour" sz Nothing (mouse $ kbdcam ctrl)
    o <- createParameters [--("threshold",intParam 100 0 255),
                           ("rad",intParam 5 1 10),
                           ("area",percent 80),
                           ("comps",intParam 15 0 50),
                           ("fracpix",realParam 3 0 10),
                           ("show", stringParam "normal" ["normal", "poly", "mask"])]
    launch $ do
        area <- getParam o "area"
        fracpix <- getParam o "fracpix"
        comps <- getParam o "comps"
        rad <- getParam o "rad"
        mode <- getParam o "show"
        orig <- cam
--        inWin w2 $ drawImage (rgb orig)
        (rec,bg) <- getW w
        when rec $ do
            putW w (False,orig)
        when (not rec) $ inWin w $ do
            r <- getROI w
            th' <- getParam o "threshold" :: IO Int
            let img = diffRGB rad bg orig
--              th = fromIntegral th'
                th = otsuThreshold (modifyROI (const r) img)
--                th = autobin img
                mask = binarize8u th img
                -- mask = autobin img
                -- mask = binarize8u (debug $ otsuThreshold (modifyROI (const r) img)) img
            if mode == "mask"
                then drawImage $ copyMask32f (float . gray $ orig) mask
                else drawImage (rgb orig)
--            drawROI $ sugRoi img
            drawROI r

            let (Size h w) = size img
                pixarea = h*w*area`div`1000
                redu = douglasPeuckerClosed fracpix
                cs = map (redu.fst3) $ contours 100 pixarea th True img
            pixelCoordinates (size $ img)
            setColor 1 0 0
            lineWidth $= 1
            when (mode == "poly") $ mapM_ shcont' cs
            pointCoordinates (size img)
            setColor 1 1 0
            lineWidth $= 3
            mapM_ (shcont. invFou 100 comps . fourierPL. Closed . pixelsToPoints (size img)) cs


mouse _ st (Char 's') Down _ _ = do
    (_,b) <- get st
    st $= (True,b)

mouse def _ a b c d = def a b c d

diffRGB r ch1 ch2 = toGray $ filterMax r (rd |+| gd |+| bd)
    where
        rd = float $ sdif (rCh ch1) (rCh ch2)
        gd = float $ sdif (gCh ch1) (gCh ch2)
        bd = float $ sdif (bCh ch1) (bCh ch2)

--sdif a b = absDiff8u (median Mask5x5 a) (median Mask5x5 b)

sdif = absDiff8u

autobin im = otsuThreshold im'
    where im' = modifyROI (const roi) im
          roi = sugRoi im

sugRoi im = roiFromPixel 200 p
    where (_,p) = maxIndx8u im

fst3 (a,_,_) = a

shcont' ( c) = do
    renderPrimitive LineLoop $ mapM_ vertex c

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 5
    renderPrimitive Points (vertex (head c))

invFou n w fou = Closed r where
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map (fou) [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)
    c2p (x:+y) = Point x y