-- shows all rectangles found

module Main where

import EasyVision
import Graphics.UI.GLUT hiding (Matrix, Size, Point,set)
import Vision
import Control.Monad(when)
import Numeric.LinearAlgebra
import Text.Printf(printf)
import Foreign

toGray im = unsafePerformIO $ scale32f8u 0 1 im
toFloat im = unsafePerformIO $ do
    yuvToGray im >>= scale8u32f 0 1

zero out sz = unsafePerformIO $ do
    z <- image sz
    set out (theROI z) z
    return z

zeros sz = repeat (zero (0,50,0) sz)

main = do
    sz <- findSize
    ratio <- getOption "--ratio" (1.5)
    let szA4 = Size (32*10) (round (32*10*ratio))
        nm = "ratio " ++ printf "%.2f" ratio
    prepare

    (cam,ctrl) <- getCam 0 sz >>= findRectangles ratio >>= withPause

    wimg <- evWindow () "original" sz Nothing (const $ kbdcam ctrl)
    wa4  <- evWindow () nm (Size (32*5*5) (round(32*5*ratio))) Nothing (const (kbdcam ctrl))

    launch (worker cam wimg wa4 ratio szA4)

-----------------------------------------------------------------

worker cam wImage wA4 ratio szA4 = do

    (orig',a4s) <- cam
    orig <- yuvToRGB orig'

    inWin wImage $ do
        drawImage orig

        pointCoordinates (size orig)

        setColor 1 0 0
        lineWidth $= 3
        mapM_ (renderPrimitive LineLoop . (mapM_ vertex)) a4s

        setColor 0 1 0
        pointSize $= 5
        mapM_ (renderPrimitive Points . (mapM_ vertex)) a4s

    inWin wA4 $ do
        when (length a4s >0) $ do
            let f pts = fst . rectifyQuadrangle szA4 pts $ orig
                res = blockImage $ take 5 (map return $ map (f) a4s++zeros szA4)
            drawImage res

