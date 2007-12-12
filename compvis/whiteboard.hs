-- rectification of a rectangle with know aspect ratio 
-- (e.g., an A4 sheet of paper)

import EasyVision
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Vision
import Control.Monad(when)
import Numeric.LinearAlgebra
import Text.Printf(printf)

main = do
    sz <- findSize
    ratio <- getOption "--ratio" (sqrt 2)
    let szA4 = Size (32*10) (round (32*10*ratio))
        nm = "ratio " ++ printf "%.2f" ratio
    prepare

    (cam,ctrl) <- getCam 0 sz >>= findRectangles ratio >>= withPause

    wimg <- evWindow () "original" sz Nothing (const $ kbdcam ctrl)
    wa4  <- evWindow (ident 3) nm szA4 Nothing (mouse (kbdcam ctrl))

    launch (worker cam wimg wa4 ratio szA4)

-----------------------------------------------------------------

worker cam wImage wA4 ratio szA4 = do

    (orig,a4s) <- cam

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
            let pts = head a4s
                a4aux = [[-1,-r],[1,-r],[1,r],[-1,r]]
                    where r = 1/ratio
                h = estimateHomography a4aux (map pl pts)
                    where pl (Point x y) = [x,y]
            putW wA4 h

        h <- getW wA4
        im <- yuvToRGB orig
        drawImage $ warp (0,0,0) szA4 h im

---------------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $= ident 3

mouse def _ a b c d = def a b c d
