-- image classification using the LBP texture descriptor

import EasyVision
import Control.Monad(when)
import Graphics.UI.GLUT hiding (Point, histogram)
import Data.List(minimumBy)
import Numeric.LinearAlgebra
import Vision
import Util.Rotation(rot3)
import Util.Misc(degree)

feat = vector . lbpN 8 . resize (mpSize 5) . modifyROI (shrink (100,200))

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withPause

    prepare

    w <- evWindow (False,[]) "video" sz Nothing  (mouse (kbdcam ctrl))

    r <- evWindow () "category" (mpSize 10)  Nothing  (const (kbdcam ctrl))

    launch (worker cam w r)

-----------------------------------------------------------------

worker cam w r = do

    orig <- cam

    let roi = shrink (100,200) (theROI orig)

        v   = feat (fromYUV orig)
        img = (orig, v)

    (click,pats) <- getW w
    when click $ putW w (False, versions orig ++ pats)

    inWin w $ do
        drawImage orig
        setColor 0 0 0
        drawROI roi
        setColor 1 0 0
        drawVector (c1 roi) (c2 roi-5) (20*v)

    when (not $ null pats) $ inWin r $ do
        let x = minimumBy (compare `on` dist img) pats
            d = dist x img
        drawImage $ fst x
        pointCoordinates (mpSize 5)
        text2D 0.9 0.6 (show $ round d)
        when (d>10) $ do -- low confidence in the classification
            setColor 1 0 0
            lineWidth $= 10
            renderPrimitive Lines $ mapM_ vertex $
                [ Point 1 (-1), Point (-1) 1, Point 1 1, Point (-1) (-1) ]

-----------------------------------------------------

dist (_,u) (_,v) = norm (u-v)

norm = pnorm PNorm2

vector v = fromList v :: Vector Double

-----------------------------------------------------

-- click to add a new object
mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps) <- getW st
    putW st (True,ps)

mouse def _ a b c d = def a b c d

-----------------------------------------------------

versions im = map f ws where
    ws = [rot3 (r*degree) <> scaling k | r <- [-20,-10 .. 20], k <- [0.8,1,1.2] ]
    f w = (im, feat x)
        where x = warp 0 (size im) w img
              img = fromYUV im
