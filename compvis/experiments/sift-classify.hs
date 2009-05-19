-- image classification using interest points

import EasyVision
import Control.Monad(when)
import Graphics.UI.GLUT hiding (Point, Size)
import Data.List(minimumBy)
import Numeric.LinearAlgebra
import Vision
import ImagProc.GPU.SIFT
import EasyVision.MiniApps.SiftParams


main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz ~> channels >>= withPause

    prepare

    sift <- getSift
    os <- userSIFTParams
    matchGPU <- getMatchGPU


    w <- evWindow (False,[]) "video" sz Nothing  (mouse (kbdcam ctrl))
    let d = height sz `div` 10
    evROI w $= ROI d (height sz-d) d (width sz-d)

    r <- evWindow () "category" (mpSize 10)  Nothing  (const (kbdcam ctrl))

    launch (worker cam w r sift os matchGPU)

-----------------------------------------------------------------

worker cam w r sift os matcher = do

    orig <- cam
    sp <- os
    roi <- getROI w


    let Size he wi = size (gray $ orig)
        v   = sift sp (gray orig)
        img = (orig, v)
        inr = inROI' (size (gray orig)) roi . ipPosition

    (click,pats) <- getW w
    when click $ putW w (False, (orig, filter inr v) : pats)

    let dist (_,u) (_,v) = - (fromIntegral . length) (matcher 0.2 u v)


    inWin w $ do
        drawImage (gray orig)
        setColor 0 0 0
        drawROI roi
        setColor 1 1 0
        pointCoordinates (size (gray orig))
        drawInterestPoints v

    when (not (null pats) && not (null v)) $ inWin r $ do
        let x = minimumBy (compare `on` dist img) pats
            d = dist x img
        drawImage $ gray $ fst x
        pointCoordinates (mpSize 5)
        text2D 0.9 0.6 (show $ round d)
        when (d>10) $ do -- low confidence in the classification
            setColor 1 0 0
            lineWidth $= 10
            renderPrimitive Lines $ mapM_ vertex $
                [ Point 1 (-1), Point (-1) 1, Point 1 1, Point (-1) (-1) ]

-----------------------------------------------------

dist (_,u) (_,v) = - (fromIntegral . length) (basicMatches (u,v) distFeat 0.2)

norm = pnorm PNorm2

vector v = fromList v :: Vector Double

--basicMatches (feats0, feats1) distFeat err

distFeat = distv `on` ipDescriptor

distv a b = norm (a-b)

-----------------------------------------------------

-- click to add a new object
mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps) <- get st
    st $= (True,ps)

mouse def _ a b c d = def a b c d
