-- image classification using interest points

import EasyVision
import Control.Monad(when)
import Graphics.UI.GLUT hiding (Point, Size)
import Data.List(minimumBy)
import Numeric.LinearAlgebra
import Vision

feat = interestPoints 13 0.3

interestPoints n h orig = feats where
    sigmas = take (n+2) $ getSigmas 1 3
    imr = float $ gray $ orig
    feats = take 200 $ fullHessian (surf 2 3) sigmas 100 h imr
    sel = filter (inROI roi . ipRawPosition) feats
    Size he wi = size imr
    roi = shrink (he`div`4 ,wi`div`4) (theROI imr)

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz ~> channels >>= withPause

    prepare

    w <- evWindow (False,[]) "video" sz Nothing  (mouse (kbdcam ctrl))

    r <- evWindow () "category" (mpSize 10)  Nothing  (const (kbdcam ctrl))

    launch (worker cam w r)

-----------------------------------------------------------------

worker cam w r = do

    orig <- cam

    let Size he wi = size (gray $ orig)
        roi = shrink (he`div`4 ,wi`div`4) (theROI $ gray orig)

        v   = feat orig
        img = (orig, v)

    (click,pats) <- getW w
    when click $ putW w (False, img : pats)

    inWin w $ do
        drawImage (gray orig)
        setColor 0 0 0
        drawROI roi
        setColor 1 0 0
        pointCoordinates (size $ gray orig)
        drawInterestPoints $ map ip v

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

distFeat = (distv `on` (ipDescriptor.ip))

distv a b = norm (a-b)

-----------------------------------------------------

-- click to add a new object
mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps) <- get st
    st $= (True,ps)

mouse def _ a b c d = def a b c d

