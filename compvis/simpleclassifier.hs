-- image classification using the LBP texture descriptor

import EasyVision
import ImagProc.Ipp.Core
import Control.Monad(when,(>=>))
import Graphics.UI.GLUT hiding (Point)
import Data.List(minimumBy)

addFeature fun cam = return $ do
    im' <- cam
    let im = modifyROI (shrink (100,200)) im'
    return (im, fun im)

featLBP sz = addFeature $
    lbpN 8 . resize sz . fromYUV

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= featLBP (mpSize 5) >>= withPause

    prepare

    w <- evWindow (False,[]) "video" sz Nothing  (mouse (kbdcam ctrl))

    r <- evWindow () "category" (mpSize 10)  Nothing  (const (kbdcam ctrl))

    launch (worker cam w r)

-----------------------------------------------------------------

worker cam w r = do

    img@(orig,v) <- cam

    (click,pats) <- getW w
    when click $ putW w (False, img:pats)

    inWin w $ do
        drawImage orig
        pointCoordinates (size orig)
        setColor 0 0 0
        renderAxes
        setColor 1 0 0
        renderSignal (map (*0.5) v)

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

dist (_,u) (_,v) = sum $ map (^2) $ zipWith subtract u v

-----------------------------------------------------

-- click to add a new object
mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps) <- get st
    st $= (True,ps)

mouse def _ a b c d = def a b c d
