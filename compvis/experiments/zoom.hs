-- zoom window: click to center, wheel to zoom

import EasyVision
import Graphics.UI.GLUT hiding (Size)
import System(getArgs)
import Control.Monad(when)
import ImagProc.Ipp.Core
import Foreign

main = do
    sz <- findSize
    file:_ <- getArgs
    prepare
    cam <- mplayer ("mf://"++file) sz
    img <- cam

    zoom "Image" 600 (gray $ channels img)

    mainLoop

--------------------------------------------------------------

zoom title szz img = evWindow (Pixel (h`div`2) (w`div`2),z0) title isz (Just disp) (mouse kbdQuit)
    where
    isz = Size szz szz
    Size h w = size img
    s2 = (szz-1) `div` 2
    z0 = min h w `div` 2
    disp st = do
        k@(p,z) <- get st
        let roi = roiFromPixel z p
            imgz = modifyROI (const roi) img
        drawImage $ resize isz imgz
        pointCoordinates isz
        setColor 0.4 0 0
        when (z>12) renderAxes
        when (z<=12) $ do
            pixelCoordinates (Size (2*z+1) (2*z+1))
            let ROI r1 r2 c1 c2 = theROI imgz
                v = fromIntegral (2*z+1) / fromIntegral szz
                dx = 0.5 - 5*v
                dy = 0.5 + 5*v
            setColor 0.6 0.6 0.6
            renderPrimitive Lines $ sequence_ [vertex (Pixel (r-r1) 0) >> vertex (Pixel (r-r1) (c2-c1+1)) |r<-[r1..r2]]
            renderPrimitive Lines $ sequence_ [vertex (Pixel 0 (c-c1)) >> vertex (Pixel (r2-r1+1) (c-c1)) |c<-[c1..c2]]
            setColor 0 0 0.7
            sequence_ [text2D' (fromIntegral (c-c1) +dx) (fromIntegral (r-r1) +dy) (show $ imgz `val8u` (Pixel r c)) |r<-[r1..r2],c<-[c1..c2]]
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        (p,z) <- get st
        st $= clip (p,z+(max 1 $ z`div`10))
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        (p,z) <- get st
        st $= clip (p,z-(max 1 $ z`div`10))
        postRedisplay Nothing
    mouse _ st (MouseButton LeftButton) Down _ (Position x y) = do
        (Pixel r c,z) <- get st
        st $= clip (Pixel (r+(fromIntegral y-s2)*z`div`s2) (c+(fromIntegral x-s2)*z`div`s2) ,z)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d

    clip (Pixel r c, z) = (Pixel r' c', z') where
        z' = max 1 $ min (min h w`div`2) $ z
        r' = max z' $ min (h-z'-1) $ r
        c' = max z' $ min (w-z'-1) $ c

    text2D' x y s = do
        rasterPos (Vertex2 x (y::GLfloat))
        renderString Helvetica10 s
