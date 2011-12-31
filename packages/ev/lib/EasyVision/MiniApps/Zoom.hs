-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.Zoom
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Zoom window.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Zoom (
    zoomer, zoomWindow
)where

import Graphics.UI.GLUT as GL hiding (Size,Point,Matrix,matrix)
import EasyVision.GUI
import ImagProc hiding (r1,r2,c1,c2)
import Control.Monad(when)
import ImagProc.Ipp.Core(val8u)


-- | Creates a \"zoom\" window, in which we can easily observe pixel values. When the zoom level is very large numeric pixel values
--   are also shown. The function returns an update function to change the image to be zoomed (in the same location as the previous one).
--
-- click: center pixel
--
-- mouse wheel: zoom in and zoom out
--
-- q: destroy this window (only)

zoomer :: String        -- ^ window title
       -> Int          -- ^ size in pixels of the zoom window
       -> ImageGray    -- ^ initial image to analyze
       -> IO (ImageGray -> IO ()) -- ^ update function
zoomer title szz img0 = do
    ww <- evWindow (img0,Pixel (h`div`2) (w`div`2),z0,True)
                   title isz (Just disp) (mouse kbdQuit)
    let f im = do
            (_,p,z,ok) <- getW ww
            when ok $ do                 -- inWin w, disp (evSt w) hmm
                putW ww (im,p,z,ok)
                postRedisplay (Just (evW ww))
    return f
    where
    isz = Size szz szz
    Size h w = size img0
    s2 = (szz-1) `div` 2
    z0 = min h w `div` 2
    disp st = do
        (img,p,z,_) <- get st
        let roi = roiFromPixel z p
            imgz = modifyROI (const roi) img
        drawImage $ resize8u InterpNN isz imgz
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
    mouse _ evw (MouseButton WheelUp) Down _ _ = do
        (im,p,z,ok) <- getW evw
        putW evw $ clip (im,p,z+(max 1 $ z`div`10),ok)
        postRedisplay Nothing
    mouse _ evw (MouseButton WheelDown) Down _ _ = do
        (im,p,z,ok) <- getW evw
        putW evw $ clip (im,p,z-(max 1 $ z`div`10),ok)
        postRedisplay Nothing
    mouse _ evw (MouseButton LeftButton) Down _ (Position x y) = do
        (im,Pixel r c,z,ok) <- getW evw
        putW evw $ clip (im, Pixel (r+(fromIntegral y-s2)*z`div`s2) (c+(fromIntegral x-s2)*z`div`s2) ,z, ok)
        postRedisplay Nothing
    mouse _ evw (Char ' ') Down _ _ = do
        (im,Pixel r c,z,ok) <- getW evw
        putW evw (im,Pixel r c,z, not ok)
        postRedisplay Nothing
    mouse _ _ (Char 'q') Down _ _ = do
        Just ww <- get currentWindow
        destroyWindow ww
    mouse def _ a b c d = def a b c d

    clip (im,Pixel r c, z, ok) = (im,Pixel r' c', z', ok) where
        z' = max 1 $ min (min h w`div`2) $ z
        r' = max z' $ min (h-z'-1) $ r
        c' = max z' $ min (w-z'-1) $ c

    text2D' x y s = do
        rasterPos (Vertex2 x (y::GLfloat))
        renderString Helvetica10 s

-- | zoom window as a virtual camera
zoomWindow :: String -> Int -> (a -> ImageGray) -> IO a -> IO (IO a)
zoomWindow winname sz f cam = do
    i0 <- cam
    z <- zoomer winname sz (f i0)
    return $ do
        im <- cam
        z (f im)
        return im

