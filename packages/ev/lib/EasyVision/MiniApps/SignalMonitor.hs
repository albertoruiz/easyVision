-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.SignalMonitor
Copyright   :  (c) Alberto Ruiz 2009
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.SignalMonitor (
    signalMonitor
) where

import Graphics.UI.GLUT hiding (Matrix, Size, Point, triangulate)
import Text.Printf
import Control.Applicative
import EasyVision.GUI
import ImagProc.Ipp.Core


signalMonitor :: String         -- ^ window title
             -> Int           -- ^ history length
             -> Int            -- ^ window height
             -> (Double -> String) -- ^ monitorization of the mean value
             -> (Double, Double) -- ^ min and max values
             -> IO ([Double] -> IO ()) -- ^ update function
signalMonitor title n h fsh (mn,mx) = do
    let sz = Size h (10*n)
    w <- evWindow [] title sz (Just disp) (const kbdQuit)
    clearColor $= Color4 1 1 1 1
    let f xs = do
            let fixl l = if null l
                            then replicate (length xs) []
                            else map (take (n-1)) l
            vals <- fixl <$> getW w
            putW w (zipWith (:) xs vals)
            postRedisplay (Just (evW w))
    return f
    where
    w = 10*n
    fsc x = m * x + b
        where m = -fromIntegral (h-1) /(mx-mn)
              b = - mn * m + fromIntegral h -1
    disp st = do
        vals <- get st
        setColor 1 0 0
        pointSize $= 5
        pixelCoordinates (Size h w)
        let f x k = vertex (Pixel (round (fsc x)) (10*k))
        let sh vs col pos = do
            col
            renderPrimitive LineStrip $ sequence_ $ zipWith f vs [0..]
            renderPrimitive Points $ sequence_ $ zipWith f vs [0..]
            let m = sum vs / fromIntegral n
                pm = round (fsc m)
            renderPrimitive Lines $ vertex (Pixel pm 0) >> vertex (Pixel pm w)
            text2D (fromIntegral w / 2) (15+15*pos) (fsh m)
        sequence_ $ zipWith3 sh vals (cycle colors) [0..]
        setColor 0 0 0
        text2D 10 15 (show mx)
        text2D 10 (fromIntegral h-5) (show mn)

colors = [setColor 0 0 0,
          setColor 1 0 0,
          setColor 0 0 1,
          setColor 0 1 0,
          setColor 1 1 0]
