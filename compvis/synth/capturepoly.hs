{-# LANGUAGE DoRec #-} -- amazing!!

-- capture a polyline with the mouse and work with it

import EasyVision
import Graphics.UI.GLUT hiding (Point)
import Control.Monad(when)
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier

main = do rec
            prepare
            sz <- findSize
            c <- capturePolyline "Poly" sz a
            a <- analyze "Fourier" sz (\n -> smooth n . feat . redu sz 3) 10 c
            --     a <- analyze "Douglas-Peucker" sz (redu sz) 3 c
          mainLoop

--------------------------------------------------------------

capturePolyline title sz other = do
    w <- evWindow [] title sz (Just disp) (mouse kbdQuit)
    motionCallback $= Just (mv w)
    return w
  where
    disp st = do
        poly <- get st
        pixelCoordinates sz
        lineWidth $= 3
        setColor 1 0 0
        renderPrimitive LineStrip $ mapM_ vertex poly
    mv w (Position x y) = do
        old <- getW w
        putW w $ Pixel (fromIntegral y) (fromIntegral x) : old
        postRedisplay Nothing
        postRedisplay (Just (evW other))
    mouse _ st (Char '\DEL') Down _ _ = do
        st $~ tail
        postRedisplay Nothing
        postRedisplay (Just (evW other))
    mouse _ st (Char 'x') Down _ _ = do
        st $= []
        postRedisplay Nothing
        postRedisplay (Just (evW other))
    mouse def _ a b c d = def a b c d

--------------------------------------------------------------

analyze title sz f n0 c = evWindow n0 title sz (Just disp) (mouse kbdQuit)
  where
    disp st = do
        poly <- getW c
        when (length poly > 1) $ do
            n    <- get st
            setColor 0.5 0.5 0.5
            pixelCoordinates sz
            renderPrimitive LineStrip $ mapM_ vertex poly
            pointCoordinates sz
            shcontP (f n poly)
            text2D 0.9 0.7 (show n)
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        st $~ (+1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        st $~ (max 0 . subtract 1)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d

--------------------------------------------------------------

redu sz n = pixelsToPoints sz . douglasPeucker (fromIntegral n)

smooth n = take 51 . invFou 100 n

invFou n w fou = r where
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map (fou) [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)
    c2p (x:+y) = Point x y

symclose = Closed . (\c -> init c ++ init (reverse c))

feat = fourierPL . symclose

shcontP c = do
    setColor 0.7 0 0
    renderPrimitive LineStrip $ mapM_ vertex c
    setColor 1 0 0
    pointSize $= 4
    renderPrimitive Points   $ mapM_ vertex c
    setColor 1 1 1
    when (not (null c)) $ renderPrimitive Points $ vertex (head c)
