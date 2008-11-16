{-# LANGUAGE RecursiveDo #-} -- amazing!!

-- capture a polyline with the mouse and work with it

import EasyVision
import Graphics.UI.GLUT hiding (Point)
import System(getArgs)
import Control.Monad(when)
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier

main = mdo
    prepare
    sz <- findSize
    c <- capturePolyline "Poly" sz a
    a <- analyze (\n -> fou n. redu 3) c
--     a <- analyze redu c
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
        shcontP poly
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

shcontP c = do
    setColor 0.5 0 0
    renderPrimitive LineStrip $ mapM_ vertex c
    setColor 1 0 0
    pointSize $= 3
    renderPrimitive Points   $ mapM_ vertex c
    pointSize $= 10
    when (not (null c)) $ renderPrimitive Points $ vertex (head c)

--------------------------------------------------------------

analyze f c = evWindow 10 "Analyze" (mpSize 20) (Just disp) (mouse kbdQuit)
  where
    disp st = do
        poly <- getW c
        n    <- get st
        pointCoordinates (mpSize 20)
        shcontP (f n poly)
        text2D 0.9 0.7 (show n)
        pixelCoordinates (mpSize 20)
        setColor 0.5 0.5 0.5
        renderPrimitive LineStrip $ mapM_ vertex poly
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        st $~ (+1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        st $~ (max 0 . subtract 1)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d

--------------------------------------------------------------

fou n = prot (take 100 . invFou 100 n . fourierPL . Closed . (\c -> init c ++ init (reverse c))) 

redu n = pixelsToPoints (mpSize 20) . prot (douglasPeucker (fromIntegral n))

prot f l = if length l > 1 then f l else l

invFou n w fou = r where
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map (fou) [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)
    c2p (x:+y) = Point x y
