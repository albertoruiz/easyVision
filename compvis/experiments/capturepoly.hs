{-# LANGUAGE RecursiveDo #-} -- amazing!!

-- capture a polyline with the mouse and work with it

import EasyVision hiding ((.*))
import Graphics.UI.GLUT hiding (Point,Matrix)
import System(getArgs)
import Control.Monad(when)
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Data.IORef
import Data.Array
import Data.List(minimumBy)
import Vision.Geometry

main = mdo
    prepare
    sz <- findSize
    c <- capturePolyline "Poly" sz a
--     a <- analyze (\n -> smooth n . feat . redu 3) c
--     a <- analyze redu c
    a <- analyze (\n -> redu 3) c
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
        shcontP (if length poly > 1 then douglasPeucker 3 poly else poly)
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
    setColor 0.8 0 0
    pointSize $= 4
    renderPrimitive Points   $ mapM_ vertex c
    setColor 1 0.2 0.2
    when (not (null c)) $ renderPrimitive Points $ vertex (head c)

--------------------------------------------------------------

analyze f c = do
    prots <- newIORef []
    w <- evWindow 10 "Analyze" (mpSize 20) (Just $ disp prots) (mouse prots kbdQuit)
    return w
  where
    disp prots st = do
        poly <- getW c
        when (length poly > 1) $ do
            n    <- get st
            setColor 0.5 0.5 0.5
            pixelCoordinates (mpSize 20)
            renderPrimitive LineStrip $ mapM_ vertex poly
            pointCoordinates (mpSize 20)
            shcontP (f n poly)
            text2D 0.9 0.7 (show n)
            protos <- readIORef prots
            when (not (null protos)) $ do
                let candis = map (align n ((feat.redu 3) poly)) protos
                    best = fst $ minimumBy (compare `on` snd) candis
                setColor 0 0 1
--                 mapM_ (\p -> renderPrimitive LineStrip $ mapM_ vertex (smooth n $ fst p)) candis
                setColor 1 1 1
                renderPrimitive LineStrip $ mapM_ vertex (smooth n $ best)


    mouse _ _ st (MouseButton WheelUp) Down _ _ = do
        st $~ (+1)
        postRedisplay Nothing
    mouse _ _ st (MouseButton WheelDown) Down _ _ = do
        st $~ (max 0 . subtract 1)
        postRedisplay Nothing
    mouse prots _ st (MouseButton LeftButton) Down _ _ = do
        poly <- getW c
        prots $~ ((feat.redu 3) poly:)
        postRedisplay Nothing
    mouse _ def _ a b c d = def a b c d

--------------------------------------------------------------


redu n = fst . whitenBoundary . pixelsToPoints (mpSize 20) . douglasPeucker (fromIntegral n)

smooth n = take 51 . invFou 100 n

invFou n w fou = r where
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map (fou) [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)
    c2p (x:+y) = Point x y

mirror [] = []
mirror l = mir l ++ l ++ reverse (mir (reverse l))
    where mir (x@(Pixel r0 c0:_)) = map f . tail . reverse $ x
            where f (Pixel r c) = Pixel (2*r0-r) (2*c0-c)

symclose = Closed . (\c -> init c ++ init (reverse c))

memo f = g where
    m = listArray (-30,30::Int) [f k | k <- [-30..30]]
    g w = m ! w

feat = memo . fourierPL . symclose

norScale f = g where
    s = sqrt $ sum [ f k * conjugate (f k) | k<-[-10..10] ]
    g 0 = 0
    g k = f k / (5*s)

align n f g = (h, pnorm PNorm2 (a <> zv - b)) where
    h 0 = g 0 + d
    h k = g k * z
    zv = a <\> b
    [z] = toList zv
    a = fromLists [[g k] | k<-freqs]
    b = fromList [f k | k<-freqs]
    d = f 0 - g 0
    freqs = [-n..(-1)]++[1..n]

whitenBoundary :: [Point] -> ([Point],Matrix Double)
whitenBoundary ps = (wps,t) where
    (mx,my,cxx,cyy,cxy) = momentsBoundary ps -- closed, hmm
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    t = desp (mx,my) <> rot3 (-a) <> diag (fromList [sqrt (l2/l1),1,1]) <> rot3 (a) <> desp (-mx,-my)
    p2l (Point x y) = [x,y]
    l2p [x,y] = Point x y
    wps = map l2p $ ht t (map p2l ps)