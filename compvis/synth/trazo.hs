-- affine alignment of open curves

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

main = prepare >> trazo >> mainLoop

trazo = do
    prots <- newIORef []
    rpoly <- newIORef []
    w <- evWindow 10 "Trazo" (mpSize 20) (Just $ disp rpoly prots) (mouse rpoly prots kbdQuit)
    motionCallback $= Just (mv rpoly)
    return w
  where
    disp rpoly prots st = do
        poly <- readIORef rpoly
        when (length poly > 1) $ do
            n <- get st
            pixelCoordinates (mpSize 20)
            lineWidth $= 3
            setColor 1 0 0
            renderPrimitive LineStrip $ mapM_ vertex poly
--             shcontP (redu 3 poly)
            pointCoordinates (mpSize 20)
            lineWidth $= 1
            text2D 0.9 0.7 (show n)
            protos <- readIORef prots
            when (not (null protos)) $ do
                let candis = map (align n ((feat. fst . whitenBoundary . redu 3) poly)) protos
                    best = fst $ minimumBy (compare `on` snd) candis
                setColor 0 0 1
--                 mapM_ (\p -> renderPrimitive LineStrip $ mapM_ vertex (smooth n $ fst p)) candis
                setColor 1 1 1
                let h = (snd . whitenBoundary . redu 3) poly
                    best' = map l2p $ ht (inv h) (map p2l (smooth n $ best))
                when (h==h && rcond h > 0.001) $  renderPrimitive LineStrip $ mapM_ vertex best'

    mouse _ _ _ st (MouseButton WheelUp) Down _ _ = do
        st $~ (+1)
        postRedisplay Nothing
    mouse _ _ _ st (MouseButton WheelDown) Down _ _ = do
        st $~ (max 0 . subtract 1)
        postRedisplay Nothing
    mouse rpoly prots _ st (Char 'a') Down _ _ = do
        poly <- readIORef rpoly
        prots $~ ((feat.fst . whitenBoundary .redu 3) poly:)
        postRedisplay Nothing
    mouse rpoly _ _ st (Char 'x') Down _ _ = do
        rpoly $= []
        postRedisplay Nothing
    mouse _ _ def _ a b c d = def a b c d

    mv rpoly (Position x y) = do
        rpoly $~ (Pixel (fromIntegral y) (fromIntegral x) :)
        postRedisplay Nothing

--------------------------------------------------------------

redu n = pixelsToPoints (mpSize 20) . douglasPeucker (fromIntegral n)

smooth n = take 51 . invFou 100 n

invFou n w fou = r where
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map (fou) [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)
    c2p (x:+y) = Point x y

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
    wps = map l2p $ ht t (map p2l ps)

p2l (Point x y) = [x,y]
l2p [x,y] = Point x y

shcontP c = do
    setColor 0.5 0 0
    renderPrimitive LineStrip $ mapM_ vertex c
    setColor 0.8 0 0
    pointSize $= 4
    renderPrimitive Points   $ mapM_ vertex c
    setColor 1 0.2 0.2
    when (not (null c)) $ renderPrimitive Points $ vertex (head c)
