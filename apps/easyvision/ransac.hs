-- This should work with any video source

import Ipp
import Graphics.UI.GLUT hiding (RGB,Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import Data.List(minimumBy)
import GSL hiding (size)
import Control.Monad(when)
import GHC.Float(double2Float)
import Ipp.Core

main = do
    args <- getArgs

    let sz = Size (480`div`2) (640`div`2)

    (cam, ctrl)  <- cameraRGB (args!!0) (Just sz)

    state <- prepare undefined (0,Nothing, Nothing)

    o <- createParameters state [("h",percent 20),
                                 ("smooth",intParam 3 0 10),
                                 ("alpha",realParam 0.9 0 1),
                                 ("see",realParam 1 0 100)]

    addWindow "camera" sz Nothing (const (kbdcam ctrl)) state
    addWindow "left"   sz Nothing (kbdcont 1)  state
    addWindow "right"  sz Nothing (kbdcont 2) state
    addWindow "cost"  sz Nothing undefined state

    launch state (worker cam o)

-----------------------------------------------------------------

worker cam param inWindow _ (k,v1,v2) = do

    ph <- getParam param "h" :: IO Int
    let h = fromIntegral ph / 100
    smooth <- getParam param "smooth"
    alpha <- getParam param "alpha"
    see <- getParam param "see" :: IO Float

    orig <- cam
    im <- rgbToGray orig >>= scale8u32f 0 1
    ips <- getSaddlePoints smooth 7 h 500 20 10 im

    inWindow "camera" $ do
        drawImage orig
        pointCoordinates (size im)
        setColor 1 0 0
        pointSize $= 3
        text2D 0.9 0 (show $ length ips)
        renderPrimitive Points (mapM_ vertex (map ipPosition ips))

    inWindow "left" $ case v1 of
        Nothing -> drawImage orig
        Just (_,im) -> drawImage im

    inWindow "right" $ case v2 of
        Nothing -> drawImage orig
        Just (_,im) -> drawImage im




    (v1',v2') <- case k of
        0 -> return (Just (ips,im),v1) --return (v1,v2)
        1 -> return (Just (ips,im), v2)
        2 -> return (v1,Just (ips,im))

    when (k>=0) $ case (v1',v2') of
        (Just (pl@(_:_),_),Just(ql@(_:_),_)) -> do
            c <- image (Size (length pl) (length ql))
            let d = [[double2Float $ distComb alpha p q | q <- ql] | p <- pl]
            setData32f c d
            inWindow "cost" $ do
                --set32f 0 c (ROI 100 100 0 320)
                --set32f 0 c (ROI 0 240 100 100)
                (_,m) <- Ipp.minmax c
                print (m,maximum (concat d))
                scale32f (see/30) c >>= drawImage

        _ -> return ()

    return (0,v1',v2')

text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s

kbdcont x st (MouseButton LeftButton) _ _ _ = do
    s@State {ust = (_,v1,v2)} <- readIORef st
    writeIORef st s {ust = (x,v1,v2)}
kbdcont _ _ _ _ _ _ = return ()


---------------------------------------------------------

on f g = \x y -> f (g x) (g y)

closestBy f [] p = p
closestBy f hp p = minimumBy (compare `on` f p) hp

distFeat = (dist `on` ipDescriptor)
    where dist u v = norm (u-v)

distSpatial = (dist `on` ipRawPosition)
    where dist (Pixel a b) (Pixel x y) = sqrt $ fromIntegral ((a-x)^2+(b-y)^2)

distComb alpha p q = alpha*distFeat p q + (1-alpha)*distSpatial p q
