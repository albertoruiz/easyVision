-- This should work with any video source

import Ipp
import Graphics.UI.GLUT hiding (RGB,Size,Point)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import Data.List(minimumBy)
import GSL hiding (size)
import qualified GSL
import Control.Monad(when)
import GHC.Float(double2Float,isDoubleNaN)
import Ipp.Core
import Vision

main = do
    args <- getArgs

    let sz = Size (480`div`2) (640`div`2)

    (cam, ctrl)  <- cameraRGB (args!!0) (Just sz)

    state <- prepare undefined (3,Nothing, Nothing)

    o <- createParameters state [("h",percent 20),
                                 ("smooth",intParam 3 0 10),
                                 ("alpha",realParam 0.9 0 1),
                                 ("see",realParam 1 0 10),
                                 ("umb",realParam 0.1 0 0.3),
                                 ("ranumb", realParam 0.01 0 0.1)]

    addWindow "camera" sz Nothing (const (kbdcam ctrl)) state
    addWindow "left"   sz Nothing (kbdcont 1)  state
    addWindow "right"  sz Nothing (kbdcont 2) state
    addWindow "cost"  (Size 300 300) Nothing undefined state
    addWindow "track" sz Nothing undefined state
    addWindow "warp" (Size 400 400) Nothing undefined state

    launch state (worker cam o)

-----------------------------------------------------------------

worker cam param inWindow _ (k,v1,v2) = do

    ph <- getParam param "h" :: IO Int
    let h = fromIntegral ph / 100
    smooth <- getParam param "smooth"
    alpha <- getParam param "alpha"
    see <- getParam param "see"
    umb <- getParam param "umb"
    ranumb <- getParam param "umb"


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




    (v1',v2') <- case k of
        0 -> return (v1,v2)
        1 -> return (Just (ips,im), v2)
        2 -> return (v1,Just (ips,im))
        3 -> return (Just (ips,im),v1) -- track

    when (k>=0) $ case (v1',v2') of
        (Just (pl@(_:_),im1),Just(ql@(_:_),im2)) -> do
            let n1 = length pl
            let n2 = length ql
            --print (n1,n2)
            c <- image (Size n1 n2)
            let t = [[double2Float $ - distComb alpha p q | q <- ql] | p <- pl]
            setData32f c t
            --print t
            inWindow "cost" $ do
                scale32f (-see) c >>= drawImage

            corrs <- corresp umb n1 n2 c (inWindow "left")
            let pizq = map ((pl!!).row) corrs
            let pder = map ((ql!!).col) corrs
            let h = estimateHomographyRansac ranumb  (prep pder) (prep pizq)
            --dispR 2 h

            inWindow "left" $ do
                    drawImage im1
                    pointCoordinates (size im)
                    setColor 1 0 0
                    pointSize $= 3
                    renderPrimitive Points (mapM_ vertex (map ipPosition pizq))

            inWindow "right" $ do
                    drawImage im
                    pointCoordinates (size im2)
                    setColor 1 0 0
                    pointSize $= 3
                    renderPrimitive Points (mapM_ vertex (map ipPosition pder))

            inWindow "track" $ do
                a <- scale32f 0.5 im1
                b <- scale32f 0.5 im2
                c <- a |+| b
                drawImage c
                pointCoordinates (size im2)
                setColor 0 0 0
                pointSize $= 3
                renderPrimitive Points (mapM_ vertex (map ipPosition ips))
                setColor 1 0 0
                pointSize $= 3
                renderPrimitive Points (mapM_ vertex (map ipPosition pizq))
                --renderPrimitive Points (mapM_ vertex (map ipPosition pder))
                setColor 0.5 0 0
                let g a b = [a,b]
                renderPrimitive Lines $ mapM_ vertex $ concat (zipWith g (map ipPosition pizq) (map ipPosition pder))

            inWindow "warp" $ do
                warp (Size 400 400) h im >>= drawImage

        _ -> return ()


    let k' = if k==3 then 3 else 0

    return (k',v1',v2')

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
    where dist u v = norm (u-v)/ fromIntegral (GSL.size u)

distSpatial = (dist `on` ipPosition)
    where dist (Point a b) (Point x y) = sqrt $ abs ((a-x)^2+(b-y)^2)

distComb alpha p q = chk $ alpha*distFeat p q + (1-alpha)*distSpatial p q

chk x | isDoubleNaN x == 0 = x 
      | otherwise          = 500 -- error "NaN"
-----------------------------------------------------------

corresp umb h w simil iw = do
    (v, p@(Pixel r c)) <- maxIndx simil
    --print (-v,p)
    if (r>=h || c>=w) then do val <- getData32f simil
                              print (length val, length (head val))
                              mapM_ print val
                              error "problemas"
                      else return ()
    if (-v) > umb then return []
             else do set32f (-1000) simil ROI {r1=r,r2=r,c1=0,c2=w-1}
                     set32f (-1000) simil ROI {r1=0,r2=h-1,c1=c,c2=c}
                     --iw $ scale32f (-1) simil >>= drawImage
                     --getChar
                     sig <- corresp umb h w simil iw
                     return (p:sig)


prep = map (g.ipPosition) where g (Point x y) = [x,y]