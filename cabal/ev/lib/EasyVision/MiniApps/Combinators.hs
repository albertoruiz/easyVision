-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.Combinators
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Camera combinators: higher order functions which make virtual cameras from other (virtual or not) cameras.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Combinators (
  -- * Camera combinators
  -- | A few useful combinators
  camera,
  withPause,
  addSmall,
  detectMotion,
  detectStatic,
  temporalEnvironment,
  warper,
  findPolygons, getPolygons, polyconsis,
  findRectangles,
  onlyRectangles,
  rectifyQuadrangle,
  monitor, observe, run,
  counter, countDown,
  frameRate, compCost, timeMonitor,
)where

import ImagProc.Ipp.Core
import ImagProc
import ImagProc.Util
--import ImagProc.C.Segments
import Features.Polyline
import Foreign
import Foreign.C.Types (CChar,CUChar)
import Foreign.C.String(newCString)
import Data.IORef
import System.IO
import System.Process
import System.Exit
import System.IO.Unsafe(unsafeInterleaveIO)
import EasyVision.GUI
import Graphics.UI.GLUT hiding (Size,Point)
--import Control.Concurrent
import Vision
import Util.Rotation
import Util.Misc(degree)
import Vision.Autofrontal(autoOrthogonality)
import Numeric.LinearAlgebra
import ImagProc.Generic
import Debug.Trace
import Data.List(tails)
import Control.Monad(when,(>=>),forever)
import ImagProc.Camera(mpSize)
import Features.Segments
import System.CPUTime
import System.Time
import Text.Printf
--import ImagProc.C.Segments

type SegmentExtractor = Int
         -> Float
         -> Int
         -> CUChar
         -> CUChar
         -> Bool
         -> ImageGray
         -> [Segment]


debug x = trace (show x) x


{- | Adds a pause control to a camera. Commands:

    \"pause\" -> toggles the pause state

    \"step\"  -> toggles the frame by frame state (the next frame is obtained by \"pause\")

-}
withPause :: IO a                       -- ^ original camera
          -> IO (IO a, String -> IO ()) -- ^ camera and controller
withPause grab = do
    paused <- newIORef False
    frozen <- newIORef undefined
    step   <- newIORef False
    pass   <- newIORef True

    let control command = do
        case command of
         "pause" -> do modifyIORef paused not
                       p <- readIORef paused
                       if p then grab >>= writeIORef frozen
                            else return ()
         "step"   -> modifyIORef step not
         "pass"   -> modifyIORef pass not

    let virtual = do
        s <- readIORef step
        p <- readIORef paused
        g <- readIORef pass
        let grab' = if g then grab >> readIORef frozen else readIORef frozen
        if not s && p
             then grab'
             else 
                if s then if p then grab'
                               else do writeIORef paused True
                                       grab >>= writeIORef frozen
                                       readIORef frozen
             else grab

    return (virtual,control)

----------------------------------------------------------------------

cameraFolder = do
    path <- optionString "--photos" "."
    sz <- findSize
    imgs <- readFolder path (Just sz)
    let disp rk = do
           k <- get rk  
           drawImage (imgs!!k)
    w <- evWindow 0 ("Folder: "++path) (mpSize 10) (Just disp) (mouseGen (acts (length imgs -1)) kbdQuit)               
    return $ do
        k <- getW w    
        return (channels $ imgs!!k)
  where
    acts n = [((MouseButton WheelUp,   Down, modif), \_ k -> min (k+1) n)
             ,((MouseButton WheelDown, Down, modif), \_ k -> max (k-1) 0)]

cameraV = findSize >>= getCam 0 ~> channels

-- | returns the camera 0. It also admits --photos=path/to/folder/ with images.
camera :: IO (IO Channels)
camera = do
    f <- hasValue "--photos"
    if f then cameraFolder
         else cameraV

----------------------------------------------------------------------

-- | Motion detector with a desired condition on the absolute pixel difference in the roi of two consecutive images (computed with help of 'addSmall').
detectMotion :: (Double -> Bool) -> IO (ImageYUV, ImageGray) -> IO (IO (ImageYUV, ImageGray))
detectMotion cond = virtualCamera (detectMov' cond)

detectMov' cond ((a,f):(b,g):t) =
    if cond (absdif f g)
        then (a,f) : detectMov' cond ((b,g):t)
        else detectMov' cond ((b,g):t)

absdif a b = sum8u (absDiff8u a b)

-- | Given a yuv camera adds a gray version (typically small), to be used with other combinators like 'detectMov'
addSmall :: Size -> IO ImageYUV -> IO (IO (ImageYUV, ImageGray))
addSmall sz grab = return $ do
    im <- grab
    let f = resize sz (fromYUV im)
    return (im,f)

-- | Camera combinator which creates a list of the past and future values of a property in each frame (and applies a given function to it).
temporalEnvironment :: ([p] -> x) -- ^ function to apply to the temporal enviroment (you can of course use id)
                    -> Int        -- ^ number of frames in the past
                    -> Int        -- ^ number of frames in the future
                    -> IO (a,p)   -- ^ source camera
                    -> IO (IO (a, x)) -- ^ result
temporalEnvironment g past future = virtualCamera f where
    f = map (adjust . unzip) . slidingGroup time
    time = past + future + 1
    adjust (cams, env)  = (cams!!past, g env)
    slidingGroup n = map (take n) . tails

----------------------------------------------------------
-- Detector of static frames

addDiff = virtualCamera auxDif
    where auxDif ((a,f):(b,g):t) = (a, nrm (size f) $ absdif f g) : auxDif ((b,g):t)
          nrm (Size r w) = (/n) where n = fromIntegral (r*w*255)

data StaticDetectorState = SDSGetIt
                         | SDSWaitUp
                         | SDSWaitDown
                         deriving (Eq,Show)

auxStatic th SDSGetIt x = SDSWaitUp

auxStatic th SDSWaitUp x | last x > 5*th = SDSWaitDown
                         | otherwise        = SDSWaitUp

auxStatic th SDSWaitDown x | maximum x < th = SDSGetIt
                           | otherwise      = SDSWaitDown

addSDS th = virtualCamera (f SDSWaitUp)
    where f st ((c,p):rest) = (c,(p,st')) : f st' rest
               where st' = auxStatic th st p

getIt xs = [c | (c,(_,st)) <- xs, st == SDSGetIt ]

monitorStatic th (imag,(env,st)) = do
    drawImage imag
    setColor 1 0 0
    text2D 30 30 (show st)
    pointCoordinates (size imag)
    lineWidth $= 2
    renderSignal (map (10*) env)
    setColor 0.5 0 0
    lineWidth $= 1
    renderAxes
    renderPrimitive Lines $ mapM_ vertex [Point (-0.1) (10*th), Point 0.1 (10*th)]
    renderPrimitive Lines $ mapM_ vertex [Point (-0.1) (5*10*th), Point 0.1 (5*10*th)]

-- | Detector of static frames.
detectStatic :: Double -- ^ threshold (.e.g., 0.01)
             -> Int    -- ^ number of frames which must be \"quiet\" (e.g. 5)
             -> IO ImageYUV -- ^ source camera
             -> IO (IO (ImageYUV)) -- ^ virtual camera
detectStatic th nframes =
    addSmall (mpSize 3)
    >=> addDiff
    -- >=> temporalEnvironment mean 3 0  -- for local extremes
    >=> temporalEnvironment id nframes 0
    >=> addSDS th
    >=> monitor "temp env" (mpSize 10) (monitorStatic th)
    >=> virtualCamera getIt

---

mean l = sum l / fromIntegral (length l)

-- std l = sqrt $ mean $ map ((^2).subtract (mean l)) l
-- 
-- posMax l = p where
--     Just p = elemIndex (maximum l) l
-- 
-- posMin l = p where
--     Just p = elemIndex (minimum l) l

---------------------------------------------------------

-- | Generic display function for a camera combinator. The monitor function is invoked in the window with the grabbed object.
--   The window includes a withPause controller.
monitor :: String     -- ^ window name
        -> Size       -- ^ window size
        -> (a->IO ()) -- ^ monitor function
        -> (IO a)     -- ^ original camera
        -> IO (IO a)  -- ^ new camera
monitor name sz fun cam = do
    (cam', ctrl) <- withPause cam
    w <- evWindow () name sz Nothing (const (kbdcam ctrl))
    return $ do
        thing <- cam'
        inWin w (fun thing)
        return thing

---------------------------------------------------------

countDown :: Int -> IO a -> IO (IO a)
countDown tot cam = do
    vn <- newIORef tot
    return $ do
        n <- readIORef vn
        if n==0 then exitWith ExitSuccess else writeIORef vn (n-1)
        cam

counter :: IO a -> IO (IO a)
counter cam = do
    vn <- newIORef 1
    return $ do
        x <- cam
        n <- readIORef vn
        putStrLn $ "#" ++ show n
        writeIORef vn (n+1)
        return x

---------------------------------------------------------

observe :: (Drawable b, Image b) => String -> (a -> b) -> IO a -> IO (IO a)
observe winname f = monitor winname (mpSize 20) (drawImage'.f)

run :: IO (IO a) -> IO ()
run c = prepare >> (c >>= launch . (>> return ()))

-----------------------------------------------------------

frameRate cam = do
    t0 <- getClockTime
    t <- newIORef (t0,40)
    return $ do
            (t0,av) <- readIORef t
            r <- cam
            t1 <- getClockTime
            let dt = diffClockTimes t1 t0
            let delta = (fromIntegral (tdSec dt) * 10^12 + tdPicosec dt) `div` 10^9
                av' = av *0.99 + 0.01* fromIntegral delta
            writeIORef t (t1,av')
            return (r,av')


compCost cam = do
    t <- newIORef 40
    return $ do
            av <- readIORef t
            t0 <- getCPUTime
            r <- cam
            t1 <- getCPUTime
            let delta = fromIntegral (t1 - t0) / (10^9 :: Double)
                av' = av *0.99 + 0.01* delta
            writeIORef t av'
            return (r,av')


timeMonitor = compCost >=> frameRate >=> monitor "Timing"  (Size 50 230) f >~> (fst.fst) where
    f ((_,t1),t2) = do
        pixelCoordinates (Size 50 230)
        text2D 15 30 $ printf " %3.0f ms CPU  / %4.0f Hz   /   %3.0f%%" (t1::Double) (1000/t2::Double) (100*t1/t2)


------------------------------------------------------------

conjugateRotation pan tilt rho foc sca =
        scaling sca
        <> kgen foc
        <> rot1 tilt
        <> rot2 pan 
        <> rot3 rho 
        <> kgen (1/foc)

warper sz name = do
    param <- createParameters   [ ("pan",  realParam (0) (-40) (40))
                                 ,("tilt", realParam (0) (-30) (30))
                                 ,("rho",  realParam  0 (-60) (60))
                                 ,("foc",  listParam 2.8 [0.5, 0.7, 1, 2, 2.6, 2.8, 5, 5.5, 9,10])
                                 ,("sca",  listParam 1 [1.1**k|k<-[-20..20]])]
    let h = do
            pan   <- getParam param "pan"
            tilt  <- getParam param "tilt"
            rho   <- getParam param "rho"
            foc   <- getParam param "foc"
            sca   <- getParam param "sca"
            let t = conjugateRotation (pan*degree) (tilt*degree) (rho*degree) foc sca
            return t
        f img = do
            t <- h
            return $ warp (0::CUChar) sz t img

    let drw w img = do
        inWin w $ do
            windowStatus $= Shown
            f img >>= drawImage

    w <- evWindow undefined name sz Nothing (const kbdQuit)
    windowStatus $= Hidden
    putW w (h,drw w)
    return w

----------------------------------------------------------------

-- A virtual camera which finds rectangles with a given aspect ratio

findRectangles segments ratio cam = do
    op <- createParameters [ ("radius",intParam 4 0 10),
                             ("width",realParam 1.5 0 5),
                             ("median",intParam 5 3 5),
                             ("high",intParam 40 0 255),
                             ("low",intParam 20 0 255),
                             ("postproc",intParam 1 0 1),
                             ("minlength",realParam 0.15 0 1),
                             ("maxdis",realParam 0.06 0 0.1),
                             ("orthotol",realParam 0.4 0.01 1.0)]
    let a4 = [[   0,            0]
            ,[   0, (2.10*ratio)]
            ,[2.10, (2.10*ratio)]
            ,[2.10,           0]]
    return $ do
        orig <- cam
        let img = gray orig
        radius <- getParam op "radius"
        width  <- getParam op "width"
        median <- getParam op "median"
        high   <- fromIntegral `fmap` (getParam op "high" :: IO Int)
        low    <- fromIntegral `fmap` (getParam op "low" :: IO Int)
        postp  <- getParam op "postproc" :: IO Int
        let pp = if postp == 0 then False else True
        minlen <- getParam op "minlength"
        maxdis <- getParam op "maxdis"
        orthotol  <- getParam op "orthotol"
        let
            alter pts = map (rotateList pts) [0 .. 3]
            mbf = Nothing
            segs = filter ((>minlen).segmentLength) $ (segments::SegmentExtractor) radius width median high low pp img
            polis = segmentsToPolylines maxdis segs
            closed4 = [p | Closed p <- polis, length p == 4]
            a4s = filter (isA4 mbf orthotol a4) (concatMap alter closed4)
        return (orig,a4s)


isA4 mbf tol a4 pts = ao < tol && cy < 0
    where mbomega = fmap omegaGen mbf
          ao = autoOrthogonality mbomega h
          h = estimateHomography (map pl pts) a4
          Just p = poseFromHomogZ0 mbf h
          (_,cy,_) = cameraCenter p
          omegaGen f = kgen (recip (f*f))

----------------------------------------------------------------

onlyRectangles segments sz ratio sel cam = do
    fr <- findRectangles segments ratio cam
    return $ do
        (orig,a4s) <- fr
        let f pts = fst . rectifyQuadrangle sz pts . sel $ orig
        return $ map f a4s

------------------------------------------------------------------

-- | convenience function
--rectifyQuadrangle :: Size -> [Point] -> ImageFloat -> (ImageFloat, Matrix Double)
rectifyQuadrangle sz pts imf = (r,h) where
    a4aux = [[-1,-r],[1,-r],[1,r],[-1,r]]
        where r = 1/ratio
              Size h w = sz
              ratio = fromIntegral w / fromIntegral h
    h = estimateHomography a4aux (map pl pts)
        where pl (Point x y) = [x,y]
    r = warp' sz h imf

-- initialization not required
-- FIXME
warp' s h im = unsafePerformIO $ do
    r <- image s
    warpOn' h r im
    return r

------------------------------------------------------------------------

-- A camera combinator which finds a given polygon and gives its pose
findPolygons :: SegmentExtractor -> Maybe Double -> [[Double]] -> IO (Channels) -> IO (IO(Channels,[([Point],CameraParameters)]))
findPolygons segments mbf ref cam = do
    op <- createParameters [ ("radius",intParam 4 0 10),
                             ("width",realParam 1.5 0 5),
                             ("median",intParam 5 3 5),
                             ("high",intParam 40 0 255),
                             ("low",intParam 20 0 255),
                             ("postproc",intParam 1 0 1),
                             ("minlength",realParam 0.15 0 1),
                             ("maxdis",realParam 0.06 0 0.1),
                             ("orthotol",realParam 0.4 0.01 1.0)]
    return $ do
        orig <- cam
        let img = gray orig
        radius <- getParam op "radius"
        width  <- getParam op "width"
        median <- getParam op "median"
        high   <- fromIntegral `fmap` (getParam op "high" :: IO Int)
        low    <- fromIntegral `fmap` (getParam op "low" :: IO Int)
        postp  <- getParam op "postproc" :: IO Int
        let pp = if postp == 0 then False else True
        minlen <- getParam op "minlength"
        maxdis <- getParam op "maxdis"
        orthotol  <- getParam op "orthotol"
        let oks = getPolygons' segments radius width median high low pp minlen maxdis orthotol mbf ref img
        return (orig,oks)

pl (Point x y) = [x,y]

rotateList list n = take (length list) $ drop n $ cycle list

-- (to do: normalize or pass as parameter costHomog value)
consistency mbf tol ref pts = (okpose && ao < tol && costHomog pts ref p < 1E-1 * perim, (pts,p)) -- && cy < 0
    where mbomega = fmap omegaGen mbf
          ao = autoOrthogonality mbomega h
          h = estimateHomography (map pl pts) ref
          pose = poseFromHomogZ0 mbf h
          okpose = isJust pose
          isJust Nothing = False
          isJust _       = True
          Just p = pose
          (_,cy,_) = cameraCenter p
          omegaGen f = kgen (recip (f*f))
          perim = perimeter (Closed pts)

costHomog view world c = pnorm PNorm1 $ flatten (fromLists (map pl view) -
                                        htm  (syntheticCamera c) (fromLists $ map (++[0]) world))

-- the essential function used by findPolygons. TO DO: move to a more appropriate module
getPolygons :: SegmentExtractor -> Maybe Double -> [[Double]] -> ImageGray -> [([Point],CameraParameters)]
getPolygons segments = getPolygons' segments 4 1.5 5 40 20 True 0.05 0.06 0.4

getPolygons' segments radius width median high low pp minlen maxdis orthotol mbf ref img = oks where
    l = length ref
    alter pts = map (rotateList pts) [0 .. l-1]
    segs = filter ((>minlen).segmentLength) $ segments radius width median high low pp img
    polis = segmentsToPolylines maxdis segs
    candis = [p | Closed p <- polis, length p == l]
    poses = map (consistency mbf orthotol ref) (concatMap alter candis)
    oks = map snd (filter fst poses)


polyconsis mbf orthotol ref polys = oks
    where poses = map (consistency mbf orthotol ref) (concatMap alter candis)
          oks = map snd (filter fst poses)
          alter pts = map (rotateList pts) [0 .. l-1]
          l = length ref
          candis = filter ((==l).length) polys

