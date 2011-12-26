-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.Combinators
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Camera combinators: higher order functions which make virtual cameras from other (virtual or not) cameras.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Combinators (
  camera, cameraFolderG,
  run, runFPS,
  monitor,
  monitorWheel,
  monitor3D,
  monitorScanLine, 
  gray,
  counter, countDown,
  frameRate, compCost, timeMonitor,
  selectROI, selectROIfun,
  selectSnd,
  updateMaybe, updateMaybeSave,
  clickStatusWindow,
  warpTo,
  maskDir, maskPoly,
  polyEllipseMoments
)where

import ImagProc.Ipp.Core
import ImagProc
import ImagProc.Util
import Data.IORef
import System.Exit
import EasyVision.GUI
import Graphics.UI.GLUT hiding (Size,Point,ctrl)
import Control.Monad(when,(>=>))
import ImagProc.Camera(mpSize)
import System.CPUTime
import System.Time
import Text.Printf
import Util.Options(optionString,hasValue,optionFromFile,getFlag)
import Data.Maybe(isJust)
import Data.List(foldl1')
import Control.Arrow((***))
import Util.Misc(Mat)
import Contours.Base
import Contours.Normalization(eig2x2Dir)
import Numeric.LinearAlgebra(inv,single,toList)
import Vision(pt2hv,cross)
import Util.Ellipses

-- deprecated
gray = grayscale

----------------------------------------------------------------------

cameraFolder = do
    path <- optionString "--photos" "."
    sz <- findSize
    imgs <- readFolder path (Just sz)
    let disp rk = do
           k <- get rk  
           drawImage (imgs!!k)
    w <- evWindow 0 ("Folder: "++path) (mpSize 10) (Just disp) (mouseGen (cfacts (length imgs -1)) kbdQuit)               
    return $ do
        k <- getW w    
        return (channels $ imgs!!k)

cameraFolderG = do
    path <- optionString "--photos" "."
    imgs <- readFolder' path
    let disp rk = do
           k <- get rk  
           drawImage'' 320 (fst $ imgs!!k)
    w <- evWindow 0 ("Folder: "++path) (mpSize 10) (Just disp) (mouseGen (cfacts (length imgs -1)) kbdQuit)               
    return $ do
        k <- getW w    
        return (channelsFromRGB $ fst $ imgs!!k)


cfacts n = [((MouseButton WheelUp,   Down, modif), \_ k -> min (k+1) n)
           ,((SpecialKey  KeyUp,     Down, modif), \_ k -> min (k+1) n)
           ,((MouseButton WheelDown, Down, modif), \_ k -> max (k-1) 0)
           ,((SpecialKey  KeyDown,   Down, modif), \_ k -> max (k-1) 0)]


cameraV = findSize >>= getCam 0 ~> channels

-- | returns the camera 0. (It also admits --photos=path/to/folder/ with images, and --variable-size, to read images of
-- different arbitrary sizes.)
camera :: IO (IO Channels)
camera = do
    f <- hasValue "--photos"
    g <- getFlag "--variable-size"
    if f then if g then cameraFolderG else cameraFolder
         else cameraV

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

-- | This is a "monitor" for 3D objects. The viewpoint can be changed with the mouse (drag, wheel, O).
monitor3D :: String     -- ^ window name
          -> Int        -- ^ size of square window
          -> (a->IO ()) -- ^ monitor function
          -> (IO a)     -- ^ original camera
          -> IO (IO a)  -- ^ new camera
monitor3D name sz fun cam = do
    (cam', ctrl) <- withPause cam
    w <- evWindow3D () name sz (const $ kbdQuit)
    clearColor $= Color4 1 1 1 1
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

run :: IO (IO a) -> IO ()
run = runIdle

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


timeMonitor :: IO b -> IO (IO b)
timeMonitor = compCost >=> frameRate >=> monitor "Timing"  (Size 50 230) f >~> (fst.fst) where
    f ((_,t1),t2) = do
        pixelCoordinates (Size 50 230)
        text2D 15 30 $ printf " %3.0f ms CPU  / %4.0f Hz   /   %3.0f%%" (t1::Double) (1000/t2::Double) (100*t1/t2)


----------------------------------------------------------------------

selectROI :: (Image b, Drawable b) => String -> (a -> b) -> IO a -> IO (IO (a, ROI))
selectROI name f = selectROIfun name f (\_ _ -> return ()) (,)


selectROIfun :: (Image b, Drawable b) => String -> (a -> b) -> (a -> ROI -> IO()) -> (a -> ROI -> c)-> IO a -> IO (IO c)
selectROIfun name sel mon result cam = do
    (cam', ctrl) <- withPause cam
    let sz = mpSize 20
    w <- evWindow () name sz Nothing (const (kbdcam ctrl))
    let d = 50
    evROI w $= ROI d (height sz-d) d (width sz-d)
    return $ do
        x <- cam'
        r <- getROI w
        inWin w $ do drawImage'' 640 (sel x)
                     sz <- evSize `fmap` get windowSize
                     pixelCoordinates sz
                     setColor 1 0 0
                     drawROI r
                     mon x r
        return (result x r)

----------------------------------------------------------------------

-- | forwards the snd part of the tuple if the user clicks
selectSnd :: String -> ((a, b) -> IO ()) -> IO (a, b) -> IO (IO (a, Maybe b))
selectSnd name f cam = do
    w <- evWindow False ("Click to select: "++name) (mpSize 10) Nothing (mouseGen acts kbdQuit)
    return $ do
        (x,y) <- cam
        inWin w $ f (x,y)
        s <- getW w
        let y' = if s then Just y else Nothing
        putW w False
        return (x, y')
  where acts = [((MouseButton LeftButton,   Down, modif), \ _ _ -> True)]

----------------------------------------------------------------------

-- | this is like a fold, using the Just values to update
updateMaybe :: String        -- ^ window name
            -> (a -> b -> b) -- ^ update function
            -> b             -- ^ initial value
            -> ((x,b) -> IO ()) -- ^ display operation
            -> IO (x, Maybe a)  -- ^ input process
            -> IO(IO(x,b))      -- ^ output
updateMaybe name f b0 disp cam = do
    w <- evWindow b0 name (mpSize 10) Nothing (const kbdQuit)
    return $ do
        (x,ma) <- cam
        b <- getW w
        let b' = case ma of
                    Just a  -> f a b
                    Nothing -> b
        putW w b'
        inWin w $ disp (x,b')
        return (x,b')


-- | the same as updateMaybe, but the updated values are saved to a file 
--   and can be loaded with a command line option.
updateMaybeSave
    :: (Read b, Show b)
    => String        -- ^ window name
    -> String        -- ^ load option name (without --)
    -> (a -> b -> b) -- ^ update function
    -> b             -- ^ initial value
    -> ((x,b) -> IO ()) -- ^ display operation
    -> IO (x, Maybe a)  -- ^ input process
    -> IO(IO(x,b))      -- ^ output
updateMaybeSave name option f b0 disp cam = do
    b0' <- optionFromFile ("--"++option) b0
    w <- evWindow b0' name (mpSize 10) Nothing (const kbdQuit)
    return $ do
        (x,ma) <- cam
        b <- getW w
        let b' = case ma of
                    Just a  -> f a b
                    Nothing -> b
        putW w b'
        inWin w $ disp (x,b')
        when (isJust ma) $ writeFile ("saved-"++option) (show b')
        return (x,b')

----------------------------------------------------------------------

-- | windows with a state which is updated when the user clicks on it
clickStatusWindow
    :: String            -- ^ window name
    -> Size              -- ^ initial size
    -> s                 -- ^ initial state
    -> (x -> s -> s)     -- ^ update on click
    -> (x -> s -> IO ()) -- ^ display function (always)
    -> (x -> s -> IO ()) -- ^ action (only when the state is updated)
    -> IO x              -- ^ input
    -> IO (IO (x,s))     -- ^ result
clickStatusWindow name sz s0 update display act cam = do
    w <- evWindow (False,s0) name sz Nothing (mouse kbdQuit)
    return $ do
        x <- cam
        (click, s) <- getW w
        let s' = if click then update x s else s
        inWin w (display x s')
        putW w (False, s')
        when click (act x s')
        return (x,s')
  where
    mouse _ st (MouseButton LeftButton) Down _ _ = do
        updateW st (not *** id)
    mouse m _ a b c d = m a b c d

----------------------------------------------------------------------

-- | This is a monitor with a display function which depends on an Int value
-- which can be changed with the mouse wheel
monitorWheel
    :: (Int,Int,Int)       -- ^ initial value and range of display parameters
    -> String              -- ^ window name
    -> Size                -- ^ window size
    -> (Int -> a -> IO ()) -- ^ monitor function
    -> (IO a)              -- ^ original camera
    -> IO (IO a)           -- ^ new camera
monitorWheel (k0,k1,k2) name sz fun cam = do
    (cam', ctrl) <- withPause cam
    w <- evWindow k0 name sz Nothing (mouse (kbdcam ctrl))
    return $ do
        thing <- cam'
        k <- getW w
        inWin w (fun k thing)
        return thing
  where
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        updateW st (min k2 . (+1))
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        updateW st (max k1 . (subtract 1))
    mouse _ st (SpecialKey KeyUp) Down _ _ = do
        updateW st (min k2 . (+1))
    mouse _ st (SpecialKey KeyDown) Down _ _ = do
        updateW st (max k1 . (subtract 1))
    mouse m _ a b c d = m a b c d

--------------------------------------------------------------------------------

-- temporary location, and
-- to be replaced by a new warpBack function
warpTo :: GImg pix img => ROI -> Mat -> img -> img
warpTo droi h img = warp zeroP sz h x
  where
    sz = size img
    sroi = transformROI sz (inv h) droi
    x = modifyROI (const sroi) img

transformROI :: Size -> Mat -> ROI -> ROI
transformROI sz h = shrink (-1,-1) . poly2roi sz . transPol h . roi2poly sz

----------------------------------------------------------------------

maskDir (xI,yI) (Segment p0 p1) = binarize32f (-c) (a .* xI |+| b .* yI)
  where
    [a,b,c] = toList $ single $ cross (pt2hv p0) (pt2hv p1)

maskPoly :: (ImageFloat,ImageFloat) -> Polyline -> ImageGray
maskPoly (xI,yI) p = foldl1' (andI) (map (toGray . maskDir b) ss)
  where
    sz = size xI
    ss = asSegments p
    roi = poly2roi sz p
    b = (setROI roi xI, setROI roi yI)

polyEllipseMoments :: (Double,Double,Double,Double,Double) -> Int -> Polyline
polyEllipseMoments (mx,my,cxx,cyy,cxy) n = Closed . conicPoints 30 $ e
  where
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    e = InfoEllipse {
            conicCenter = (mx,my), 
            conicSizes = (2*sqrt l1, 2*sqrt l2), 
            conicAngle = a,
            conicMatrix = undefined, 
            conicTrans = undefined }


----------------------------------------------------------------------

monitorScanLine :: String -> (x -> ImageGray) -> IO x -> IO (IO x)
monitorScanLine name sel = monitorWheel (240,0,479) name (mpSize 20) sh
  where
    sh k x = do
      let im = sel x
          ROI r1 r2 c1 c2 = theROI im
          vs = zipWith (flip Pixel) [c1..] $ map (\x->round (480-479/255 * fromIntegral x::Float)) $ sampleLine8u im (Pixel k c1) (Pixel k c2)
      drawImage im
      setColor 1 0.8 0.8
      renderPrimitive LineStrip (mapM_ vertex [Pixel 240 0,Pixel 240 639])
      setColor 1 1 0
      renderPrimitive LineStrip (mapM_ vertex [Pixel k c1,Pixel k c2])
      setColor 1 0 0
      when (r1<=k && k <= r2) $ renderPrimitive LineStrip (mapM_ vertex vs)

