{-# OPTIONS -fffi -fglasgow-exts #-}

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

module EasyVision.Combinators (
  -- * Virtual camera generator
  virtualCamera,
  -- * Camera combinators
  -- | A few useful combinators
  withPause,
  addSmall,
  detectMov,
  warper,
  panoramic,
  monitorizeIn,
  inThread
)where

import ImagProc.Ipp.Core
import ImagProc.ImageProcessing
import Foreign
import Foreign.C.Types (CChar,CUChar)
import Foreign.C.String(newCString)
import Data.IORef
import System.IO
import System
import System.IO.Unsafe(unsafeInterleaveIO)
import EasyVision.GUI
import EasyVision.Draw
import EasyVision.Parameters
import Graphics.UI.GLUT hiding (Size)
import Control.Concurrent
import Vision
import Numeric.LinearAlgebra



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

    let control command = do
        case command of
         "pause" -> do modifyIORef paused not
                       p <- readIORef paused
                       if p then grab >>= writeIORef frozen
                            else return ()
         "step"   -> do modifyIORef step not

    let virtual = do
        s <- readIORef step
        p <- readIORef paused
        if not s && p
             then readIORef frozen
             else 
                if s then if p then readIORef frozen
                               else do writeIORef paused True
                                       grab >>= writeIORef frozen
                                       readIORef frozen
             else grab

    return (virtual,control)

-----------------------------------------------

createGrab l = do
    pl <- newIORef l
    return $ do
        h:t <- readIORef pl
        writeIORef pl t
        return h

grabAll grab = do
    im <- grab
    rest <- unsafeInterleaveIO (grabAll grab)
    return (im:rest)

-- | Creates a virtual camera by some desired processing of the infinite list of images produced by another camera.
virtualCamera :: ([a]-> IO [b]) -> IO a -> IO (IO b)
virtualCamera filt grab = grabAll grab >>= filt >>= createGrab


--------------------------------------------------------------------------

-- | Movement detector with a desired condition on the absolute pixel difference in the roi of two consecutive images (computed with help of 'addSmall').
detectMov :: (Double -> Bool) -> IO (ImageYUV, ImageGray) -> IO (IO (ImageYUV, ImageGray))
detectMov cond = virtualCamera (return . detectMov' cond)

detectMov' cond ((a,f):(b,g):t) =
    if cond (absdif f g)
        then (a,f) : detectMov' cond ((b,g):t)
        else detectMov' cond ((b,g):t)

absdif a b = unsafePerformIO $ absDiff8u a b >>= sum8u

-- | Given a yuv camera adds a gray version (typically small), to be used with other combinators like 'detectMov'
addSmall :: Size -> IO ImageYUV -> IO (IO (ImageYUV, ImageGray))
addSmall sz grab = return $ do
    im <- grab
    f <- yuvToGray im >>= resize8u sz
    return (im,f)

---------------------------------------------------------

-- | The grabbed image (extracted from a general structure by the selector) is automatically shown in a window in each grab. The window provides a simple camera control with the keyboard. See the example interpolate.hs.
monitorizeIn :: Drawable im => String    -- ^ window name
                            -> Size      -- ^ window size
                            -> (a->im)   -- ^ selector
                            -> (IO a)    -- ^ original camera
                            -> IO (IO a) -- ^ new camera
monitorizeIn name sz selector cam = do
    (cam', ctrl) <- withPause cam
    w <- evWindow () name sz Nothing (const $ kbdcam ctrl)
    return $ do
        thing <- cam'
        saved <- get currentWindow  -- required, since it may happen inside an inWindow bracket
        currentWindow $= Just (evW w)
        drawImage (selector thing)
        swapBuffers
        currentWindow $= saved
        return thing

---------------------------------------------------------

-- | Creates a virtual camera which always supplies the most recent frame.
inThread :: IO a -> IO (IO a)
inThread cam = do
    c <- newEmptyMVar
    cam >>= putMVar c
    let loop = do img <- cam
                  swapMVar c img
                  loop
    forkIO loop
    return (readMVar c)

-----------------------------------------------------------

-- | Creates a panoramic view from two cameras with (nearly) common camera center. Currently the synthetic rotations are set manually, but soon...
panoramic :: Size -> IO ImageFloat -> IO ImageFloat -> IO (IO ImageFloat)
panoramic sz cam1 cam2 = do
    wr1 <- warper sz "warper1"
    wr2 <- warper sz "warper2"
    return $ do
        orig1 <- cam1
        floor <- image sz
        set32f 0 (theROI floor) floor
        (rh1,_) <- getW wr1
        h1 <- rh1
        warpOn h1 floor orig1
        orig2 <- cam2
        (rh2,_) <- getW wr2
        h2 <- rh2
        warpOn h2 floor orig2
        return floor

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
                                 ,("sca",  listParam 0.5 [1.1**k|k<-[-20..20]])]
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
            warp sz t img

    let drw w img = do
        inWin w $ do
            windowStatus $= Shown
            f img >>= drawImage

    w <- evWindow undefined name sz Nothing (const kbdQuit)
    windowStatus $= Hidden
    putW w (h,drw w)
    return w


