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
  monitorizeIn

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
import EasyVision.GUI(addWindow,kbdcam,State)
import EasyVision.Draw(drawImage,Drawable)
import Graphics.UI.GLUT(currentWindow,($=),swapBuffers,get)



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
                            -> (IORef (State u)) -- ^ application state
                            -> (IO a)    -- ^ original camera
                            -> IO (IO a) -- ^ new camera
monitorizeIn name sz selector app cam = do
    (cam', ctrl) <- withPause cam
    w <- addWindow name sz Nothing (const $ kbdcam ctrl) app
    return $ do
        thing <- cam'
        saved <- get currentWindow  -- required, since it may happen inside an inWindow bracket
        currentWindow $= Just w
        drawImage (selector thing)
        swapBuffers
        currentWindow $= saved
        return thing
