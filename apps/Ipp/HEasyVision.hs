-----------------------------------------------------------------------------
{- |
Module      :  Ipp.HEasyVision
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

A minimalist version of EasyVision [ref.] using HOpenGL.

See the simple examples: hmplayer.hs, hessian.hs, warp.hs, etc. in the easyvision folder.

-}
-----------------------------------------------------------------------------

module Ipp.HEasyVision (
-- * Application interface
  State(..)
, prepare
, addWindow
, launch
, InWindow
, kbdcam, roiControl
-- * Drawing utilities
, module Ipp.Draw
-- * DV camera driver
, module Ipp.Camera
) where

import Ipp.Camera
import Ipp.Draw
import Ipp.Core -- (Size(..))
import Ipp.Camera
import Graphics.UI.GLUT hiding (RGB, Matrix, Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import qualified Data.Map as Map
import Data.Map

--import qualified Data.Map as Map
--import Data.Map((!))

-- | creates an HOpenGL window which can be selected inside the worker function.
addWindow :: String -- ^ window identifier
             -> Size -- ^ size
             -> Maybe (State userState -> IO t)                    -- ^ optional drawing callback
             -> (IORef (State userState) -> KeyboardMouseCallback) -- ^ keyboard callback receiving the user defined state
             -> IORef (State userState)                            -- ^ the user defined state
             -> IO Window
addWindow name s dcallback kcallback state = do
    w <- installWindow name s dcallback kcallback state
    modifyIORef state $ \s -> s { wins = insert name w (wins s)}
    return w

-- | The state of the application, including the user defined state.
data State userState = 
    State { wins  :: Map String Window
          , camid :: Camera
          , frame :: Int 
          , ust  :: userState
}

-- | Initializes the application state with a camera and a user-defined state.
prepare :: Camera
           -> userState
           -> IO (IORef (State userState))
prepare cam s = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]

    state <- newIORef State {camid = cam, frame = 0,
                             wins = empty, ust = s}
    return state

-- | Window selector for the HOpenGL functions
type InWindow = String -> IO () -> IO ()

-- | Starts the application with a worker function (idle callback).
launch :: IORef (State userState)      -- ^ the state of the application
          -> (InWindow -> Camera -> userState -> IO userState)  -- ^ worker function
          -> IO ()
launch state worker = do
    idleCallback $= Just ( do
        st <- readIORef state
        newstate <- worker (inWindow st) (camid st) (ust st)
        writeIORef state st {frame = frame st +1, ust = newstate}
        )
    mainLoop

inWindow st name f = do
    let w = wins st ! name
    currentWindow $= Just w
    f
    swapBuffers

installWindow name (Size hei wid) (Just fun) kbdcallback state = do
    w <- createWindow name
    windowSize $= GL.Size (fromIntegral wid) (fromIntegral hei)
    displayCallback $= draw
    keyboardMouseCallback $= Just (kbdcallback state)
    return w
  where
    draw = do
        clear [ColorBuffer]
        st <- readIORef state
        fun st
        swapBuffers

installWindow name (Size hei wid) Nothing kbdcallback state = do
    w <- createWindow name
    displayCallback $= return ()
    windowSize $= GL.Size (fromIntegral wid) (fromIntegral hei)
    keyboardMouseCallback $= Just (kbdcallback state)
    return w

-- | keyboard callback for camera control and exiting the application with ESC.
kbdcam :: (String->IO ()) -> KeyboardMouseCallback
kbdcam ctrl = kbd where
    kbd (Char 'p') Down _ _ = ctrl "pause"
    kbd (Char ' ') Down _ _ = ctrl "pause"
    kbd (Char 's') Down _ _ = ctrl "step"
    kbd (Char 'q') Down _ _ = exitWith ExitSuccess
    kbd (Char '\27') Down _ _ = exitWith ExitSuccess
    kbd _ _ _ _ = return ()

-- | Installs mouse support for interactive selection of a 'ROI' (region of interest) in a window. The ROI is selected with the right button. The initial ROI can be recovered with the key R.
roiControl :: ROI                    -- ^ initial roi
           -> KeyboardMouseCallback  -- ^ keyboard callback for the window
           -> IO (IO ROI)            -- ^ function to retrieve the current roi
roiControl initROI defaultFunc = do
    r <- newIORef initROI
    keyboardMouseCallback $= Just (kbd r defaultFunc)
    motionCallback $= Just (mv r)
    return (readIORef r)
        where
            d = 20
            kbd r _ (MouseButton RightButton) Down _ pos@(Position x y) =
                modifyIORef r (\ (ROI _ r2 _ c2) ->
                                  ROI (min (r2-d) (fromIntegral y)) r2
                                      (min (c2-d) (fromIntegral x)) c2)
            kbd r _ (Char 'r') Down _ _ = writeIORef r initROI
            kbd _ defaultFunc a b c d = defaultFunc a b c d
            mv r (Position x y) =
                modifyIORef r (\ (ROI r1 _ c1 _) ->
                                  ROI r1 (max (r1+d) (fromIntegral y))
                                      c1 (max (c1+d) (fromIntegral x)))
