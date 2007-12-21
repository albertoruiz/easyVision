-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.GUI
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

The GUI of EasyVision, based on HOpenGL.


-}
-----------------------------------------------------------------------------

module EasyVision.GUI (
-- * Application interface
  State(..)
, prepare, prepare'
, addWindow, evWindow, EVWindow(..)
, launch, launch'
, InWindow, inWin, getW, putW, getROI
, kbdcam, kbdQuit, roiControl
-- * Drawing utilities
, module EasyVision.Draw
) where

import EasyVision.Draw
import EasyVision.Util
import ImagProc.Ipp.Core -- (Size(..))
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
          , frame :: Int 
          , ust  :: userState
}

prepare' :: userState
        -> IO (IORef (State userState))
prepare' s = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]

    state <- newIORef State {frame = 0,
                             wins = empty, 
                             ust = s}
    return state

-- | Window selector for the HOpenGL functions
type InWindow = String -> IO () -> IO ()

launch' :: IORef (State userState)      -- ^ the state of the application
          -> (InWindow -> userState -> IO userState)  -- ^ worker function
          -> IO ()
launch' state worker = do
    idleCallback $= Just ( do
        st <- readIORef state
        newstate <- worker (inWindow st) (ust st)
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

-- | keyboard callback for camera control and exiting the application with ESC. p or SPACE pauses, s sets frame by frame mode.
kbdcam :: (String->IO ()) -> KeyboardMouseCallback
kbdcam ctrl = kbd where
    kbd (Char 'p') Down _ _ = ctrl "pause"
    kbd (Char ' ') Down _ _ = ctrl "pause"
    kbd (Char 's') Down _ _ = ctrl "step"
    kbd a b c d = kbdQuit a b c d

-- | keyboard callback for exiting the application with ESC or q, useful as default callback.
-- Also, pressing i saves a screenshot of the full opengl window contents.
kbdQuit :: KeyboardMouseCallback
kbdQuit (Char   'q') Down _ _ = exitWith ExitSuccess
kbdQuit (Char '\27') Down _ _ = exitWith ExitSuccess
kbdQuit (Char   'i') Down _ _ = captureGL >>= saveRGB Nothing
kbdQuit _ _ _ _               = return ()

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


-----------------------------------------------------------------
-- simpler interface, with state in each window

-- | Initializes the HOpenGL system.
prepare :: IO ()
prepare = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]

-- | Starts the application with a worker function (idle callback).
launch :: IO () -> IO ()
launch worker = do
    idleCallback $= Just worker
    mainLoop

----------------------------------------------------------------

data EVWindow st = EVW { evW   :: Window
                       , evSt   :: IORef st
                       , evROI :: IORef ROI
                       }

evWindow st0 name size mdisp kbd = do
    st <- newIORef st0
    glw <- createWindow name
    windowSize $= glSize size
    let draw = case mdisp of
            Nothing -> return ()
            Just fun -> do
                clear [ColorBuffer]
                fun st
                swapBuffers
    displayCallback $= draw

    let Size h w = size
        initROI = ROI {r1=0, r2=h-1, c1=0, c2=w-1}
    r <- newIORef initROI

    keyboardMouseCallback $= Just (kbdroi r initROI (kbd st))
    motionCallback $= Just (mvroi r)

    return EVW { evW = glw
               , evSt = st
               , evROI = r
               }

---------------------------------------------------------------

inWin w f = do
    saved <- get currentWindow
    currentWindow $= Just (evW w)
    f
    swapBuffers
    currentWindow $= saved

getW = get . evSt
putW w x = evSt w $= x

getROI = get . evROI

----------------------------------------------------------------

minroi = 20

kbdroi r _ _ (MouseButton RightButton) Down _ pos@(Position x y) =
    modifyIORef r (\ (ROI _ r2 _ c2) ->
                        ROI (min (r2-minroi) (fromIntegral y)) r2
                            (min (c2-minroi) (fromIntegral x)) c2)
kbdroi r initroi _ (Char 'r') Down _ _ = writeIORef r initroi
kbdroi _ _ defaultFunc a b c d = defaultFunc a b c d

mvroi r (Position x y) =
    modifyIORef r (\ (ROI r1 _ c1 _) ->
                        ROI r1 (max (r1+minroi) (fromIntegral y))
                            c1 (max (c1+minroi) (fromIntegral x)))

