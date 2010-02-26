-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.GUI.GUI
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

The GUI of EasyVision, based on HOpenGL.


-}
-----------------------------------------------------------------------------

module EasyVision.GUI.GUI (
-- * Application interface
  State(..)
, prepare, prepare'
, addWindow, evWindow, evWindow3D, EVWindow(..)
, launch, launch', launchFreq
, InWindow, inWin, getW, putW, getROI
, kbdcam, kbdQuit, mouseGen, modif
-- * Drawing utilities
, module EasyVision.GUI.Draw
, module EasyVision.GUI.Objects
) where

import EasyVision.GUI.Draw
import ImagProc.Base
import ImagProc.Ipp(Size(..),ippSetNumThreads,ROI(..),saveRGB)
import Graphics.UI.GLUT hiding (RGB, Matrix, Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System(system)
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import qualified Data.Map as Map
import Data.Map
import EasyVision.GUI.Objects

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
    kbd (Char ' ') Down Modifiers {shift=Up} _ = ctrl "pause"
    kbd (Char ' ') Down Modifiers {shift=Down} _ = ctrl "pass"
    kbd (Char 's') Down _ _ = ctrl "step"
    kbd a b c d = kbdQuit a b c d

-- | keyboard callback for exiting the application with ESC or q, useful as default callback.
-- Also, pressing i saves a screenshot of the full opengl window contents.
kbdQuit :: KeyboardMouseCallback
kbdQuit (Char '\27') Down Modifiers {shift=Down} _ = leaveMainLoop >> system "killall mplayer" >> return ()
kbdQuit (Char '\27') Down _ _ = leaveMainLoop
kbdQuit (Char   'i') Down _ _ = captureGL >>= saveRGB Nothing
kbdQuit _ _ _ _               = return ()

mouseGen acts def st a b c d = do
    v <- get st
    case Prelude.lookup (a,b,c) acts of
        Just op -> st $= op v >> postRedisplay Nothing
        Nothing -> def a b c d

modif = Modifiers {ctrl = Up, shift = Up, alt = Up }

-----------------------------------------------------------------
-- simpler interface, with state in each window

-- | Initializes the HOpenGL system.
prepare :: IO ()
prepare = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    ippSetNumThreads 1
    return ()

-- | Starts the application with a worker function (idle callback).
launch :: IO () -> IO ()
launch worker = do
    idleCallback $= Just worker
    mainLoop

-- | Starts the application with a worker function which runs at the desired frequency (in Hz).
launchFreq :: Int -> IO () -> IO ()
launchFreq freq worker = do
    let callback = do
        addTimerCallback (1000 `div` freq) callback
        worker
    addTimerCallback 1000 callback
    mainLoop


----------------------------------------------------------------

data EVWindow st = EVW { evW   :: Window
                       , evSt   :: IORef st
                       , evROI :: IORef ROI
                       , evInit :: IO ()
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

    actionOnWindowClose $= MainLoopReturns

    let Size h w = size
        initROI = ROI {r1=0, r2=h-1, c1=0, c2=w-1}
    r <- newIORef initROI

    keyboardMouseCallback $= Just (kbdroi r initROI (kbd st))
    motionCallback $= Just (mvroi r)

    return EVW { evW = glw
               , evSt = st
               , evROI = r
               , evInit = clear [ColorBuffer]
               }

---------------------------------------------------------------

inWin w f = do
    saved <- get currentWindow
    currentWindow $= Just (evW w)
    evInit w
    f
    swapBuffers
    currentWindow $= saved

getW = get . evSt
putW w x = evSt w $= x

getROI = get . evROI

----------------------------------------------------------------

evWindow3D ist name sz kbd = do
    (trackball,kc,mc) <- newTrackball
    w <- evWindow ist name (Size sz sz) Nothing (kc kbd)
    motionCallback $= Just mc
    depthFunc $= Just Less
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Replace
    return w { evInit = clear [ColorBuffer, DepthBuffer] >> trackball}

----------------------------------------------------------------

minroi = 20

kbdroi r _ _ (MouseButton RightButton) Down Modifiers {shift=Up} pos@(Position x y) =
    modifyIORef r (\ (ROI _ r2 _ c2) ->
                        ROI (min (r2-minroi) (fromIntegral y)) r2
                            (min (c2-minroi) (fromIntegral x)) c2)
kbdroi r _ _ (MouseButton RightButton) Down Modifiers {shift=Down} pos@(Position x y) =
    modifyIORef r (\ (ROI r1 _ c1 _) ->
                        ROI r1 (max (r1+minroi) (fromIntegral y))
                            c1 (max (c1+minroi) (fromIntegral x)))
kbdroi r initroi _ (Char 'r') Down _ _ = writeIORef r initroi
kbdroi _ _ defaultFunc a b c d = defaultFunc a b c d

mvroi r (Position x y) =
    modifyIORef r (\ (ROI r1 _ c1 _) ->
                        ROI r1 (max (r1+minroi) (fromIntegral y))
                            c1 (max (c1+minroi) (fromIntegral x)))
