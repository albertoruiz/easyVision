-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.GUI.OldGUI
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Deprecated functions

-}
-----------------------------------------------------------------------------

module EasyVision.GUI.OldGUI (
  State(..)
, prepare'
, addWindow
, launch'
, InWindow
) where

import EasyVision.GUI.Util
import EasyVision.GUI.Draw
import ImagProc.Base
import ImagProc.Ipp(Size(..),ippSetNumThreads,ROI(..),saveRGB')
import Graphics.UI.GLUT hiding (RGB, Matrix, Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Process(system)
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

