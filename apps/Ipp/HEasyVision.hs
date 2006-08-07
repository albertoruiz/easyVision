-----------------------------------------------------------------------------
{- |
Module      :  Ipp.HEasyVision
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

A minimalist version of EasyVision [ref.] using HOpenGL.

See the simple examples: player.hs, hessian.hs, warp.hs, etc. in the easyvision folder.

-}
-----------------------------------------------------------------------------

module Ipp.HEasyVision (
  State(..)
, prepare
, addWindow
, launch
, InWindow
) where

import Ipp.Core
import Ipp.Typical
import Ipp.Draw
import Ipp.Camera
import Graphics.UI.GLUT hiding (RGB, Matrix)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import Data.Map

-- | creates an HOpenGL window which can be selected inside the worker function.
addWindow :: String -- ^ window identifier
             -> (Int, Int)  -- ^ size (width,height)
             -> Maybe (State userState -> IO t)                    -- ^ optional drawing callback
             -> (IORef (State userState) -> KeyboardMouseCallback) -- ^ keyboard callback receiving the user defined state
             -> IORef (State userState)                            -- ^ the user defined state
             -> IO Window
addWindow name (w,h) dcallback kcallback state = do
    w <- installWindow name (w,h) dcallback kcallback state
    modifyIORef state $ \s -> s { wins = insert name w (wins s)}
    return w

-- | The state of the application, including the user defined state.
data State userState = 
    State { wins  :: Map String Window
          , camid :: Camera
          , frame :: Int 
          , pause :: Bool
          , camera :: Img
          , ust  :: userState
}

-- | Initializes the application state with a camera and a user-defined state.
prepare :: Camera
           -> userState
           -> IO (IORef (State userState))
prepare cam s = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]

    state <- newIORef State {camid = cam, frame = 0, pause = False,
                             camera = undefined, wins = empty, ust = s}
    return state

-- | Window selector for the HOpenGL functions
type InWindow = String -> IO () -> IO ()

-- | Starts the application with a worker function (idle callback).
launch :: IORef (State userState)      -- ^ the state of the application
          -> (InWindow -> Img -> userState -> IO userState)  -- ^ worker function
          -> IO ()
launch state worker = do
    idleCallback $= Just ( do
        st <- readIORef state
        when (not (pause st)) $ do
            im <- grab (camid st)
            writeIORef state (st {camera = im})
        st <- readIORef state    
        newstate <- worker (inWindow st) (camera st) (ust st)
        writeIORef state st {frame = frame st +1, ust = newstate}
        )
    mainLoop

inWindow st name f = do
    let w = wins st ! name
    currentWindow $= Just w
    f
    swapBuffers

installWindow name (wid,hei) (Just fun) kbdcallback state = do
    w <- createWindow name
    windowSize $= Size (fromIntegral wid) (fromIntegral hei)
    displayCallback $= draw
    keyboardMouseCallback $= Just (kbdcallback state)
    return w
  where
    draw = do
        clear [ColorBuffer]
        st <- readIORef state
        fun st
        swapBuffers  

installWindow name (wid,hei) Nothing kbdcallback state = do
    w <- createWindow name
    displayCallback $= return ()
    windowSize $= Size (fromIntegral wid) (fromIntegral hei)
    keyboardMouseCallback $= Just (kbdcallback state)
    return w
