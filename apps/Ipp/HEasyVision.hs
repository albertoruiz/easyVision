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
-- * Application interface
  State(..)
, prepare
, addWindow
, launch
, InWindow
, kbdcam
, createParameters
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
    kbd (Char '\27') Down _ _ = exitWith ExitSuccess
    kbd _ _ _ _ = return ()


sizePar = 35

createParameters ops st = do
    o <- newIORef (Map.fromList ops)
    w <- addWindow "Parameters" (Size (2+length ops * sizePar) 200)
                                (Just (f o))
                                (const $ kbdopts o k)
                                st
    return (giveme o)
 where k _ _ _ _ = return ()
       f o s = do
           m <- readIORef o
           let els = Prelude.map fst $ Map.elems m
           pixelCoordinates (Size (2+length els * sizePar) 200)

           sequence_ $ zipWith3 bar [0..] els (keys m) 
           return () 
       bar p k s = do
           setColor 0 0 0.5
           renderPrimitive Polygon (mapM_ vertex [Pixel r1 c1,
                                                         Pixel r1 c2,
                                                         Pixel r2 c2,
                                                         Pixel r2 c1])
           setColor 1 1 1
           rasterPos (Vertex2 (5::GLfloat) (4+fromIntegral r1/2+fromIntegral r2/2))
           renderString Helvetica12 (s++" = "++show k++"%")

        where   r1 = 2+p*sizePar
                r2 = 2+p*sizePar+(sizePar -2)
                c1 = 1
                c2 = 2*k

kbdopts opts def = kbd where
    kbd (MouseButton WheelUp) _ _ pos@(Position x y) = do
        m <- readIORef opts
        let s' = keys m
        let s = (s' !! (fromIntegral y `div` sizePar))
        let (v,d) = m!s 
        let m' = insert s (min 100 (v+d),d) m
        writeIORef opts m'
        postRedisplay Nothing
    kbd (MouseButton WheelDown) _ _ pos@(Position x y) = do
        m <- readIORef opts
        let s' = keys m
        let s = (s' !! (fromIntegral y `div` sizePar))
        let (v,d) = m!s 
        let m' = insert s (max 0 (v-d),d) m
        writeIORef opts m'
        postRedisplay Nothing
    kbd (MouseButton LeftButton) Down _ pos@(Position x y) = do
        m <- readIORef opts
        let s' = keys m
        let s = (s' !! (fromIntegral y `div` sizePar))
        let (v,d) = m!s 
        let m' = insert s (fromIntegral x `div` 2,d) m
        writeIORef opts m'
        postRedisplay Nothing
    kbd a b c d = def a b c d

giveme opts val = do
    m <- readIORef opts
    return (fst $ m!val)
