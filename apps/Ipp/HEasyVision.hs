-----------------------------------------------------------------------------
{- |
Module      :  Ipp.HEasyVision
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

A minimalist version of EasyVision [ref.] using HOpenGL.

-}
-----------------------------------------------------------------------------

module Ipp.HEasyVision
where

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

addWindow name (w,h) dcallback kcallback state = do
    w <- installWindow name (w,h) dcallback kcallback state
    modifyIORef state $ \s -> s { wins = insert name w (wins s)}
    return w

data State userState = 
    State { wins  :: Map String Window
          , camid :: Camera
          , frame :: Int 
          , pause :: Bool
          , camera :: Img
          , ust  :: userState
}

prepare cam s = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Decal

    state <- newIORef State {camid = cam, frame = 0, pause = False,
                             camera = undefined, wins = empty, ust = s}
    return state

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
