module HEasyVision 
where

import Ipp
import Typical
import Draw
import Camera
import Graphics.UI.GLUT hiding (RGB)
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
