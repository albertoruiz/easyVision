-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./a.out penguin.dv

import Ipp
import Typical
import Draw
import Camera
import Graphics.UI.GLUT
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
     

main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) 1 (288,384)

    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]

    dummyimg <- img 1 1 100 100
    state <- newIORef $ State {camid = cam, frame = 0, pause = False,
                               rawimage = dummyimg, hess = dummyimg, locmax = dummyimg,
                                smooth = 5}

    w1 <- installWindow "camera" (w,h) rawimage keyboard state
    w2 <- installWindow "hessian" (w,h) hess keyboard state
    
    attachMenu LeftButton $ Menu [MenuEntry "Quit" (exitWith ExitSuccess)]
    
    w3 <- installWindow "local maxima" (w,h) locmax keyboard state

    idleCallback $= Just ( do
        st <- readIORef state
        when (not (pause st)) $ do
            im <- grab (camid st)
            writeIORef state (st {rawimage = im})
        st <- readIORef state    
        newstate <- worker st
        writeIORef state newstate {frame = frame st +1}
        mapM_ (postRedisplay . Just) [w1,w2,w3])

    mainLoop

keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess
keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {smooth = smooth s + 1}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {smooth = max (smooth s - 1) 0}    
keyboard _ _ _ _ _ = return ()

-------------------------------------------------------

data State = 
    State { camid :: Camera
          , frame :: Int 
          , pause :: Bool
          , rawimage :: Img
          , hess :: Img
          , locmax :: Img
          , smooth :: Int
          }

worker st = do
    --when (frame st == 100) (exitWith ExitSuccess)
        
    im  <- scale8u32f 0 1 (rawimage st)
    img <- ((smooth st) `times` gauss 55) im
    h   <- hessian img >>= abs32f >>= sqrt32f
    copyROI32f im h
    lm <- localMax 7 h
    
    return (st {hess = im {vroi = vroi h}, locmax = lm})
