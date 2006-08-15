-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./hessian penguin.dv

import Ipp
import Graphics.UI.GLUT hiding (Size)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)

data MyState = ST {smooth :: Int}

--------------------------------------------------------------

main = do
    args <- getArgs
    let sz = Size 288 384
    cam <- openCamera (args!!0) sz

    state <- prepare cam ST {smooth = 5}

    addWindow "camera" sz Nothing keyboard state
    addWindow "hessian" sz Nothing keyboard state

    launch state worker

--------------------------------------------------------------

worker inWindow cam st = do

    camera <- grab cam
    im  <- scale8u32f 0 1 camera
    img <- (smooth st `times` gauss Mask5x5) im
    h   <- secondOrder img >>= hessian >>= abs32f >>= sqrt32f
    copyROI32f im h

    inWindow "hessian" $ do
        drawImage h --{vroi = vroi h}

    (mn,mx) <- Ipp.minmax h
    hotPoints <- localMax 7 h
             >>= thresholdVal32f (mx/5) 0.0 IppCmpLess
             >>= getPoints32f 1000

    inWindow "camera" $ do
        drawImage camera
        pointSize $= 3
        setColor 0 0.5 0
        pixelCoordinates (size camera)
        renderPrimitive Points $ mapM_ vertex hotPoints

    return st

-----------------------------------------------------------------
keyboard str (Char 'p') Down _ _ = do
    st <- readIORef str
    pause (camid st)
keyboard str (Char ' ') Down _ _ = do
    st <- readIORef str
    pause (camid st)
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = smooth (ust s) + 1}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = max (smooth (ust s) - 1) 0}}
keyboard _ _ _ _ _ = return ()
------------------------------------------------------------------
