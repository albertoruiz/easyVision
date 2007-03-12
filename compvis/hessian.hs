
import EasyVision
import Graphics.UI.GLUT hiding (Size,minmax)
import Data.IORef
import System.Environment(getArgs)
import ImagProc.Ipp

data MyState = ST {smooth :: Int}

--------------------------------------------------------------

main = do
    args <- getArgs
    let sz = Size 288 384
    (cam,ctrl) <- mplayer (args!!0) sz >>= withPause

    state <- prepare ST {smooth = 5}

    addWindow "camera" sz Nothing (const (kbdcam ctrl)) state
    addWindow "hessian" sz Nothing keyboard state

    launch state (worker cam)

--------------------------------------------------------------

hess s im = purifyWith (copyROI32f im) (iohess s im)

iohess s im =
    (s `times` gauss Mask5x5) im
    >>= secondOrder
    >>= hessian
    >>= abs32f
    >>= sqrt32f

--------------------------------------------------------------

worker cam inWindow st = do

    camera <- cam >>= yuvToGray >>= scale8u32f 0 1
    let h = hess (smooth st) camera

    inWindow "hessian" $ do
        drawImage h --{vroi = vroi h}

    (mn,mx) <- minmax h
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

keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = smooth (ust s) + 1}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = max (smooth (ust s) - 1) 0}}
keyboard _ _ _ _ _ = return ()
------------------------------------------------------------------
