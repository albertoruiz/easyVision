
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

    state <- prepare' ST {smooth = 5}

    addWindow "camera" sz Nothing (const (kbdcam ctrl)) state
    addWindow "hessian" sz Nothing keyboard state

    launch' state (worker cam)

--------------------------------------------------------------

hess s im = sqrt32f
          $ abs32f
          $ hessian
          $ secondOrder
          $ (s `times` gauss Mask5x5) im

times n f = (!!n) . iterate f

--------------------------------------------------------------

worker cam inWindow st = do

    camera <- cam >>= return . scale8u32f 0 1 . yuvToGray
    let h = hess (smooth st) camera

    inWindow "hessian" $ do
        drawImage h --{vroi = vroi h}

    let (mn,mx) = minmax h
        hotPoints = getPoints32f 1000
                  $ thresholdVal32f (mx/5) 0.0 IppCmpLess
                  $ localMax 7 h

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
