-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./hessian penguin.dv

import Ipp
import Graphics.UI.GLUT
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)

data MyState = ST {smooth :: Int}

--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)
    
    state <- prepare cam ST {smooth = 5} 
    
    addWindow "camera" (w,h) Nothing keyboard state
    addWindow "hessian" (w,h) Nothing keyboard state
    
    launch state worker
    
-----------------------------------------------------------------
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = smooth (ust s) + 1}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = max (smooth (ust s) - 1) 0}}
keyboard _ _ _ _ _ = return ()
------------------------------------------------------------------

worker inWindow camera st = do
            
    im  <- scale8u32f 0 1 camera
    img <- (smooth st `times` gauss Mask5x5) im
    h   <- hessian img >>= abs32f >>= sqrt32f
    copyROI32f im h
    
    inWindow "hessian" $ do
        display im {vroi = vroi h}
    
    (mn,mx) <- Ipp.minmax h
    hotPoints <- localMax 7 h >>= thresholdVal32f (mx/2) 0.0 ippCmpLess >>= getPoints32f 200 
        
    inWindow "camera" $ do
        display camera
        mydraw hotPoints
    
    return st

------------------------------------------------------
mydraw xs = do
    pointSize $= 3
    renderPrimitive Points $ mapM_ f xs where
        f [y,x] = vertex (Vertex2 (fromIntegral x::GLfloat) (288-1-fromIntegral y))
