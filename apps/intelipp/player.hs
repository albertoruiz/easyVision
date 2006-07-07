-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./player penguin.dv

import Ipp
import Typical
import Draw
import Camera
import Graphics.UI.GLUT hiding (RGB)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import HEasyVision

-----------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (1000,1300) 

    state <- prepare cam ()

    addWindow "camera" (w,h) Nothing keyboard state
    
    attachMenu LeftButton $ Menu [MenuEntry "Quit" (exitWith ExitSuccess),
                                  MenuEntry "fullScreen" fullScreen,
                                  MenuEntry "normal" (windowSize $= Size 384 288)]
        
    launch state worker   
    
    
------------------------------------------------------------------    
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess
keyboard _ _ _ _ _ = return ()

-------------------------------------------------------------------
worker inWindow camera st = do
    --when (frame st == 1500) (exitWith ExitSuccess)
    
    inWindow "camera" $ do
        display camera
        
    
    return st
