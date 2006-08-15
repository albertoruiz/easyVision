-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./player penguin.dv

import Ipp
import Graphics.UI.GLUT hiding (RGB,Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)

-----------------------------------------------------------------

main = do
    args <- getArgs
    let sz = Size 576 720
    cam <- openCamera (args!!0) sz

    state <- prepare cam True

    addWindow "camera" sz Nothing keyboard state

    attachMenu LeftButton $ Menu 
        [MenuEntry "Quit" (exitWith ExitSuccess)
        ,MenuEntry "fullScreen" fullScreen
        ,MenuEntry "normal" (do windowSize $= GL.Size 720 576
                                windowPosition $= Position 100 100)
        ,MenuEntry "pause" $ (do st<- readIORef state
                                 pause (camid st))
        , SubMenu "mode" $ Menu [ MenuEntry "RGB" $ modifyIORef state $ \s -> s {ust = True}
                                , MenuEntry "Gray" $ modifyIORef state $ \s -> s {ust = False}
                                ]
        ]

    launch state worker

-----------------------------------------------------------------

worker inWindow cam st = do

    inWindow "camera" $ case st of
        True ->  grab cam >>= (drawImage :: ImageRGB  -> IO ())
        False -> grab cam >>= (drawImage :: ImageGray -> IO ())

    return st

------------------------------------------------------------------
keyboard str (Char 'p') Down _ _ = do
    st <- readIORef str
    pause (camid st)
keyboard str (Char ' ') Down _ _ = do
    st <- readIORef str
    pause (camid st)
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess
keyboard _ _ _ _ _ = return ()
-------------------------------------------------------------------
