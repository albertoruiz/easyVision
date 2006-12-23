{-# OPTIONS -fffi #-}

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
import Foreign
import Foreign.C.Types
import Foreign.C.String(newCString)
import Ipp.Core


main = do
    args <- getArgs
    let sz = Size 576 720
    cam <- openCamera (args!!0) sz

    let szmp = Size 480 640


    mplayer <- cameraRGB (args!!1) {-Nothing --or-} (Just szmp)

    state <- prepare cam (True,mplayer)

    --addWindow "camera" sz Nothing keyboard state

    addWindow "mplayer" szmp Nothing keyboard state


    attachMenu LeftButton $ Menu 
        [MenuEntry "Quit" (exitWith ExitSuccess)
        ,MenuEntry "fullScreen" fullScreen
        ,MenuEntry "normal" (do windowSize $= GL.Size 720 576
                                windowPosition $= Position 100 100)
        ,MenuEntry "pause" $ (do st<- readIORef state
                                 pause (camid st))
        , SubMenu "mode" $ Menu [ MenuEntry "RGB" $ modifyIORef state $ \s -> s {ust = (True,mplayer)}
                                , MenuEntry "Gray" $ modifyIORef state $ \s -> s {ust = (False,mplayer)}
                                ]
        ]

    launch state worker

-----------------------------------------------------------------

worker inWindow cam (st,mp) = do
{-
    inWindow "camera" $ case st of
        True ->  grab cam >>= (drawImage :: ImageRGB  -> IO ())
        False -> grab cam >>= (drawImage :: ImageGray -> IO ())
-}
    inWindow "mplayer" $ do
        --im <- img RGB (Size 480 640)
        --getFrame mp (castPtr $ ptr im)
        --drawImage (C im)
        im <- mp
        drawImage im
        --print "."

    return (st,mp)

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
