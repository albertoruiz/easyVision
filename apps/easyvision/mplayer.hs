-- This should work with any video source

import Ipp
import Graphics.UI.GLUT hiding (RGB,Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)


main = do
    args <- getArgs
    --let sz = Size 576 720
    --cam <- openCamera (args!!0) sz

    let szmp = Size 480 640


    mplayer <- cameraRGB (args!!0) {-Nothing --or-} (Just szmp)

    state <- prepare undefined (True,mplayer)

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

    inWindow "mplayer" $ case st of
        True ->  mp >>= drawImage
        False -> mp >>= rgbToGray >>= drawImage

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
