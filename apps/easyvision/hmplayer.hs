-- This should work with any video source

import Ipp
import Graphics.UI.GLUT hiding (RGB,Size)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)

import Ipp.Core

main = do
    args <- getArgs

    let sz = Size 480 640

    (cam, ctrl)  <- cameraRGB (args!!0) {-Nothing --or-} (Just sz)

    state <- prepare undefined (1,cam)

    addWindow "mplayer" sz Nothing keyboard state


    attachMenu LeftButton $ Menu 
        [MenuEntry "Quit" (exitWith ExitSuccess)
        ,MenuEntry "fullScreen" fullScreen
        ,MenuEntry "normal" (do windowSize $= GL.Size 720 576
                                windowPosition $= Position 100 100)
        ,MenuEntry "pause" $ (do st<- readIORef state
                                 pause (camid st))

        ,MenuEntry "pause'" (ctrl "pause")
        , SubMenu "mode" $ Menu [ MenuEntry "RGB" $ modifyIORef state $ \s -> s {ust = (1,cam)}
                                , MenuEntry "Gray" $ modifyIORef state $ \s -> s {ust = (2,cam)}
                                , MenuEntry "Integral" $ modifyIORef state $ \s -> s {ust = (3,cam)}
                                ]
        ]

    launch state worker

-----------------------------------------------------------------

k = 1/(640*480*128)


worker inWindow cam (op,mp) = do

    inWindow "mplayer" $ case op of
        1 -> mp >>= drawImage
        2 -> mp >>= rgbToGray >>= drawImage
        3 -> mp >>= rgbToGray >>= integral >>= scale32f k >>= drawImage

    return (op,mp)

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
