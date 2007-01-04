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

    state <- prepare undefined 1

    giveme <- createParameters [("umb",(50,2)),("kk",(20,1)),("x",(75,1))] state

    addWindow "mplayer" sz Nothing (const (kbdcam ctrl)) state


    attachMenu LeftButton $ Menu 
        [MenuEntry "Quit" (exitWith ExitSuccess)
        ,MenuEntry "fullScreen" fullScreen
        ,MenuEntry "normal" (do windowSize $= GL.Size (fromIntegral $ width sz) (fromIntegral $ height sz)
                                windowPosition $= Position 100 100)

        ,MenuEntry "pause'" (ctrl "pause")
        , SubMenu "mode" $ Menu [ MenuEntry "RGB" $ modifyIORef state $ \s -> s {ust = 1}
                                , MenuEntry "Gray" $ modifyIORef state $ \s -> s {ust = 2}
                                , MenuEntry "Integral" $ modifyIORef state $ \s -> s {ust = 3}
                                , MenuEntry "Umbral" $ modifyIORef state $ \s -> s {ust = 4}
                                ]
        ]

    launch state (worker cam (giveme))

-----------------------------------------------------------------

k = 1/(640*480*128)

worker cam param inWindow _ op = do

    ith <- param "umb"
    let th = fromIntegral ith / 100

    inWindow "mplayer" $ case op of
        1 -> cam >>= drawImage
        2 -> cam >>= rgbToGray >>= drawImage
        3 -> cam >>= rgbToGray >>= integral >>= scale32f k >>= drawImage
        4 -> cam >>=
             rgbToGray >>=
             scale8u32f 0 1 >>=
             thresholdVal32f th 0 IppCmpLess >>=
             thresholdVal32f th 1 IppCmpGreater >>=
             drawImage

    return op
