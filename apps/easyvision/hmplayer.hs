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

    let sz = Size (480`div`1) (640`div`1)

    (cam, ctrl)  <- cameraRGB (args!!0) {-Nothing --or-} (Just sz)

    state <- prepare undefined "RGB"

    o <- createParameters state [("umbral",realParam 0.5 0 1),("h",percent 20)]

    addWindow "mplayer" sz Nothing (const (kbdcam ctrl)) state

    let mode m = MenuEntry m $ modifyIORef state $ \s -> s {ust = m}

    attachMenu LeftButton $ Menu 
        [MenuEntry "Quit" (exitWith ExitSuccess)
        ,MenuEntry "fullScreen" fullScreen
        ,MenuEntry "normal" (do windowSize $= GL.Size (fromIntegral $ width sz) (fromIntegral $ height sz)
                                windowPosition $= Position 100 100)

        ,MenuEntry "pause'" (ctrl "pause")
        , SubMenu "mode" $ Menu $ map mode
            ["RGB","Gray","Integral","Umbraliza","Hessian", "Corners", "Features"]
        ]

    launch state (worker cam o)

-----------------------------------------------------------------

k = 1/(640*480*128)

worker cam param inWindow _ op = do

    th <- getParam param "umbral"
    ph <- getParam param "h" :: IO Int
    let h1 = fromIntegral ph / 100
    let h2 = fromIntegral ph / 100 -- different types: Float and Double...

    inWindow "mplayer" $ case op of
        "RGB"  ->
             cam >>= drawImage
        "Gray" ->
             cam >>= rgbToGray >>= drawImage
        "Integral" ->
             cam >>= rgbToGray >>= integral >>= scale32f k >>= drawImage
        "Umbraliza" ->
             cam >>=
             rgbToGray >>=
             scale8u32f 0 1 >>=
             thresholdVal32f th 0 IppCmpLess >>=
             thresholdVal32f th 1 IppCmpGreater >>=
             drawImage
        "Hessian" ->
             cam >>= rgbToGray >>= scale8u32f 0 1 >>=
             3 `times` gauss Mask5x5 >>= secondOrder >>= hessian >>=
             abs32f >>= sqrt32f >>= drawImage
        "Corners" -> do
             im <- cam >>= rgbToGray >>= scale8u32f 0 1
             ips <- getCorners 3 7 h1 500 im
             drawImage im
             pixelCoordinates (size im)
             setColor 1 0 0
             pointSize $= 3
             renderPrimitive Points (mapM_ vertex ips)
             print (length ips)
        "Features" -> do
             orig <- cam
             im <- rgbToGray orig >>= scale8u32f 0 1
             ips <- getSaddlePoints 3 7 h2 500 20 10 im
             drawImage orig
             pointCoordinates (size im)
             setColor 1 0 0
             pointSize $= 3
             print (length ips)
             renderPrimitive Points (mapM_ vertex (map ipPosition ips))

    return op
