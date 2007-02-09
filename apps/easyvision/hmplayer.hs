-- This should work with any video source

import Ipp
import Graphics.UI.GLUT hiding (RGB,Size,minmax)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)

import Ipp.Core
import System
import System.Process
import System.IO
import Foreign
import System.IO.Error
import Control.Concurrent

-----------------------------------------------------------



------------------------------------------------------------

main = do
    args <- getArgs

    let sz = Size (480`div`1) (640`div`1)
    --let sz = Size (480`div`2) (640`div`2)
    --let sz = Size 288 384

    

    --sequence $ replicate 5 (readMPlayer k)


    (cam, ctrl)  <- cameraRGB (args!!0) {-Nothing --or-} (Just sz)

    state <- prepare undefined "RGB"

    o <- createParameters state [("umbral",realParam 0.5 0 1),
                                 ("h",percent 20),
                                 ("smooth",intParam 3 0 10)]

    addWindow "mplayer" sz Nothing (const (kbdcam ctrl)) state
                                   ---- or undefined ---

    getRoi <- roiControl (ROI 0 (height sz) (width sz`div`2) (width sz)) (kbdcam ctrl)

    let mode m = MenuEntry m $ modifyIORef state $ \s -> s {ust = m}

    attachMenu LeftButton $ Menu 
        [MenuEntry "Quit" (exitWith ExitSuccess)
        ,MenuEntry "fullScreen" fullScreen
        ,MenuEntry "normal" (do windowSize $= GL.Size (fromIntegral $ width sz) (fromIntegral $ height sz)
                                windowPosition $= Position 100 100)

        --,MenuEntry "pause'" (ctrl "pause")
        , SubMenu "mode" $ Menu $ map mode
            ["prueba","RGB","Gray","Float", "Median"
            ,"Integral","Umbraliza","Hessian"
            ,"Corners", "Features", "Canny"]
        ]

    launch state (worker cam o getRoi)

-----------------------------------------------------------------

k = 1/(640*480*128)

worker cam param getRoi inWindow _ op = do

    th <- getParam param "umbral"
    ph <- getParam param "h" :: IO Int
    let h1 = fromIntegral ph / 100
    let h2 = fromIntegral ph / 100 -- different types: Float and Double...
    smooth <- getParam param "smooth"

    inWindow "mplayer" $ case op of

        "RGB"  ->
             cam >>= drawImage
        "Gray" ->
             cam >>= rgbToGray >>= drawImage
        "Float" ->
             cam >>= rgbToGray >>= scale8u32f 0 1 >>= drawImage
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
             smooth `times` gauss Mask5x5 >>= secondOrder >>= hessian >>=
             abs32f >>= sqrt32f >>= drawImage
        "Corners" -> do
             im <- cam >>= rgbToGray >>= scale8u32f 0 1
             ips <- getCorners smooth 7 h1 500 im
             drawImage im
             pixelCoordinates (size im)
             setColor 1 0 0
             pointSize $= 3
             renderPrimitive Points (mapM_ vertex ips)
             text2D 10 20 (show $ length ips)
        "Features" -> do
             orig <- cam
             im <- rgbToGray orig >>= scale8u32f 0 1
             ips <- getSaddlePoints smooth 7 h2 500 20 10 im
             drawImage orig
             pointCoordinates (size im)
             setColor 1 0 0
             pointSize $= 3
             text2D 0.9 0 (show $ length ips)
             renderPrimitive Points (mapM_ vertex (map ipPosition ips))
        "Canny" -> do
             roi <- getRoi
             orig <- cam
             im <- rgbToGray orig >>= scale8u32f 0 1
             F s <- (smooth `times` gauss Mask5x5) im
             gx <- sobelVert $ F s {vroi = roi `intersection` vroi s}
             gy <- sobelHoriz $ F s {vroi = roi `intersection` vroi s}
             c <- canny (gx,gy) (th/3,th) >>= scale8u32f 0 1
             copyROI32f im c
             drawImage im

        "Median" -> do
             orig <- cam
             im <- rgbToGray orig
             s <- (smooth `times` median Mask5x5) im
             drawImage s

    return op

text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s