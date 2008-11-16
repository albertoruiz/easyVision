{- save some frames with a ROI

$ ./roisel source --save=selected.yuv
  stop video with SPACE, mark roi with mouse right button, save desired frames/roi with S
  ESC to end.
$ ./roisel selected
  to browse the selected frames/rois with mouse wheel

-}

import EasyVision
import Graphics.UI.GLUT
import Control.Monad(when)
import System(getArgs)
import qualified Data.Colour.Names as Col

main = do
    args <- getArgs
    create <- getFlag "--save"
    if create
        then main1
        else main2

--------------------------------------------------------------

main1 = do
    sz <- findSize
    (cam,ctrl) <- getCam 0 sz >>= withPause
    prepare

    mbname <- getRawOption "--save"
    let name = case mbname of
                Nothing -> error "--save=filename.yuv is required"
                Just nm -> nm

    w <- evWindow False "Press S to save frame" sz Nothing  (mouse (kbdcam ctrl))

    save <- optionalSaver sz

    launch $ do
        orig <- cam
        rec <- getW w
        roi <- getROI w

        inWin w $ do
            drawImage orig
            drawROI roi
            when rec $ do
                save orig
                appendFile (name++".roi") (show roi++"\n")
                putW w False

mouse _ st (Char 's') Down _ _ = do
    st $~ not

mouse def _ a b c d = def a b c d

-----------------------------------------------------------------

main2 = do
    sz <- findSize
    file:_ <- getArgs
    prepare

    roisraw <- readFile (file++".yuv.roi")
    let rois = map read (lines roisraw) :: [ROI]
        nframes = length rois
    print nframes

    cam <- mplayer (file++".yuv -benchmark") sz
    imgs <- sequence (replicate nframes cam)
    putStrLn $ show (length imgs) ++ " cases"

    seeRois imgs rois
    mainLoop

seeRois imgs rois = evWindow 0 "Selected ROI" (mpSize 20) (Just disp) (mouse kbdQuit)
    where
    disp st = do
        k <- get st
        drawImage (imgs!!k)
        lineWidth $= 3
        setColor' Col.yellow
        drawROI (rois!!k)
        text2D 50 50 (show $ k+1)
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        k <- get st
        st $= min (k+1) (length imgs -1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        k <- get st
        st $= max (k-1) 0
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d
