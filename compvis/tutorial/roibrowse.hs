-- $ ./roibrowse selected.yuv
--   Browses selected.yuv / selected.yuv.roi with mouse wheel
import EasyVision
import Graphics.UI.GLUT
import System(getArgs)
import qualified Data.Colour.Names as Col

main = do
    prepare
    sz <- findSize
    file:_ <- getArgs
    roisraw <- readFile (file++".roi")
    let rois = map read (lines roisraw) :: [ROI]
        nframes = length rois
    cam <- mplayer (file++" -benchmark") sz
    imgs <- sequence (replicate nframes cam)
    putStrLn $ show nframes ++ " cases"
    seeRois imgs rois
    mainLoop

seeRois imgs rois = evWindow 0 "Selected ROI" (mpSize 20) (Just disp) (mouse kbdQuit)
    where
    disp st = do
        k <- get st
        drawImage (imgs!!k)
        lineWidth $= 3; setColor' Col.yellow
        drawROI (rois!!k)
        text2D 50 50 (show $ k+1)
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        st $~ (min (length imgs -1) . (+1))
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        st $~ (max 0 . subtract 1)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d
