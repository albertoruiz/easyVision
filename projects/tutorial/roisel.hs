-- $ ./roisel source --save=selected.yuv
--   SPACE to stop video, mark region with mouse right button,
--   S to save desired frame/region, ESC to end.
import EasyVision
import Graphics.UI.GLUT
import Control.Monad(when)
import Util.Options

main = do
    sz <- findSize
    (cam,ctrl) <- getCam 0 sz >>= withPause
    prepare
    mbname <- getRawOption "--save"
    let name = case mbname of
                Nothing -> error "--save=filename.yuv is required"
                Just nm -> nm
    w <- evWindow False "Press S to save frame"
                  sz Nothing  (mouse (kbdcam ctrl))
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

mouse _ st (Char 's') Down _ _ = putW st True
mouse def _ a b c d = def a b c d
