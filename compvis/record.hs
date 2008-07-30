-- save captured video
-- then you can convert the generated yuv to a nicer format:
-- $ ./justsave webcam1 --save=file.yuv [--limit=numframes]
-- $ mencoder file.yuv -o file.avi -ovc lavc -fps 15

import EasyVision
import Graphics.UI.GLUT
import Control.Monad(when)

main = do
    sz <- findSize
    (cam,ctrl) <- getCam 0 sz >>= withPause
    prepare

    ok <- hasValue "--save"

    let title = if ok then "Click to start/stop recording"
                      else "Image"

    w <- evWindow False title sz Nothing  (mouse (kbdcam ctrl))

    save <- optionalSaver sz

    launch $ do
        orig <- cam
        rec <- getW w

        inWin w $ do
            drawImage orig
            when rec $ do
                setColor 1 0 0
                text2D 20 20 "Recording..."
                save orig

----------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $~ not

mouse def _ a b c d = def a b c d