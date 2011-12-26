-- example of virtual camera

import EasyVision
import Graphics.UI.GLUT hiding (Size)
import Util.Options

main = do
    sz <- findSize

    th <- getOption "--sensi" 0.01

    (cam,ctrl) <- getCam 0 sz
                  >>= addSmall (Size 90 120) (gray.channels)
                  >>= detectMotion (th*255*90*120<)
                  >>= withPause

    prepare

    w <- evWindow () "motion" sz Nothing  (const (kbdcam ctrl))
    windowStatus $= Hidden

    sv <- optionalSaver sz

    launch (worker w cam sv)

-----------------------------------------------------------------

worker w cam save = do

    inWin w $ do
        orig <- cam >>= return . fst
        drawImage (yuvToRGB orig)
        -- system "artsplay /usr/share/sounds/KDE_Notify.wav"
        save orig
        windowStatus $= Shown
