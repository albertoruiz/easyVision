-- contour of something different from a background image

import EasyVision
import Graphics.UI.GLUT
import Control.Monad(when)

main = do
    sz <- findSize
    prepare

    cam <- getCam 0 sz ~> channels
    w <- evWindow (True,undefined) "contour" sz Nothing (mouse kbdQuit)

    launch $ do
        orig <- cam
        (rec,bg) <- getW w
        when rec $ do
            putW w (False,orig)
        when (not rec) $ inWin w $ do
            let mask = binarize8u 100 $ diffRGB bg orig
            drawImage $ copyMask32f (float . gray $ orig) mask


mouse _ st (Char 's') Down _ _ = do
    (_,b) <- getW st
    putW st (True,b)

mouse def _ a b c d = def a b c d

diffRGB ch1 ch2 = toGray (rd |+| gd |+| bd)
    where
        rd = float $ absDiff8u (rCh ch1) (rCh ch2)
        gd = float $ absDiff8u (gCh ch1) (gCh ch2)
        bd = float $ absDiff8u (bCh ch1) (bCh ch2)
