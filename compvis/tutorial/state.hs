import EasyVision
import Graphics.UI.GLUT

main = do
    sz <- findSize
    prepare
    cam <- getCam 0 sz ~> channels
    w <- evWindow (True,undefined) "bg diff" sz Nothing (mouse kbdQuit)
    launch $ do
        img <- fmap grayscale cam
        (rec,bg) <- getW w
        if rec
            then putW w (False, img)
            else inWin w $ drawImage $ absDiff8u img bg

mouse _ st (Char 's') Down _ _ = st $= (True,undefined)
mouse def _ a b c d = def a b c d
