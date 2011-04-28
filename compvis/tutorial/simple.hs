import EasyVision
import Graphics.UI.GLUT
import System(getArgs)

main = do
    sz <- findSize
    file:_ <- getArgs
    prepare
    cam <- mplayer ("mf://"++file) sz
    img <- cam

    let x = float. grayscale . channels $ img

    watch "Image" (const id) img
    watch "3 * 4" (const $ gaussS 3 . gaussS 4) x
    watch "Gaussian" (gaussS . fromIntegral) x

    mainLoop

watch title f img = evWindow 0 title (size img) (Just disp) (mouse kbdQuit)
    where
    disp st = do
        k <- get st
        drawImage (f k img)
        text2D 15 15 (show k)
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        st $~ (+1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        st $~ (max 0 . subtract 1)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d
