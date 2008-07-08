-- shows --tot [250] frames and exits.
-- Useful for grab speed measurements.

import EasyVision
import Control.Monad(when)
import System.Exit

main = do
    sz <- findSize
    cam <- getCam 0 sz
    n <- getOption "--tot" 250
    prepare

    w <- evWindow (0::Int) "image" sz Nothing  (const kbdQuit)

    launch $ do
        orig <- cam

        k <- getW w

        inWin w $ do
            drawImage (gray (channels orig))
            text2D 20 20 (show k)

        when (k==n) $ exitWith ExitSuccess
        putW w (k+1)
