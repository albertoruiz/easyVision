-- test of panoramic combinator

module Main where

import EasyVision
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Control.Monad(foldM)

a & b = panoramic (Size 320 640) a b


main = do
    sz  <- findSize
    n <- numCams

    cams <- mapM (flip getCam sz) [0..n-1]

    let cf k = cams!!k >>= yuvToGray >>= scale8u32f 0 1

    prepare

    cam <- foldM (&) (cf 0) (map cf [1..n-1])

    wDest  <- evWindow () "pano" (Size 320 640) Nothing (mouse (kbdQuit))

    launch (worker cam wDest)

-----------------------------------------------------------------

worker cam wDest = do
    inWin wDest $
        cam >>= drawImage

---------------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $= ()

mouse def _ a b c d = def a b c d
