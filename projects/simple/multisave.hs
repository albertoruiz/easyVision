-- save multicamera video, interleaving frames
-- ./multisave webcam0 webcam1 etc.

import EasyVision
import Graphics.UI.GLUT hiding (Size)
import Text.Printf
import Data.List(tails,transpose)

main = do
    prepare
    sz <- findSize
    n <- numCams
    save <- optionalSaver sz

    multicam <- getMulticam sz n ~~> history 2 ~> difs
    wm <- evWindow () "views" (Size 150 (200*n)) Nothing (const kbdQuit)

    hist <- signalMonitor "Sync" 50 100 (printf "%.0f grayscale levels") (0,640*480*255/10)

    launch $ do
        (imgs,ds) <- multicam
        let x = blockImage [map grayscale imgs]
        hist ds
        inWin wm $ do
            drawImage x
            mapM_ (save.yuv) imgs

-----------------------------------------------------------------------------

history n = map (take n) . tails

difs imgs = (last imgs, map f (transpose imgs)) where
    f [x,y] = sum8u $ absDiff8u (grayscale x) (grayscale y)

------------------------------------------------------------------------------
