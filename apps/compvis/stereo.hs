
import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)

--import Ipp.Core

import Data.Map

------------------------------------------------------------

main = do
    args <- getArgs

    let opts = fromList $ zip args (tail args)

    let sz = if member "--size" opts
                 then mpSize $ read $ findWithDefault "20" "--size" opts
                 else Size (read $ findWithDefault "480" "--rows" opts)
                           (read $ findWithDefault "640" "--cols" opts)

    (cam1,ctrl1) <- mplayer (args!!0) sz >>= withPause
    (cam2,ctrl2) <- mplayer (args!!1) sz >>= withPause

    state <- prepare ()

    addWindow "left" sz Nothing  (const (kbdcam ctrl1)) state
    addWindow "right" sz Nothing (const (kbdcam ctrl2)) state
    addWindow "dif" sz Nothing   (const (kbdcam ctrl1)) state

    launch state (worker cam1 cam2)

-----------------------------------------------------------------


worker cam1 cam2 inWindow _ = do

    im1 <- cam1 >>= yuvToRGB 
    im2 <- cam2 >>= yuvToGray >>= scale8u32f 0 1

    g1 <- rgbToGray im1

    histogram [0,64 .. 256] g1 >>= print

    f1 <- scale8u32f 0 1 g1

    inWindow "left" $ drawImage im1

    inWindow "right" $ drawImage im2

    inWindow "dif" $ do
        (f1 |-| im2) >>= abs32f >>= drawImage

    return ()

