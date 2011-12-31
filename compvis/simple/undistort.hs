-- remove radial distortion

import EasyVision
import Graphics.UI.GLUT hiding (Size,Point)
import Control.Monad(when)
import ImagProc.Ipp.Core
import Foreign(nullPtr)
import ImagProc.Ipp.Auto

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz ~> channels >>= withPause

    umap <- undistortMap sz 2 (-0.28)

    w1 <- evWindow () "original" sz Nothing (const (kbdcam ctrl))
    w2 <- evWindow () "undistorted" sz Nothing (const (kbdcam ctrl))
    o <- createParameters [("k",realParam (-0.28) (-1) 1),
                           ("mode", stringParam "compute" ["none","table","compute","nn"])]
    launch $ do
        k <- getParam o "k"
        mode <- getParam o "mode"

        x <- cam

        inWin w1 $ drawImage (rgb x)
        case mode of
            "none"    -> inWin w2 $ drawImage (grayscale x)
            "compute"  -> inWin w2 $ drawImage $ uradial 2 k (grayscale x)
            "table"    -> inWin w2 $ drawImage $ remap umap InterpLinear (grayscale x)
            "nn"       -> inWin w2 $ drawImage $ remap umap InterpNN (grayscale x)
