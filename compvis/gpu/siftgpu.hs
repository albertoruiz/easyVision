-- demo of siftgpu

import EasyVision
import ImagProc.GPU.SIFT

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz ~> gray . channels >>= withPause

    sift <- getSift

    w <- evWindow () "SIFT GPU" sz Nothing (const (kbdcam ctrl))
    o <- winSIFTParams
    launch $ do
        x <- cam
        pars <- o
        inWin w $ do
            let feats = sift pars x
            drawImage x
            pointCoordinates sz
            setColor 1 1 0
            drawInterestPoints feats
