-- demo of siftgpu

import EasyVision
import ImagProc.GPU.SIFT
import Control.Monad((>=>))

main = run $ camera >>= wsift gray >>= timeMonitor

wsift f = sift f >=> siftmonitor f

sift f cam = do
    fsift <- getSift
    o <- winSIFTParams
    return $ do
       x <- cam
       pars <- o
       return (x, fsift pars (f x))

siftmonitor f = monitor "SIFT GPU" (mpSize 20) sh where
    sh (x, feats) = do
        let im = f x
        drawImage' im
        pointCoordinates (size im)
        setColor 1 1 0
        drawInterestPoints feats

