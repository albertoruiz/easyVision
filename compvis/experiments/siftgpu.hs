{-# LANGUAGE RecordWildCards #-}

-- demo of siftgpu


import EasyVision
import Graphics.UI.GLUT hiding (Size,Point)
import Control.Monad(when)
import Numeric.LinearAlgebra
import ImagProc.C.SIFT
import EasyVision.MiniApps.SiftParams
import Vision
import Control.Applicative

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz ~> gray . channels >>= withPause

    sift <- getSift

    w <- evWindow () "SIFT GPU" sz Nothing (const (kbdcam ctrl))
    o <- userSIFTParams
    launch $ do
        x <- cam
        pars <- o
        inWin w $ do
            let feats = sift pars x
            drawImage x
            pointCoordinates sz
            setColor 1 1 0
            drawInterestPoints feats
