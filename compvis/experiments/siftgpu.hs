{-# LANGUAGE RecordWildCards #-}

-- demo of siftgpu


import EasyVision
import Graphics.UI.GLUT hiding (Size,Point)
import Control.Monad(when)
import Numeric.LinearAlgebra
import ImagProc.C.SIFT
import Vision
import Control.Applicative

main = do
    sz <- findSize
    prepare

    (cam,ctrl) <- getCam 0 sz ~> gray . channels >>= withPause

    sift <- getSift

    w <- evWindow () "SIFT GPU" sz Nothing (const (kbdcam ctrl))
    o <- userSIFTParam
    launch $ do
        x <- cam
        pars <- o
        inWin w $ do
            let feats = sift pars x
            drawImage x
            pointCoordinates sz
            drawInterestPoints feats

userSIFTParam = do
    SIFTParam{..} <- getSIFTParam

    o <- createParameters' "SIFT Parameters"
        [ ("oct1" , intParam    oct1   0 3)
        , ("thres", realParam   thres  0 0.01)
        , ("nmax",  intParam    nmax   0 2000)
        ]

    return $ SIFTParam <$> getParam o "oct1"
                       <*> getParam o "thres"
                       <*> getParam o "nmax"

getSIFTParam = SIFTParam <$> getOption "--oct1"  oct1
                         <*> getOption "--thres" thres
                         <*> getOption "--nmax"  nmax

    where SIFTParam{..} = defaultSIFTParam
