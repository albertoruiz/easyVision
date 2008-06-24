{-# LANGUAGE CPP #-}

import EasyVision
import Numeric.LinearAlgebra

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withChannels >>= inThread >>= withPause

    prepare

    o <- createParameters [("sigma",realParam 1.0 0 3)
                          ,("steps",intParam 3 1 10)
                          ,("n",intParam 13  0 20)
                          ,("tot",intParam 200 1 500)
                          ,("h",realParam 0.3 0 2)
                          ,("mode",intParam 1 1 6)
                          ]

#define PAR(S) S <- getParam o "S"


    w <- evWindow (False, Pixel 0 0, constant (0::Double) 36) "Interest Points" sz Nothing  (const (kbdcam ctrl))

    launchFreq 15 $ do

        PAR(n)
        PAR(steps) :: IO Int
        PAR(sigma)
        PAR(mode)  :: IO Int
        PAR(h)
        PAR(tot)

        let sigmas = take (n+2) $ getSigmas sigma steps

        orig <- cam
        let imr = float $ gray $ orig

            feats = take tot $ fullHessian sigmas 100 h usurf imr

        inWin w $ do
            drawImage (rgb orig)
            mapM_ boxFeat feats



boxFeat p = do
    let Pixel r c = ipRawPosition p
--     setColor 0 0.5 0
--     drawVector (c-18) (r+2*ipRawScale p) (50* (ipDescriptor.ip) p)
    setColor 1 1 1
    text2D (fromIntegral c) (fromIntegral r) (show $ ipRawScale p)
    setColor 1 0 0
    drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)
