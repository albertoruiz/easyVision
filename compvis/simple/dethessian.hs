{-# LANGUAGE CPP #-}

-- ./hessian [--size=15] [url]
-- click to store points

import EasyVision
import Numeric.LinearAlgebra
import Graphics.UI.GLUT
import Control.Monad(when)
import Data.List(minimumBy)
import Text.Printf(printf)

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz ~> channels >>= withPause

    prepare

    o <- createParameters [("sigma",realParam 1.0 0 3)
                          ,("steps",intParam 3 1 10)
                          ,("n",intParam 13  0 20)
                          ,("tot",intParam 200 1 500)
                          ,("h",realParam 0.2 0 2)
                          ,("mode",intParam 0 0 1)
                          ,("err",realParam 0.3 0 1)
                          ]

    w <- evWindow [] "Interest Points" sz Nothing  (mouse (kbdcam ctrl))

    roi <- getROI w
    evROI w $= roiFromPixel (roiRadius roi `div`2) (roiCenter roi)

    launch $ do

#define PAR(S) S <- getParam o "S"

        PAR(n)
        PAR(steps) :: IO Int
        PAR(sigma)
        PAR(mode)  :: IO Int
        PAR(h)
        PAR(tot)
        PAR(err)

        roi <- getROI w

        let sigmas = take (n+2) $ getSigmas sigma steps

        orig <- cam
        let imr = float $ gray $ orig

            descrip = if mode == 0 then usurf else surf
            feats = take tot $ fullHessian (descrip 2 3) sigmas 100 h imr
            sel = filter (inROI roi . ipRawPosition) feats

        vs <- getW w
        when (null vs && not (null sel)) $ do
            putW w sel

        let matches = findMatches err feats vs

        inWin w $ do
            drawImage (rgb orig)
            lineWidth $= 1
            setColor 0.5 0.5 0.5
            mapM_ boxFeat feats
            setColor 0 0 0
            drawROI roi
            when (not $ null vs) $ do
                lineWidth $= 2
                setColor 1 1 1
                mapM_ boxFeat matches
                setColor 0 0 1
                text2D 20 20 $ printf "%d matches" (length matches)


boxFeat p = do
    drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)


mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_) <- getW st
    putW st ([])
mouse def _ a b c d = def a b c d

distv a b = pnorm PNorm2 (a-b)

findMatches h as bs = filter f as where
    f v = minimum (map (g v) bs) < h
    g = distv `on` ipDescriptor.ip
