{-# LANGUAGE CPP #-}

-- ./matches2 [--size=15] [url1] [url2]

import EasyVision
import Numeric.LinearAlgebra
import Graphics.UI.GLUT hiding (Size, Point)
import Control.Monad(when)
import Data.List(minimumBy)
import Text.Printf(printf)
import Control.Parallel.Strategies
import Vision

-----------------------------------------------------------

interestPoints n h orig = feats where
    sigmas = take (n+2) $ getSigmas 1 3
    imr = float $ gray $ orig
    feats = take 200 $ fullHessian (usurf 2 4) sigmas 100 h imr

-----------------------------------------------------------
---- two cameras

#define PAR(S) S <- getParam o "S"

main2 = do
    sz@(Size r c) <- findSize

    cam0 <- getCam 0 sz >>= inThread >>= withChannels
    cam1 <- getCam 1 sz >>= inThread >>= withChannels

    prepare

    o <- createParameters [("sigma",realParam 1.0 0 3)
                          ,("steps",intParam 3 1 10)
                          ,("n",intParam 13  0 20)
                          ,("tot",intParam 200 1 500)
                          ,("h",realParam 0.2 0 2)
                          ,("mode",intParam 0 0 1)
                          ,("err",realParam 0.3 0 1)
                          ,("ranThres", realParam 0.003 0 0.01)
                          ,("ranProb", realParam 0.9 0.5 1)
                          ]

    wm <- evWindow () "Matches" (Size r (2*c)) Nothing  (const (kbdQuit))

    launchFreq 15 $ do

        PAR(err) :: IO Double
        PAR(n)
--        PAR(ranProb)
--        PAR(ranThres)
        PAR(h)

        orig0 <- cam0
        orig1 <- cam1

        let [feats0,feats1] = parMap rnf (map ip . interestPoints n h) [orig0,orig1]

        let matches = basicMatches (feats0, feats1) distFeat err

        inWin wm $  do
            let pair = blockImage [[gray orig0, gray orig1]]
            drawImage pair
            pointCoordinates (size pair)
            when (length matches > 0) $ do
                let Size h w = sz
                --setColor 0.5 0.5 0.5
                renderPrimitive Lines $ mapM_ drawPair' matches
                --setColor 1 1 1
                --renderPrimitive Lines $ mapM_ drawPair goodmatches
                --pixelCoordinates sz
                --when (err < 1E-2) $ text2D 20 20 (show (foc,err))

-----------------------------------------------------------------------
-- one camera and click

main1 = do
    sz@(Size r c) <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withChannels >>= inThread >>= withPause

    prepare

    o <- createParameters [("sigma",realParam 1.0 0 3)
                          ,("steps",intParam 3 1 10)
                          ,("n",intParam 13  0 20)
                          ,("tot",intParam 200 1 500)
                          ,("h",realParam 0.2 0 2)
                          ,("mode",intParam 0 0 1)
                          ,("err",realParam 0.3 0 1)
                          ]

    w <- evWindow ([],undefined) "Interest Points" sz Nothing  (mouse (kbdcam ctrl))
    wm <- evWindow () "Matches" (Size r (2*c)) Nothing  (const (kbdcam ctrl))

    roi <- getROI w
    evROI w $= roiFromPixel (roiRadius roi `div`2) (roiCenter roi)

    launchFreq 15 $ do

        PAR(n)
        PAR(steps) :: IO Int
--        PAR(sigma)
        PAR(mode)  :: IO Int
        PAR(h)
--        PAR(tot)
        PAR(err)

        roi <- getROI w

        orig <- cam
        let feats = interestPoints n h orig
            sel = map ip $ filter (inROI roi . ipRawPosition) feats

        (vs, prev) <- getW w
        when (null vs && not (null sel)) $ do
            putW w (sel,gray orig)

        let matches = basicMatches (vs, map ip $ feats) distFeat err

            ok = not (null vs) && not (null feats)

        inWin w $ do
            drawImage (rgb orig)
            lineWidth $= 1
            setColor 0.5 0.5 0.5
            mapM_ boxFeat feats
            setColor 0 0 0
            drawROI roi
            when ok $ do
                setColor 1 1 1
                text2D 20 20 $ printf "%d matches" (length matches)
                pointCoordinates sz
                drawInterestPoints sz (map snd matches)

        when ok $ inWin wm $ do
            let pair = blockImage [[prev,gray orig]]
            drawImage pair
            pointCoordinates (size pair)
            when (length matches > 0) $ do
                let Size h w = sz
                renderPrimitive Lines $ mapM_ drawPair' matches

--------------------------------------------------------

main = do
    with2 <- getFlag "--2"
    if with2
        then main2
        else main1

-----------------------------------------------------

distFeat = (distv `on` ipDescriptor)

prep = map (g.ipPosition) where g (Point x y) = [x,y]

drawPair (a,b) = vertex (f1 a) >> vertex (f2 b) where
    f1 [x,y] = Point (x/2+0.5) (y/2)
    f2 [x,y] = Point (x/2-0.5) (y/2)


drawPair' (a,b) = vertex (ipPosition $ desp1 a) >> vertex (ipPosition $ desp2 b)
    where
        f1 (Point x y) = Point (x/2+0.5) (y/2)
        f2 (Point x y) = Point (x/2-0.5) (y/2)
        desp1 x = x {ipPosition = f1 (ipPosition x)}
        desp2 x = x {ipPosition = f2 (ipPosition x)}

boxFeat p = do
    drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)


mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,_) <- get st
    st $= ([],undefined)
mouse def _ a b c d = def a b c d

distv a b = pnorm PNorm2 (a-b)


instance NFData InterestPoint where
     rnf = rnf . ipPosition

instance NFData Point where
     rnf (Point x y) = rnf x
