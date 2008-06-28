{-# LANGUAGE CPP #-}

-- ./hessian [--size=15] [url]
-- click to store points

import EasyVision
import Numeric.LinearAlgebra
import Graphics.UI.GLUT hiding (Size)
import Control.Monad(when)
import Data.List(minimumBy)
import Text.Printf(printf)

main = do
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
            feats = take tot $ fullHessian (descrip 2 4) sigmas 100 h imr
            sel = filter (inROI roi . ipRawPosition) feats

        (vs, prev) <- getW w
        when (null vs && not (null sel)) $ do
            putW w (sel,imr)

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

        when (not $ null vs) $ inWin wm $ do
            let pair = blockImage [[prev,imr]]
            drawImage pair
            --mapM_ boxFeat vs
            let Size h w = sz
                f (Pixel r c) = Pixel r (c+w)
                desp x = x {ipRawPosition = f (ipRawPosition x)}
                drawPair (a,b) = vertex (ipRawPosition b) >> vertex (ipRawPosition $ desp a)
            --mapM_ (boxFeat.desp) feats
            renderPrimitive Lines $ mapM_ drawPair (pairMatches err feats vs)



boxFeat p = do
    drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)


mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,_) <- get st
    st $= ([],undefined)
mouse def _ a b c d = def a b c d

distv a b = pnorm PNorm2 (a-b)

findMatches h as bs = filter f as where
    f v = minimum (map (g v) bs) < h
    g = distv `on` ipDescriptor.ip

pairMatches h as bs = [p | (d,p) <- map f as, d < h]
    where
    f a = (d,(a,b)) where b = minimumBy (compare `on` g a) bs
                          d = g a b
    g = distv `on` ipDescriptor.ip

drawPair (a,b) = vertex (ipRawPosition a) >> vertex (ipRawPosition b)
