-- by now only automatic rotation of the text

import EasyVision
import Vision
import Numeric.LinearAlgebra
import Util.Rotation(rot3)

fftpow  = 8
fftl    = 2^8
fftsize = Size fftl fftl
sz = mpSize 20
p2roi = ROI r1 (r1 + fftl-1) c1 (c1 + fftl-1)
    where r1 = (height sz -fftl) `div` 2
          c1 = (width sz -fftl) `div` 2

up = modifyROI (\r -> r {r2 = r1 r + fftl `div` 2 - 1})

main = do
    (cam,ctrl) <- getCam 0 sz ~> channels >>= withPause

    prepare

    w <- evWindow () "image" sz Nothing  (const (kbdcam ctrl))
    f <- evWindow () "fft"  fftsize Nothing  (const kbdQuit)
    hz <- evWindow () "horizontal" sz Nothing  (const kbdQuit)


    fft <- genFFT 8 8 DivFwdByN AlgHintFast

    launch $ do
        img <- cam
        let imf = float . grayscale $ img
        inWin w $ do
            drawImage imf
            drawROI p2roi
        let d = powerSpectrum $ magnitudePack $ fft (modifyROI (const p2roi) imf)
            c@(Pixel r0 c0) = roiCenter (theROI d)
        set32f 0 (roiFrom2Pixels c c) d
        let (m,Pixel rm cm) = maxIndx (up d)
            ROI r1 _ c1 _ = p2roi
            dr = fromIntegral (rm+r1-r0)
            dc = fromIntegral (cm+c1-c0)
            alpha = atan2 (-dc) (-dr)
            nd = sqrt (dr*dr+dc*dc)
            sc = (1/m) .* d
        r <- image fftsize
        copy sc (theROI sc) r (theROI r)
        let h = scaling (nd/8) <> rot3 (-alpha)
        inWin hz $ drawImage (warp 0 sz h imf)
        inWin f $ do
            drawImage r
            text2D 20 20 (show (round nd,round (alpha*180/pi)))


