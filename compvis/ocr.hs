-- by now only automatic rotation of the text

import EasyVision
import Vision
import Numeric.LinearAlgebra

fftpow  = 8
fftl    = 2^8
fftsize = Size fftl fftl
sz = mpSize 20
p2roi = ROI r1 (r1 + fftl-1) c1 (c1 + fftl-1)
    where r1 = (height sz -fftl) `div` 2
          c1 = (width sz -fftl) `div` 2

main = do
    (cam,ctrl) <- getCam 0 sz >>= withChannels >>= withPause

    prepare

    w <- evWindow () "image" sz Nothing  (const (kbdcam ctrl))
    f <- evWindow () "fft"  fftsize Nothing  (const kbdQuit)
    hz <- evWindow () "horizontal" sz Nothing  (const kbdQuit)


    fft <- genFFT 8 8 DivFwdByN AlgHintFast

    launch $ do
        img <- cam
        let imf = float . gray $ img
        inWin w $ drawImage (modifyROI (const p2roi) imf)
        d <- fft (modifyROI (const p2roi) imf) >>= magnitudePack >>= powerSpectrum
        let c@(Pixel r0 c0) = cent (theROI d)
        set32f 0 (roiFrom2Pixels c c) d
        let up = modifyROI (\r -> r {r2 = r1 r + fftl `div` 2-1})
        (m,Pixel rm cm) <- maxIndx (up d)
        let ROI r1 _ c1 _ = p2roi
        let dr = fromIntegral (rm+r1-r0)
            dc = fromIntegral (cm+c1-c0)
            alpha = atan2 (-dc) (-dr)
            nd = sqrt (dr*dr+dc*dc)
        sc <- scale32f (1/m) d
        r <- image fftsize
        copy sc (theROI sc) r (theROI r)
        let h = scaling (nd/8) <> rot3 (-alpha)
        inWin hz $ drawImage (warp 0 sz h imf)
        inWin f $ do
            drawImage r
            text2D 20 20 (show (round nd,round (alpha*180/pi)))

cent (ROI r1 r2 c1 c2) = Pixel (r1 + (r2-r1+1)`div`2) (c1 + (c2-c1+1)`div`2)
roiFrom2Pixels (Pixel r1 c1) (Pixel r2 c2) = ROI (min r1 r2) (max r1 r2) (min c1 c2) (max c1 c2)
