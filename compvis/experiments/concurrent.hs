import EasyVision
import Control.Concurrent
import Control.Monad
import Graphics.UI.GLUT
import Debug.Trace
import Control.Parallel.Strategies
import ImagProc.Ipp.Core

observe winname f a = monitor' winname (mpSize 20) (readMVar a >>= f) where

run n ws = sequence ws >>= launchFreq n . sequence_

async cam = do
    c <- cam
    r <- c >>= newMVar
    forkIO $ forever $ c >>= swapMVar r
    return r

concF d n f = do
    v <- readMVar n >>= newMVar . f
    forkIO $ forever $ do threadDelay d
                          x <- readMVar n
                          let y = f x `using` rnf
                          y `seq` swapMVar v y
    return v

concF2 d f n1 n2 = do
    x1 <- readMVar n1
    x2 <- readMVar n2
    v <- newMVar (f x1 x2)
    forkIO $ forever $ do
        threadDelay d
        x1 <- readMVar n1
        x2 <- readMVar n2
        let y = (f x1 x2) `using` rnf
        y `seq` swapMVar v y
    return v



infixl 1 -<
(f,d) -< n = concF (1000*d) n f


monitor' name sz fun = do
    w <- evWindow () name sz (Just (const fun)) (const kbdQuit)
    return $ postRedisplay (Just (evW w))


main = do
    prepare
    cam <- async (findSize >>= getCam 0 ~> channels)

    feat <- (fun.float.gray, 50) -< cam

    feat2 <- (fun.float.resize (mpSize 10).gray, 50) -< cam

    x <- (float . gray, 100) -< cam

    a <- (gaussS (sqrt $ 10^2 + 10^2), 10) -< x

    b <- (gaussS 10 . gaussS 10, 20) -< x

    c <- concF2 1000 (\a b -> 10 .* (a |-| b)) a b

    run 20 [ observe "cam" (drawImage.rgb) cam
           , observe "feats1" sh feat
           , observe "feats2" sh feat2
           , observe "prop" drawImage c
           ]

fun img = (img, fullHessian (usurf 2 3) (take 15 $ getSigmas 1 3) 50 0.2 img)

boxFeat p = drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)

sh (img, feats) = do
    drawImage img
    lineWidth $= 1
    setColor 1 0 0
    text2D 20 20 (show $ length $ feats)
    mapM_ boxFeat feats


instance NFData ImageFloat where
    rnf x = rnf (x `fval` (Pixel 0 0))

instance NFData DetailedInterestPoint where
     rnf p = rnf (ipRawPosition p)

instance NFData Pixel where
     rnf (Pixel r c) = rnf r

monitor'' name sz fun = do
    let disp st = do
            fun
            k <- get st
            text2D 20 20 (show k)
            st $= k+1
    w <- evWindow 0 name sz (Just disp) (const kbdQuit)
    return $ postRedisplay (Just (evW w))