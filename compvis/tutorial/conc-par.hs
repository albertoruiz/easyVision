import EasyVision hiding (observe,run,camera)
import Graphics.UI.GLUT(postRedisplay)
import Control.Monad(liftM2)
import Control.Arrow((***))

monitor' name sz fun = do
    w <- evWindow () name sz (Just (const fun)) (const kbdQuit)
    return $ postRedisplay (Just (evW w))

observe winname f a = monitor' winname (mpSize 12) (a >>= f)

run n ws = sequence ws >>= launchFreq n . sequence_

camera k = findSize >>= getCam k ~> float . gray . channels

(.&.) = liftM2 (liftM2 (,))

async f = asyncFun 0 id f
infixl 1 -<
f -< n = asyncFun 0 f n

main = do
    prepare
    cams <- camera 0 .&. camera 1 >>= async

    feats <- g |***| g -< cams

    run 20 [ observe "cam1" (drawImage'.fst) cams
           , observe "cam2" (drawImage'.snd) cams
           , observe "f1" (sh.fst) feats
           , observe "f2" (sh.snd) feats
           ]

sigmas = (take 15 $ getSigmas 1 3)
fun img = (img, fullHessian (surf 2 2) sigmas 50 0.2 img)
g = fun

sh (img, feats) = do
    drawImage' img
    setColor 1 1 0
    text2D 20 20 (show $ length $ feats)
    mapM_ showFeat feats

showFeat p = do
    drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)
    let Pixel y x = ipRawPosition p
    drawVector x y (10*ipDescriptor (ip p))
