import EasyVision
import Graphics.UI.GLUT(postRedisplay)

monitor' name sz fun = do
    w <- evWindow () name sz (Just (const fun)) (const kbdQuit)
    return $ postRedisplay (Just (evW w))

observe winname f a = monitor' winname (mpSize 20) (a >>= f)

run n ws = sequence ws >>= launchFreq n . sequence_

camera k = findSize >>= getCam k >>= async ~> channels

async f = asyncFun 0 id f
infixl 1 -<
f -< n = asyncFun 0 f n

main = do
    prepare
    cam1 <- camera 0
    cam2 <- camera 1

    feat1 <- fun . float . gray -< cam1
    feat2 <- fun . float . gray -< cam2

    run 20 [ observe "cam1" (drawImage.rgb) cam1
           , observe "cam2" (drawImage.rgb) cam2
           , observe "f1" sh feat1
           , observe "f2" sh feat2
           ]

sigmas = (take 15 $ getSigmas 1 3)

fun img = (img, fullHessian (surf 2 2) sigmas 50 0.2 img)

sh (img, feats) = do
    drawImage img
    setColor 1 1 0
    text2D 20 20 (show $ length $ feats)
    mapM_ showFeat feats

showFeat p = do
    drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)
--    let Pixel y x = ipRawPosition p
--    drawVector x y (10*ipDescriptor (ip p))
