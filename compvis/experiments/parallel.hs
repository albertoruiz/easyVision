import EasyVision
import Control.Arrow
import Control.Monad
import Debug.Trace
import Numeric.LinearAlgebra hiding ((.*))

camera k = findSize >>= getCam k ~> channels
observe winname f = monitor winname (mpSize 20) f
run c = prepare >> (c >>= launch . (>> return ()))
(.&.) = liftM2 (liftM2 (,))

sigmas = (take 15 $ getSigmas 1 3)

fun img = (img, fullHessian (surf 2 2) sigmas 50 0.2 img)

--g = fun.float.gray

--g = fun2.fun1 3 0.1 30

g = gaussS 20 . gaussS 20 . gaussS 20 . float. resize (mpSize 40). gray

edges th = canny (th*2/3,th) . gradients

fun1 sigma thres szw = edges thres . gaussS sigma . float. resize (mpSize szw). gray

fun2 = distanceTransform [1,1.4,2.2] . notI

sh = drawImage

sh'  (img, feats) = do
    drawImage img
    setColor 1 1 0
    text2D 20 20 (show $ length $ feats)
    -- print $ (@>0) $ flatten $ fromRows $ map (ipDescriptor.ip) feats
    mapM_ showFeat feats

showFeat p = do
    drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)
    let Pixel y x = ipRawPosition p
    drawVector x y (10*ipDescriptor (ip p))

main1 = run $   camera 0
           >>= observe "img 0" (drawImage.rgb)
           ~>  g
           >>= observe "feat 0" sh

main2 op = run $ (camera 0 .&. camera 1)
          >>= observe "img 0" (drawImage.rgb.fst)
          >>= observe "img 1" (drawImage.rgb.snd)
          ~> g `op` g
          >>= observe "feats 0" (sh.fst)
          >>= observe "feats 1" (sh.snd)

main = do
    two <- getFlag "-2"
    one <- getFlag "-1"
    if one then main1 else if two then main2 (|***|) else main2 (***)
