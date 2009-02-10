import EasyVision
import Control.Arrow
import Control.Monad
import Debug.Trace
import Numeric.LinearAlgebra hiding ((.*))
import Tutorial(run)

camera k = findSize >>= getCam k ~> channels
observe winname f = monitor winname (mpSize 20) f
(.&.) = liftM2 (liftM2 (,))

sigmas = (take 15 $ getSigmas 1 3)

fun img = (img, fullHessian (surf 2 2) sigmas 50 0.2 img)

g = fun.float.gray

sh  (img, feats) = do
    drawImage img
    setColor 1 1 0
    text2D 20 20 (show $ length $ feats)
    mapM_ showFeat feats

showFeat p = do
    drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)
    let Pixel y x = ipRawPosition p
    drawVector x y (10*ipDescriptor (ip p))


main' op = run $ (camera 0 .&. camera 1)
          >>= observe "img 0" (drawImage.rgb.fst)
          >>= observe "img 1" (drawImage.rgb.snd)
          ~> g `op` g
          >>= observe "feats 0" (sh.fst)
          >>= observe "feats 1" (sh.snd)

main = do
    two <- getFlag "-2"
    if two then main' (|***|) else main' (***)
