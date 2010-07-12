{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}
import ImagProc.C.Corners
import EasyVision
import Graphics.UI.GLUT hiding (Size,minmax)

$(autoParam "Param" "param"
    [("th1","Int",intParam 200 0 255),
     ("th2","Int",intParam 50 0 255)]
 )

main = run $ (camera ~> gray ~> ((!!0) . pyr) .&. param) ~> pru
             >>= monitor "Image" (mpSize 20) f
             >>= zoomWindow "Zoom" 600 (autoscale.float.fst.snd)
             >>= timeMonitor


guess x = dilate3x3 $ highPass8u Mask5x5 x `orI` highPass8u Mask5x5 (notI x)
smooth = gauss8u Mask5x5

pru (x,Param{..}) = (x,(h,pts))
    where (h,pts) = prucor th1 th2 1000 (guess x) (smooth x)


f (x,(h,pts)) = do
    drawImage x
    pixelCoordinates (size x)
    pointSize $= 3
    setColor 1 0 0
    renderPrimitive Points $ mapM_ (vertex.snd) pts


pyr = iterate dec

dec x = modifyROI (shrink (2,2))$ resize8u InterpNN (Size (h `div` 2) (w `div` 2)) (modifyROI (const $ theROI x) $ gauss8u Mask5x5 x)
    where (Size h w) = size x

autoscale im = f im
    where (mn,mx) = minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx
