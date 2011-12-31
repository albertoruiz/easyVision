-- moments of an image patch

import EasyVision
import Graphics.UI.GLUT hiding (Point)


main1 = run $ camera
    ~>  grayscale
    >>= monBasis
    >>= monMoments
    >>= monitorScanLine "scanLine" id
    >>= timeMonitor


main = run $ camera
    ~>  grayscale
    >>= regionMarker id
    >>= moniMask
    >>= timeMonitor

----------------------------------------------------------------------

shGrad = float . scale32f8u (-1) 1

basis = imageBasis (mpSize 20)

bxy = (xIb basis, yIb basis)



monBasis = monitorWheel (0,0,6) "basis" (mpSize 10) sh
  where
    sh k _ = drawImage' $ shGrad $ (f!!k) basis
    f = [zeroIb, oneIb, xIb, yIb, x2Ib, y2Ib, xyIb]


monMoments = selectROIfun "image moments" sel mon res
  where
    sel = id
    res = flip setROI
    mon x r = do
        let pre  = float . autoBinarize . notI
            m@(mx,my,_,_,_) = momentsImage basis . (pre . setROI r) $ x
            e = polyEllipseMoments m 30
        pointCoordinates (size $ sel x)
        pointSize $= 3
        setColor 1 0 0
        renderPrimitive Points $ vertex (Point mx my)
        renderPrimitive LineLoop (vertex e)

----------------------------------------------------------------------

moniMask = monitor "regionMask" (mpSize 20) sh
  where
    sh (im,p) = do
        let mask = maskPoly bxy p
            thing = binarize8u 128 ( notI im `andI` mask )
            m@(mx,my,_,_,_) = momentsImage basis (float thing)
            e = polyEllipseMoments m 30
        drawImage' im
        pointCoordinates (size thing)
        pointSize $= 3
        setColor 1 0 0
        renderPrimitive Points $ vertex (Point mx my)
        renderPrimitive LineLoop (vertex e) 

----------------------------------------------------------------------


