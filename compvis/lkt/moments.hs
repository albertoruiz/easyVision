{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

-- moments of an image patch

import EasyVision
import Graphics.UI.GLUT hiding (Point,Size,scale,samples,Matrix)
import Numeric.LinearAlgebra as LA hiding (i,(.*))
import Util.Misc(diagl,debug,vec,Mat,degree)
import Control.Applicative
import Control.Arrow
import Control.Monad(when)
import Vision(desp, scaling, cameraFromHomogZ0, ht)
import Util.Rotation(rot3)
import Util.Optimize(optimize)
import Text.Printf(printf)
import GHC.Float(float2Double)
import Data.Colour.Names
import Util.Ellipses

disp = putStrLn . dispf 5

shGrad = float . scale32f8u (-1) 1

setROI :: Image a => ROI -> a -> a
setROI = modifyROI . const

shcont c@(Closed _) = renderPrimitive LineLoop (vertex c)

shcont c@(Open _) = renderPrimitive LineStrip (vertex c)

----------------------------------------------------------------------

data ImageBasis = ImageBasis { zeroIb, oneIb, xIb, yIb, x2Ib, y2Ib, xyIb :: ImageFloat }

imageBasis :: Size -> ImageBasis
imageBasis (Size r c) = ImageBasis {
    zeroIb = mat2img $ konst 0 (r,c),
    oneIb = mat2img $ konst 1 (r,c),
    xIb = xI,
    yIb = yI,
    x2Ib = xI |*| xI,
    y2Ib = yI |*| yI,
    xyIb = xI |*| yI }
  where
    xcoord = linspace c (1,-1::Double)
    ycoord = linspace r (0.75,-0.75::Double)
    xI = mat2img $ single $ fromRows (replicate r xcoord)
    yI = mat2img $ single $ fromColumns (replicate c ycoord)

-- | (mx,my,cxx,cyy,cxy)
momentsImage :: ImageBasis -> ImageFloat -> (Double,Double,Double,Double,Double)
momentsImage ImageBasis {..} g = (mx,my,cxx,cyy,cxy)
  where
    s = sum32f g
    sx = sum32f (g |*| xIb)
    sy = sum32f (g |*| yIb)
    sx2 = sum32f (g |*| x2Ib)
    sy2 = sum32f (g |*| y2Ib)
    sxy = sum32f (g |*| xyIb)
    mx = sx/s
    my = sy/s
    cxx = sx2/s - mx*mx
    cyy = sy2/s - my*my
    cxy = sxy/s - mx*my


rms img = k * (sum32f . abs32f) img
  where
    k = 255 / fromIntegral (roiArea (theROI img))

autoBinarize x = binarize8u (otsuThreshold x) x

----------------------------------------------------------------------

basis = imageBasis (mpSize 20)

main = run $ camera
    ~>  grayscale
    >>= monMoments
    >>= monitorScanLine "scanLine" id
    >>= timeMonitor

monBasis = monitorWheel (0,0,6) "basis" (mpSize 10) sh
  where
    sh k (_,roi) = drawImage' $ shGrad $ setROI roi $(f!!k) basis
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

polyEllipseMoments (mx,my,cxx,cyy,cxy) n = Closed . conicPoints 30 $ e
  where
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    e = InfoEllipse {
            conicCenter = (mx,my), 
            conicSizes = (2*sqrt l1, 2*sqrt l2), 
            conicAngle = a,
            conicMatrix = undefined, 
            conicTrans = undefined }

