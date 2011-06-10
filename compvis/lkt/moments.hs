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
import GHC.Float(float2Double,double2Float)
import Data.Colour.Names
import Util.Ellipses
import Vision(estimateHomographyRaw,cross)
import Util.Kalman(unscentedTransform,ukfDefaultParam)
import Data.List(foldl1')

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

main1 = run $ camera
    ~>  grayscale
    >>= monMoments
    >>= monitorScanLine "scanLine" id
    >>= timeMonitor

main2 = run $ camera
    ~>  grayscale
    >>= regionMarker (mpSize 20) id
    ~>  fun
    >>= moni
    >>= timeMonitor

main = run $ camera
    ~>  grayscale
    >>= regionMarker (mpSize 20) id
    >>= moniMask
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

----------------------------------------------------------------------

-- must add h to rectifyRegion, remove input, and add window title!
transOfRectif im pol = t
  where
    t = estimateHomographyRaw [[s,r],[-s,r],[-s,-r],[s,-r]] (g pol)
    g (Closed ps) = map (\(Point x y) -> [x,y]) ps
    (Size h w) = size im
    r = fromIntegral h / fromIntegral w
    s = 1 --640 / fromIntegral w
    
fun (im,pol) = (im,e)
  where
    r = snd $ rectifyRegion id 640 defAspectRatioParam (im,pol)
    h = inv (transOfRectif r pol) 
    m@(mx,my,cxx,cyy,cxy) = momentsImage basis . float .autoBinarize . notI $ r
    med = fromList [mx,my]
    cov = (2><2) [cxx,cxy,cxy,cyy]
    wf v = fromList w
      where
        [w] = ht h [toList v]
    (med', cov') = unscentedTransform ukfDefaultParam wf (med,cov)
    m' = (mx',my',cxx',cyy',cxy')
    [mx',my'] = toList med'
    [cxx',cxy',_,cyy'] = toList $ flatten cov'
    e = polyEllipseMoments m' 30

moni = monitor "whoa" (mpSize 20) sh
  where
    sh (im,p) = do
       drawImage' im
       pointCoordinates (size im)
       setColor' yellow
       renderPrimitive LineLoop (vertex p)

----------------------------------------------------------------------

p2hv (Point x y) = vec[x,y,1]

moniMask = monitor "regionMask" (mpSize 20) sh
  where
    sh (im,p) = do
        let f = float (notI im)
            ss = asSegments p
            roi = poly2roi (size im) p
            b = (setROI roi (xIb basis), setROI roi (yIb basis))
            mask = foldl1' (|*|) (map (maskDir b) ss)
            thing = binarize8u 128 $ toGray ( f |*| mask )
            m@(mx,my,_,_,_) = momentsImage basis (float thing)
            e = polyEllipseMoments m 30
        drawImage' thing
        pointCoordinates (size thing)
        pointSize $= 3
        setColor 1 0 0
        renderPrimitive Points $ vertex (Point mx my)
        renderPrimitive LineLoop (vertex e) 
            
maskDir (xI,yI) (Segment p0 p1) = mask
  where
    [a,b,c] = toList $ single $ cross (p2hv p0) (p2hv p1)
    mask = thresholdVal32f (-c) 0 IppCmpLess . thresholdVal32f (-c) 1 IppCmpGreater $ a .* xI |+| b .* yI

