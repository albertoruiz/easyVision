import EasyVision hiding ((.*))
import qualified EasyVision as I
import Data.List
import Data.Function
import Control.Arrow
import Vision
import Numeric.LinearAlgebra
import GHC.Float
import ImagProc.Ipp.Core
import Graphics.UI.GLUT hiding (Size,scale)

f .***. g = uncurry zip . (f *** g) . unzip

-- rois = [ ROI 100 380 100 540]

-- rois = [ ROI 200 280 100 540,
--          ROI 100 380 280 360 ]

range = 15

regs (Size h w) = 
    [ ROI dh (6*dh) c (c+dw) | c <- [dw,3*dw .. 5*dw] ]
    ++
    [ ROI r (r+dh) dw (6*dw) | r <- [dh,3*dh .. 5*dh] ]
  where dh = h `div` 7
        dw = w `div` 7

main = do
    sz <- findSize
    run $ camera ~> grayscale -- ~> float >>= drift 0.5 ~> toGray
--        >>= monitorROI "orig" sz pruprof
      ~>  profiles (regs $ mpSize 20)
--      >>= monitor "profiles" (mpSize 5) sh
      ~~> map (onlylong . prudir) . history 2
      >>= monitor "align" (mpSize 5) shal
      ~> id *** (sum . map (\(_,_,_,x,_)->x))
      ~~> id .***. scanl1 (+)
      >>= monitor "acum" (mpSize 5) shacum

profiles rois img = (img, zip rois profs) where
    profs = map (profile int) rois
    int = integral img

----------------------------------------------------

sumROI int (ROI r1 r2 c1 c2) = float2Double (a+b-c-d) where
    a = fval int (Pixel r2 c2)
    b = fval int (Pixel r1 c1)
    c = fval int (Pixel r2 c1)
    d = fval int (Pixel r1 c2)

profile int (ROI r1 r2 c1 c2) = (vert,horiz) where
    v1 = vec $ sampleLine32f int (Pixel r1 c1) (Pixel r1 c2)
    v2 = vec $ sampleLine32f int (Pixel r2 c1) (Pixel r2 c2)
    h1 = vec $ sampleLine32f int (Pixel r1 c1) (Pixel r2 c1)
    h2 = vec $ sampleLine32f int (Pixel r1 c2) (Pixel r2 c2)
    v = v2 - v1
    h = h2 - h1
    vert = diffs v
    horiz = diffs h

vec = fromList . map float2Double

diffs v = subVector 1 (n-1) v - subVector 0 (n-1) v
    where n = dim v

----------------------------------------------------

setROI roi img = (act,res) where
    res = modifyROI (const roi) img
    act = drawImage' res

passROI roi img = (act,res) where
    res = (img,roi)
    act = drawImage' img >> drawROI roi

pruprof roi img = (act,res) where
    res = (img,(v,h))
    (v,h) = profile (integral img) roi
    act = do
        drawImage' img
        drawProfile roi (v,h)

-------------------------------------

drawProfile roi (v,h) = do
    let ROI r1 r2 c1 c2 = roi
    drawROI roi
    drawVectorH (Pixel r2 (c1+1)) (fromIntegral (c2-c1)/255/fromIntegral (dim v) `scale` v)
    drawVectorV (Pixel (r1+1) c1) (fromIntegral (r2-r1)/255/fromIntegral (dim h) `scale` h)


drawAlig (roi,vp,hp, v,h) = do
    let ROI r1 r2 c1 c2 = roi
        rm = (r1+r2) `div` 2
        cm = (c1+c2) `div` 2
    setColor 1 1 1
    lineWidth $= 1
    drawProfile roi (vp,hp)
    drawROI (ROI r1 r2 c1 ((c1+c2)`div`2 + 3*v))
    drawROI (ROI r1 ((r1+r2)`div`2 + 3*h) c1 c2)
    text2D (fromIntegral cm) (fromIntegral rm) $ show (v,h)
    setColor 1 0 0
    lineWidth $= 3
    renderPrimitive Lines $ vertex (Pixel rm cm) >> vertex (Pixel (rm+3*h) (cm+3*v))

--    drawVectorH (Pixel r2 (c1+1)) (fromIntegral (c2-c1)/255/fromIntegral (dim v) .* v)
--    drawVectorV (Pixel (r1+1) c1) (fromIntegral (r2-r1)/255/fromIntegral (dim h) .* h)
--    print (v,h)


monitorROI name sz fun cam = do
    (cam', ctrl) <- withPause cam
    w <- evWindow () name sz Nothing (const (kbdcam ctrl))
    let d = 10
    evROI w $= ROI d (height sz-d) d (width sz-d)
    return $ do
        thing <- cam'
        roi <- getROI w
        let (act,res) = fun roi thing
        inWin w act
        return res

----------------------------------------------------------

drawVectorH (Pixel r c) v = do
    renderPrimitive LineStrip $ do
        flip mapM_ [0..dim v -1] $ \ col ->
            vertex (Pixel (r-round (v@>col)) (c+col))

drawVectorV (Pixel r c) v = do
    renderPrimitive LineStrip $ do
        flip mapM_ [0..dim v -1] $ \ row ->
            vertex (Pixel (r+row) (c+round (v@>row)))

-------------------------------------------------------------

sh (img, profs) = do
    drawImage' img
    mapM_ (uncurry drawProfile) profs

shal (img, alis) = do
    drawImage' img
    mapM_ drawAlig alis

shacum (img,vac) = do
    drawImage' img
    text2D 30 30 (show $ round $ ( fromIntegral vac / 5600*180 :: Double) )
--------------------------------------------------------------

bestAlign d u v = k-d where
    l = dim u - 2*d
    sub p x = id $ subVector p l x
    corr j = norm (sub d u - sub (d-j) v)
    norm = pnorm PNorm1
    k = posMin (map corr [-d .. d])
    posMin l = k where Just k = elemIndex (minimum l) l

---------------------------------------------------------------

prudir [(im1,l1),(im2,l2)] = (im2, zipWith f l1 l2) where
    f (r,(v1,h1)) (_,(v2,h2)) = (r, v2, h2, bestAlign range v1 v2, bestAlign range h1 h2)

onlylong (im, xs) = (im, map f xs) where
    f (roi@(ROI r1 r2 w1 w2), v2, h2, v ,h) =
        if r2 - r1 > w2 - w1
            then (roi,v2,h2,0,h)
            else (roi,v2,h2,v,0)

-------------------------------------------------------------

history k = map (reverse . take k) . tails

-------------------------------------------------------------

drift alpha = virtualCamera drifter
    where drifter (a:b:rest) = a : drifter ((alpha I..* a |+| (1-alpha)I..* b):rest)

--------------------------------------------------------------

-- acum xs = zip ims acs
--     (ims, ys) = unzip xs
--     z = map (\()->v)
