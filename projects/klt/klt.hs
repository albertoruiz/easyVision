-- experiments with Lucas Kanade Tracker

import EasyVision
import Graphics.UI.GLUT hiding (Point,Size,scale,samples,Matrix)
import Numeric.LinearAlgebra as LA hiding (i,(.*))
import Util.Misc(diagl,debug,vec,Mat,degree)
import Control.Applicative
import Control.Arrow
import Vision(desp, scaling)
import Tools
import Util.Rotation(rot3)
import Util.Optimize(optimize)

disp = putStrLn . dispf 5

showImags xs = prepare >> proc >> mainLoop
  where proc = imagesBrowser "Images" (mpSize 10) (zip xs (map show [1..]))

setROI = modifyROI (const (roiFromPixel 80 (Pixel 200 250))) 

xcoord = linspace 640 (1,-1::Double)
ximg = mat2img $ single $ fromRows (replicate 480 xcoord)
ycoord = linspace 480 (0.75,-0.75::Double)
yimg = mat2img $ single $ fromColumns (replicate 640 ycoord)
zimg = mat2img $ (480><640) [0..]
uimg = mat2img $ (480><640) [1..]

gf = recip 2.504e-2

sdi dx dy = [ dx |*| ximg, dy |*| ximg, dx |*| yimg, dy|*| yimg, dx, dy] 

hessianI ims = fromLists [[sum32f (a|*|b) | a <- ims] | b <- ims]

sdpar err sds = fromList [sum32f (err|*|a) | a <- sds]

test = do
    x' <- float . gray . channelsFromRGB <$> loadRGB "hz.png"
    --x' <- loadRGB "hz.png"
    let t = setROI x'
        i = warp 0 (size x') (desp (5/640, -5/640)) x'
        g = gradients i
        wi = setROI i
        wix = setROI (gx g)
        wiy = setROI (gy g)
        e = t |-| wi
        sdis = sdi wix wiy
        sdpars = sdpar e sdis
--    print $ inv $ hessianI sdis
--    print $ sdpar e sdis
    print $ inv (hessianI sdis) <> sdpars
    print $ kltStep t i (constant 0 6)
    showImags $ [t,i, shGrad (gx g), shGrad (gy g), wi, shGrad wix, shGrad wiy, shGrad e]
              ++ map (shGrad.(sdis!!)) [0..5]
    
shGrad = float . scale32f8u (-1) 1

mkt [a,b,c,d,e,f] = (3><3) [1+a,c,   e,
                            b  ,1+d, f,
                            0  ,  0, 1]

kltStep t i' ps = ps'
  where
    h = (mkt.toList) ps
    i = warp 0 (size i') h i'
    g = gradients i
    sr = modifyROI (const (theROI t))
    wi = sr i
    wix = gf    .* sr (gx g)
    wiy = (-gf) .* sr (gy g)
    e = t |-| wi
    sdis = sdi wix wiy
    sdpars = sdpar e sdis
    dps = inv (hessianI sdis) <> sdpars
    ps' = {-debug "ps" id $ -} ps - dps


klticStep t = f
  where
    g = gradients t
    tx = gf    .* gx g
    ty = (-gf) .* gy g
    sdis = sdi tx ty
    ih = debug "H" id $ inv (hessianI sdis)
    f i' (h,_) = (h',err)
      where        
        i = warpTo (theROI t) h i'
        e = i |-| t
        err = rms e
        sdpars = sdpar e sdis
        dps = ih <> sdpars
        h' = (mkt.toList$ dps) <> h




-- to be replaced by a new warpBack function

rms img = k * (sum32f . abs32f) img
  where
    k = 255 / fromIntegral (roiArea (theROI img))



h0 = diagl[1.05,1,1] <> desp (15*2/640, -10*2/640)
--h0 = diagl[1,1,1] <> desp (1*2/640, -1*2/640)

fixed = do
    x' <- float . gray . channelsFromRGB <$> loadRGB "hz.png"
    let t = setROI x'
        i = warp 0 (size x') h0 x'
        h = double . mkt . toList $ (!!10) $ iterate (kltStep t i) (constant 0 6)
        s = warp 0 (size i) h i
    disp h0
    disp h    
    showImags [x',i,setROI i, t,setROI s]
    
main = testTracker
    
    
conver = do
    x' <- float . gray . channelsFromRGB <$> loadRGB "hz.png"
    let t = setROI x'
        i = warp 0 (size x') h0 x'
        hs = map (double . mkt . toList) $ iterate (kltStep t i) (constant 0 6)
        ss = take 30 $ map (\h->warp 0 (size i) h i) hs
    disp (inv h0)
    disp $ hs!!30
    showImags $ [x',i,setROI i, t] ++ map setROI ss


conver3 = do
    x' <- float . gray . channelsFromRGB <$> loadRGB "hz.png"
    let t = setROI x'
        klt = klticStep t
        i = warp 0 (size x') h0 x'
        hs = map (double.fst . debug "e" snd) $ iterate (klt i) (ident 3, 0)
        ss = take 30 $ map (\h->warp 0 (size i) h i) hs
    disp (inv h0)
    --disp $ hs!!30    
    showImags $ [x',i,setROI i, t] ++ map (shGrad .(|-|t)) ss

----------------------------------------------------------------------

warpTo droi h img = warp zeroP sz h x
  where
    sz = size img
    sroi = transformROI sz (inv h) droi
    x = modifyROI (const sroi) img
    
bounding p = Closed [Point x2 y2, Point x1 y2, Point x1 y1, Point x2 y1] 
  where
    x1 = minimum xs
    x2 = maximum xs
    y1 = minimum ys
    y2 = maximum ys
    xs = map px (polyPts p)
    ys = map py (polyPts p)

roi2poly sz (ROI r1 r2 c1 c2) = Closed $ pixelsToPoints sz p
  where
    p = [Pixel r1 c1, Pixel r1 c2, Pixel r2 c2, Pixel r2 c1]

poly2roi sz p = ROI r1 r2 c1 c2
  where
    (Closed [p1,_,p3,_]) = bounding p
    [Pixel r1 c1, Pixel r2 c2] = pointsToPixels sz [p1,p3]

transformROI sz h = shrink (-1,-1) . poly2roi sz . transPol h . roi2poly sz

----------------------------------------------------------------------

dbg msg x = debug msg (const "") x

pseudotrack _ (img, Nothing) = (img, Nothing)
pseudotrack (_, Nothing) (img, j@(Just _)) = (img, j)
pseudotrack (_, Just (t,(k,h,e))) (img, j) | e < 20 = tk img t k h e
                                           | otherwise = (img,j)

tk img t k h e = (img, Just (t,(k,h',e')))
  where (h',e') = fst $ kltv h e k img

kltv h e k i = optimize 0 0.5 10 (k i) snd (h,e)

testTracker = run $ camera ~> float.gray
                  >>= selectROI "select region" id
                  >>= getTemplate ~> (fst *** id)
                  ~~> scanl1 pseudotrack
                  >>= trackerMon
                  >>= trackerMon2
                  >>= timeMonitor
                  
trackerMon = monitor "Tracker" (mpSize 10) sh
  where
    sh (img, Nothing) = do
        drawImage' (img :: ImageFloat)
    sh (img, Just (t,(_,h,e))) = do
        drawImage' img
        pointCoordinates (size t)
        shcont $ transPol (inv h) $ roi2poly (size t) (theROI t)
        text2D 0.9 0.7 (show $ round $ e*10)

trackerMon2 = monitor "Tracker Error" (mpSize 10) sh
  where
    sh (img, Nothing) = return ()
    sh (img, Just (t,(_,h,e))) = do
        drawImage' $ shGrad $  t |-| warpTo (theROI t) h img

getTemplate = clickStatusWindow "getTemplate" (mpSize 10) Nothing update display action
  where
    update _ (Just _) = Nothing
    update (x,roi) Nothing = Just (t, (klticStep t,ident 3 :: Mat, 0))
      where t = modifyROI (const roi) x
    display (x,roi) Nothing = drawImage' (modifyROI (const roi) x)
    display _ (Just (x,_)) = drawImage' x >> setColor 1 0 0 >> drawROI (theROI x)
    action _ _ = return ()

