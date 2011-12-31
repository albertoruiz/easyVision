-- experiments with Lucas-Kanade Tracker
-- use in compiled mode with -O

import EasyVision hiding (setROI)
import Graphics.UI.GLUT hiding (Point,Size,scale,samples,Matrix)
import Numeric.LinearAlgebra as LA hiding (i)
import Util.Misc(diagl,debug,vec,Mat,degree)
import Control.Applicative
import Control.Arrow
import Vision(desp, scaling, cameraFromHomogZ0, ht)
import Util.Rotation(rot3)
import Util.Optimize(optimize)
import Text.Printf(printf)

disp = putStrLn . dispf 5

shGrad = float . scale32f8u (-1) 1

shcont c@(Closed _) = renderPrimitive LineLoop (vertex c)

shcont c@(Open _) = renderPrimitive LineStrip (vertex c)

----------------------------------------------------------------------

ximg = mat2img $ single $ fromRows (replicate 480 xcoord)
  where xcoord = linspace 640 (1,-1::Double)
yimg = mat2img $ single $ fromColumns (replicate 640 ycoord)
  where ycoord = linspace 480 (0.75,-0.75::Double)
zimg = mat2img $ konst 0 (480,640)
uimg = mat2img $ konst 1 (480,640)

gf = recip 2.504e-2

hessianI ims = fromLists [[sum32f (a|*|b) | a <- ims] | b <- ims]

sdpar err sds = fromList [sum32f (err|*|a) | a <- sds]

-- affine

sdiA dx dy = [ dx |*| ximg, dy |*| ximg, dx |*| yimg, dy|*| yimg, dx, dy] 

mktA [a,b,c,d,e,f] = (3><3) [1+a,c,   e,
                             b  ,1+d, f,
                             0  ,  0, 1]


-- projective

sdiP dx dy = [ dx |*| ximg, dy |*| ximg, dx |*| yimg, dy|*| yimg, dx, dy, dx|*|wxg |+| dy|*|wyg, dx|*|wxh |+| dy|*|wyh]
  where
    wxg = (-1).* ximg |*| ximg 
    wxh = (-1).* ximg |*| yimg
    wyg = wxh
    wyh = (-1).* yimg |*| yimg

mktP [a,b,c,d,e,f,g,h] = (3><3) [1+a,c,   e,
                                 b  ,1+d, f,
                                 g  ,  h, 1]

-- only displacement

sdiD dx dy = [ dx, dy] 

mktD [e,f] = (3><3) [1,0, e,
                     0,1, f,
                     0,0, 1]


-- displacement and scale

sdiS dx dy = [ dx |*| ximg |+| dy |*| yimg , dx, dy] 

mktS [s,e,f] = (3><3) [1+s,0, e,
                       0,1+s, f,
                       0,  0, 1]


-- displacement and rotation

sdiR dx dy = [ dx |*| ((-1) .* yimg) |+| dy |*| ximg , dx, dy] 

mktR [a,e,f] = (3><3) [cos a, -sin a, e,
                       sin a,  cos a, f,
                           0,      0, 1]

mkt = mktR
sdi = sdiR


lkStep t = f
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
        h' = (mkt.toList $ dps) <> h


rms img = k * (sum32f . abs32f) img
  where
    k = 255 / fromIntegral (roiArea (theROI img))


----------------------------------------------------------------------

tracker (_, Nothing) x = x                  -- no template
tracker x (img,Nothing) = (img,Nothing)     -- no template

tracker (_, Just (t,(k,h,e,ha))) (img, _)
    | e < 50    = okTrack img t k h e       -- normal tracking
    | otherwise = newTemp img t h           -- update template !?

okTrack img t k h e = (img, Just (t,(k,h',e',h)))
  where
    (h',e') = fst $ kltv h e k img

newTemp img t h = (img, Just (t',(k,h, 0,h)))
  where
    t' = modifyROI (const (theROI t)) $ warpTo (theROI t) h img
    k = lkStep t'

kltv h e k i = optimize 0 0.5 10 (k i) snd (h,e)

----------------------------------------------------------------------

main = testTracker

testTracker = run $ camera ~> float.grayscale
                  >>= selectROI "select region" id
                  >>= getTemplate ~> (fst *** id)
                  ~~> scanl1 tracker
                  >>= trackerMon
                  >>= trackerMon2
                  >>= timeMonitor
                  
trackerMon = monitorWheel (0,0,2) "Tracker" (mpSize 10) sh
  where
    sh _ (img, Nothing) = do
        drawImage' (img :: ImageFloat)
    sh 0 (img, Just (t,(_,h,e,ha))) = do
        let ih = inv h
        drawImage' img
        pointCoordinates (size t)
--        setColor 1 0.5 0.5
--        shcont $ transPol (inv ha) $ roi2poly (size t) (theROI t)
        setColor 1 1 1
        shcont $ transPol ih $ roi2poly (size t) (theROI t)        
        setColor 1 0 0
        text2D 0.9 0.65 (printf "%.1f" e)
    sh 1 (img, Just (t,(_,h,e,_))) = do
        let ih = inv h
        drawImage' (warpOn img ih t)
        pointCoordinates (size t)
        setColor 1 0 0
        text2D 0.9 0.65 (printf "%.1f" e)
    sh 2 (img, Just (t,(_,h,e,_))) = do
        let ih = inv h
        --clear [DepthBuffer]
        drawImage' img
        --clear [DepthBuffer]
        let Just cam = cameraFromHomogZ0 (Just 1.7) ih
        pointCoordinates (size t)
        setColor 1 0 0
        renderPrimitive LineStrip $ mapM_ vertex $ ht cam [[0,0,0],[0.4,0,0],[0,0,0],[0,0.4,0],[0,0,0],[0,0,0.4]] 


trackerMon2 = monitor "Tracker Error" (mpSize 10) sh
  where
    sh (img, Nothing) = return ()
    sh (img, Just (t,(_,h,e,_))) = do
        drawImage' $ shGrad $  t |-| warpTo (theROI t) h img

getTemplate = clickStatusWindow "getTemplate" (mpSize 10) Nothing update display action
  where
    update _ (Just _) = Nothing
    update (x,roi) Nothing = Just (t, (lkStep t,ident 3, 0,ident 3))
      where t = modifyROI (const roi) x
    display (x,roi) Nothing = drawImage' (modifyROI (const roi) x)
    display _ (Just (x,_)) = drawImage' x >> setColor 1 0 0 >> drawROI (theROI x)
    action _ _ = return ()

----------------------------------------------------------------------
------- tests --------------------------------------------------------

showImags xs = prepare >> proc >> mainLoop
  where proc = imagesBrowser "Images" (mpSize 10) (zip xs (map show [1..]))

setROI = modifyROI (const (roiFromPixel 80 (Pixel 200 250))) 


test = do
    x' <- float . grayscale . channelsFromRGB <$> loadRGB "hz.png"
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
    print $ lkStepF t i (constant 0 6)
    showImags $ [t,i, shGrad (gx g), shGrad (gy g), wi, shGrad wix, shGrad wiy, shGrad e]
              ++ map (shGrad.(sdis!!)) [0..5]
    
h0 = diagl[1.05,1,1] <> desp (15*2/640, -10*2/640)

lkStepF t i' ps = ps'
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
    ps' = ps - dps

conver = do
    x' <- float . grayscale . channelsFromRGB <$> loadRGB "hz.png"
    let t = setROI x'
        i = warp 0 (size x') h0 x'
        hs = map (double . mkt . toList) $ iterate (lkStepF t i) (constant 0 6)
        ss = take 30 $ map (\h->warp 0 (size i) h i) hs
    disp (inv h0)
    disp $ hs!!30
    showImags $ [x',i,setROI i, t] ++ map setROI ss


conver3 = do
    x' <- float . grayscale . channelsFromRGB <$> loadRGB "hz.png"
    let t = setROI x'
        klt = lkStep t
        i = warp 0 (size x') h0 x'
        hs = map (double.fst . debug "e" snd) $ iterate (klt i) (ident 3, 0)
        ss = take 30 $ map (\h->warp 0 (size i) h i) hs
    disp (inv h0)
    --disp $ hs!!30    
    showImags $ [x',i,setROI i, t] ++ map (shGrad .(|-|t)) ss

