-- affine alignment of contours
-- ./contours tv://

import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign
import Numeric.LinearAlgebra
import Numeric.GSL
import Vision
import Data.List(minimumBy, maximumBy)
import Control.Monad(guard)
import Data.Array

import ImagProc.Ipp.Core
import Debug.Trace

debug x = trace (show x) x

------------------------------------------------------------

easyInvar w f = fromList desc where
    sc = g 1
    g k = magnitude (f k) + magnitude (f (-k))
    h k = g k/sc
    desc = map h [2..(max 2 w)]

norm x = pnorm PNorm2 x

similarTo k eps (f,_,_) (g,_,_) = norm (easyInvar k f - easyInvar k g) < eps

prec = degree/10
nmax = 50

bestRotation w f g = (a, disc [a])
    where f' r = rotStatic r f
          c r w = m2 (f' r w - g w)
          m2 z = magnitude z ^ 2
          disc [r] = sum (map (c r) [-w..w])
          ([a], p) = minimizeNMSimplex disc [0] [10*degree] prec nmax

alignedTo w (f,hp,p) (g,hc,c) = htp h p
    where (a,_) = bestRotation w f g
          h = inv hc <> rot3 (-a) <> hp

feat' = memo . normalizeStart . fourierPL . whitenContour

feat c = (f,h,c) where
    f = memo . normalizeStart . fourierPL $ wc
    wc = htp h c
    h = whitener c

htp h (Closed c) = Closed . map l2p . ht h . map p2l $ c
p2l (Point x y) = [x,y]
l2p [x,y] = Point x y


------------------------------------------------------------

main = do
    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz >>= withPause

    state <- prepare' ([],Nothing)

    o <- createParameters [
        ("umbral2",intParam 128 1 255),
        ("area",percent 1),
        ("fracpix",realParam (1.5) 0 10),
        ("comps",intParam 8 1 20),
        ("white",intParam 0 0 1),
        ("eps",realParam 0.03 0 0.2),
        ("smooth2",intParam 1 0 10),
        ("rotation",realParam 0 (-pi) pi),
        ("showFou",intParam 0 0 1)
     ]

    addWindow "contours" sz Nothing (marker (kbdcam ctrl)) state

    launch' state (worker cam o)

-----------------------------------------------------------------

worker cam param inWindow (prots',mbp) = do


    th2' <- getParam param "umbral2" ::IO Int
    let th2 = fromIntegral th2'
    smooth2 <- getParam param "smooth2" :: IO Int
    area <- getParam param "area"
    fracpix <- getParam param "fracpix"
    comps <- getParam param "comps"
    white <- getParam param "white"
    showFou <- getParam param "showFou" :: IO Int
    eps <- getParam param "eps"
    rotation <- getParam param "rotation"

    orig <- cam >>= yuvToGray
    im <-(smooth2 `times` median Mask3x3) orig

    let (Size h w) = size im
        pixarea = h*w*area`div`1000
        rawconts = contours 100 pixarea th2 (toEnum white) im
        proc = Closed . pixelsToPoints (size orig).douglasPeuckerClosed fracpix.fst3
        cs = map proc $ rawconts
        --wcs = map whitenContour cs

        --fcs = map (filterSpectral comps 100) cs
        --selc = map (Closed . map c2p . spectralComponent comps 100) wcs
        --fwcs = map (filterSpectral comps 100) wcs

        prots = case mbp of
                Just p -> (feat . proc) (closestTo p rawconts) : prots'
                _      -> prots'


        -- detected = [c | c <- seq prots cs, p <- prots, similarTo eps p c]
        --                      ^ space leak if prots not used

        detected = [alignedTo comps p (feat c) | p <- prots, c <- cs, similarTo comps eps p (feat c)]


    inWindow "contours" $ do
             drawImage orig
             pointCoordinates (size im)
             lineWidth $= 2
             setColor 1 1 0
             --mapM_ shcont wcs
             lineWidth $= 4
             setColor 1 0 0
             --mapM_ (shcont. invFou 50 comps) detected
             mapM_ shcont detected
             lineWidth $= 2
             setColor 0 0 1
             if (showFou==0) 
                then mapM_ (shcont) cs
                else mapM_ (shcont. invFou 50 comps . rotStatic rotation . normalizeStart . fourierPL) cs
             --lineWidth $= 1
             --setColor 0 0.6 0
             --mapM_ shcont selc
             --lineWidth $= 2
             --setColor 1 0 0
             --mapM_ (\c -> renderPrimitive LineLoop $ mapM_ vertex c) (map (affine comps) cs)

    return (prots,Nothing)


-------------------------------------------

marker _ st (MouseButton LeftButton) Down _ pos@(Position x y) = do
    s @ State { ust= (prot,_) } <- readIORef st
    let clicked = Pixel (fromIntegral y) (fromIntegral x)
    writeIORef st (s {ust = (prot, Just clicked)})

marker def _ a b c d = def a b c d

closestTo pixel = minimumBy (compare `on` (distP2ROI pixel))
    where distP2ROI (Pixel r c) (_,_,ROI r1 r2 c1 c2) = ((r1+r2)`div`2 - r)^2 + ((c1+c2)`div`2 - c)^2


fst3 (a,_,_) = a

c2p (x:+y) = Point x y


shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 5
    renderPrimitive Points (vertex (head c))

filterSpectral w n cont = Closed r where
    fou = normalizeRotation $ fourierPL cont
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map fou [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)
    c2p (x:+y) = Point x y


spectralComponent w n cont = r where
    fou = normalizeRotation $ fourierPL cont
    z n = replicate n 0
    sc = abs $ fromIntegral w^2
    f = fromList $ fou 0 : z (w-1) ++ (sc*fou w) : z (n- 2*w - 1) ++ (sc*fou (-w)) : z (w-1)
    r = toList $ ifft (fromIntegral n *f)


-- affine :: Int -> Polyline -> Polyline
affine w (Closed c) = aps where
    (mx0,my0,_,_,_) = momentsContour c
    Closed wps = whitenContour (Closed c)
    fou = fourierPL (Closed wps)
    --a = (phase (fou (-w)) + phase (fou w)) / 2
    a = goodDir fou
    t = desp (mx0,my0) <> rot3 (a) <> desp (-mx0,-my0)
    p2l (Point x y) = [x,y]
    l2p [x,y] = Point x y
    aps = map l2p $ ht t (map p2l wps)


excentricity f w = (s/l,l,a) where
    p = f w
    n = f (-w)
    ap = magnitude p
    an = magnitude n
    l = ap + an
    s = abs (ap - an)
    a = 0.5*(phase p + phase n)
    --a' = if abs a < pi/2 then a else pi + a



shiftStart r f = \w -> cis (fromIntegral w*r) * f w

normalizeStart f = shiftStart (-t) f
    where t = phase ((f (1)- (conjugate $ f(-1))))


goodDir f = a where
    (_,_,a) = minimumBy (compare `on` quality) (map (excentricity f) [1..5])
    quality (ex,tam,_) = 2*ex/tam


normalizeRotation f = g where
    a = goodDir f
    aux = normalizeStart ( (* cis (-a) ).f)
    p0 = aux 1 + aux (-1) + aux 2 + aux (-2)
    pm = -aux 1 + aux (-1) + aux 2 + aux (-2)
    pc = aux 0
    z = if realPart (aux 2 + aux (-2)) < 0 --magnitude (p0-pc) > magnitude (pm - pc) 
        then cis (-a)
        else cis (pi-a)
    g 0 = f 0
    g w = z * f w

invFou n w fou = Closed r where
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map (fou) [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)
    c2p (x:+y) = Point x y


rot r f = g where g 0 = f 0
                  g w = cis (r) * f w

rotStatic r f = g where g 0 = f 0
                        g w = cis (r - (t1-t0)*fromIntegral w) * f w
                        t0 = phase ((f (1)- (conjugate $ f(-1))))
                        t1 = phase ((cis r*f 1- (conjugate $ cis r * f(-1))))

--memo = id

memo f = g where
    m = listArray (-20,20::Int) [f k | k <- [-20..20]]
    g w = m ! w