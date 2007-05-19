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
import GSL hiding (size,norm)
import qualified GSL
import Vision
import Data.List(minimumBy, maximumBy)

import ImagProc.Ipp.Core
import Debug.Trace

debug x = trace (show x) x

------------------------------------------------------------

easyInvar w f = fromList desc where
    sc = g 1
    g k = magnitude (f k) + magnitude (f (-k))
    h k = g k/sc
    desc = map h [2..w]

feat = easyInvar 10 . fourierPL . whitenContour

closestTo pixel = minimumBy (compare `on` (distP2ROI pixel))
    where distP2ROI (Pixel r c) (_,_,ROI r1 r2 c1 c2) = ((r1+r2)`div`2 - r)^2 + ((c1+c2)`div`2 - c)^2

similarTo eps f0 cont = GSL.norm (f0 - feat cont) < eps

------------------------------------------------------------

main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)

    let sz = if Map.member "--size" opts
                 then mpSize $ read $ Map.findWithDefault "20" "--size" opts
                 else Size (read $ Map.findWithDefault "480" "--rows" opts)
                           (read $ Map.findWithDefault "640" "--cols" opts)

    (cam, ctrl)  <- mplayer (args!!0) sz >>= withPause

    state <- prepare ([],Nothing)

    o <- createParameters state [("umbral2",intParam 128 1 255),
                                 ("area",percent 1),
                                 ("fracpix",realParam (1.5) 0 10),
                                 ("comps",intParam 8 1 20),
                                 ("white",intParam 0 0 1),
                                 ("eps",realParam 0.03 0 0.2),
                                 ("smooth2",intParam 1 0 10)]

    addWindow "contours" sz Nothing (marker (kbdcam ctrl)) state

    launch state (worker cam o)

-----------------------------------------------------------------

worker cam param inWindow (prots',mbp) = do


    th2' <- getParam param "umbral2" ::IO Int
    let th2 = fromIntegral th2'
    smooth2 <- getParam param "smooth2" :: IO Int
    area <- getParam param "area"
    fracpix <- getParam param "fracpix"
    --comps <- getParam param "comps"
    white <- getParam param "white"
    eps <- getParam param "eps"

    orig <- cam >>= yuvToGray
    im <-(smooth2 `times` median Mask3x3) orig

    let (Size h w) = size im
        pixarea = h*w*area`div`1000
        rawconts = contours 100 pixarea th2 (toEnum white) im
        proc = Closed .pixelsToPoints (size orig).douglasPeuckerClosed fracpix.fst3
        cs = map proc $ rawconts
        wcs = map whitenContour cs

        --fcs = map (filterSpectral comps 100) cs
        --selc = map (Closed . map c2p . spectralComponent comps 100) wcs
        --fwcs = map (filterSpectral comps 100) wcs

        prots = case mbp of
                Just p -> (feat.proc) (closestTo p rawconts) : prots'
                _      -> prots'

        detected = [c | p <- prots, c <- cs, similarTo eps p c]

        -- detected = [c | c <- seq prots cs, p <- prots, similarTo eps p c]
        --                      ^ space leak if prots not used

    inWindow "contours" $ do
             drawImage orig
             pointCoordinates (size im)
             lineWidth $= 2
             setColor 0 0 1
             mapM_ shcont cs
             lineWidth $= 2
             setColor 1 1 0
             mapM_ shcont wcs
             lineWidth $= 3
             setColor 1 0 0
             mapM_ shcont detected
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


text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s

fst3 (a,_,_) = a

c2p (x:+y) = Point x y


shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 3
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


on f g = \x y -> f (g x) (g y)

goodDir f = a where
    (_,_,a) = minimumBy (compare `on` quality) (map (excentricity f) [1..5])
    quality (ex,tam,_) = 2*ex/tam

normalizeStart f = g  where
    g w = cis (-fromIntegral w*t) * f w
    t = phase (f (1))

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
