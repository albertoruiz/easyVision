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
import Data.List(minimumBy, maximumBy, sort)
import Control.Monad(guard)
import Data.Array
import Numeric.GSL.Minimization

import ImagProc.Ipp.Core
import Debug.Trace

debug x = trace (show x) x

------------------------------------------------------------


htp h (Closed c) = Closed . map l2p . ht h . map p2l $ c
p2l (Point x y) = [x,y]
l2p [x,y] = Point x y


------------------------------------------------------------

main = do
    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz >>= withPause

    state <- prepare' ()

    o <- createParameters [
        ("umbral2",intParam 128 1 255),
        ("area",percent 1),
        ("fracpix",realParam (1.5) 0 10),
        ("maxit",intParam 100 1 200),
        ("white",intParam 1 0 1),
        ("eps",realParam 0.1 0 0.3),
        ("smooth2",intParam 1 0 10)
        --("rotation",realParam 0 (-pi) pi),
        --("showFou",intParam 0 0 1)
     ]

    addWindow "contours" sz Nothing (marker (kbdcam ctrl)) state

    launch' state (worker cam o)

-----------------------------------------------------------------

worker cam param inWindow () = do


    th2' <- getParam param "umbral2" ::IO Int
    let th2 = fromIntegral th2'
    smooth2 <- getParam param "smooth2" :: IO Int
    area <- getParam param "area"
    fracpix <- getParam param "fracpix"
    --comps <- getParam param "comps"
    white <- getParam param "white"
    --showFou <- getParam param "showFou" :: IO Int
    eps <- getParam param "eps" ::IO Double
    --rotation <- getParam param "rotation"
--    maxit <- getParam param "maxit"

    orig <- cam >>= yuvToGray
    im <-(smooth2 `times` median Mask3x3) orig

    let (Size h w) = size im
        pixarea = h*w*area`div`1000
        rawconts = contours 100 pixarea th2 (toEnum white) im
        proc = Closed . pixelsToPoints (size orig).douglasPeuckerClosed fracpix.fst3
        nice p@(Closed l) = perimeter p / fromIntegral (length l) > eps
        cs = map proc $ rawconts
        ok = filter nice cs

    let pru    = length `estOf` (map fst3 rawconts)
        sndOrd =  scanl ana (0,0,0,0,0,0) pru

    inWindow "contours" $ do
             drawImage orig
             pointCoordinates (size im)
             lineWidth $= 1
             setColor 0 0 1
             mapM_ (shcont) cs

             lineWidth $= 2
             setColor 0.5 0 0
             mapM_ (shcont) ok
             lineWidth $= 3
             setColor 1 0 0
             --let segs = concatMap (longestSegments 4) ok
--             renderPrimitive Lines $ mapM_ drawSeg segs

             
             --mapM_ shcont $ 

             --let adj = map (adjust 0.0001 maxit) ok
             --lineWidth $= 2
             --setColor 1 0 1
             --mapM_ (shcont) adj

    return ()


-------------------------------------------

marker _ st (MouseButton LeftButton) Down _ pos@(Position x y) = do
    s @ State { ust= () } <- readIORef st
    let clicked = Pixel (fromIntegral y) (fromIntegral x)
    writeIORef st (s {ust = ()})

marker def _ a b c d = def a b c d

closestTo pixel = minimumBy (compare `on` (distP2ROI pixel))
    where distP2ROI (Pixel r c) (_,_,ROI r1 r2 c1 c2) = ((r1+r2)`div`2 - r)^2 + ((c1+c2)`div`2 - c)^2



text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s

fst3 (a,_,_) = a

c2p (x:+y) = Point x y

drawSeg s = do
    vertex $ (extreme1 s)
    vertex $ (extreme2 s)

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 5
    renderPrimitive Points (vertex (head c))

shiftStart r f = \w -> cis (fromIntegral w*r) * f w

normalizeStart f = shiftStart (-t) f
    where t = phase ((f (1)- (conjugate $ f(-1))))

dist k f1 f2 = sum $ map (magnitude) $ zipWith (-) (map f1 x) (map f2 x)
    where x = [-k..k]

adjust prec nmax pol = Closed res where
    ref = memo . normalizeStart . fourierPL $ pol
    cost [x1,y1,x2,y2,x3,y3,x4,y4] = dist 8 ref ten
        where ten = {-# SCC "ten" #-} normalizeStart . fourierPL . Closed $ [Point x1 y1, Point x2 y2, Point x3 y3, Point x4 y4]
    (sol,path) = minimizeNMSimplex cost initial (replicate 8 0.2) prec nmax
    res = [Point x1 y1, Point x2 y2, Point x3 y3, Point x4 y4]
        where [x1,y1,x2,y2,x3,y3,x4,y4] = sol
    box = [x1,y1,x2,y1,x2,y2,x1,y2] where
        Closed l = pol
        lx = map (\(Point x _) -> x) l
        ly = map (\(Point _ y) -> y) l
        x1 = minimum lx
        x2 = maximum lx
        y1 = minimum ly
        y2 = maximum ly
    initial = case selectPolygons 0.05 4 [pol] of
        []  -> box
        [p] ->  let Closed [Point x1 y1, Point x2 y2, Point x3 y3, Point x4 y4] = p
                 in [x1,y1,x2,y2,x3,y3,x4,y4]


memo f = g where
    m = listArray (-8,8::Int) [f k | k <- [-8..8]]
    g w = m ! w


-----------------------------------------------------------------

estOf prop = maximumBy (compare `on` prop)

thePoints (Closed l) = l
thePoints (Open l) = l

ana (l,sx,sy,sx2,sy2,sxy) (Pixel r c) =
    (l+1, sx+c, sy+r, sx2+c*c, sy2+r*r, sxy+r*c)
