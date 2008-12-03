-- ellipse detection and rectification from circles

import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set,Matrix)
import qualified Data.Colour.Names as Col
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra
import Complex
import Vision.Estimation(homogSystem)
import Control.Monad(when)
import Data.List(sortBy)
import Vision.Geometry

------------------------------------------------------------

main = do

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    prepare

    o <- createParameters [("threshold",intParam 128 1 255),
                           ("area",percent 5),
                           ("tolerance",percent 10),
                           ("fracpix",realParam (1.5) 0 10)]

    w <- evWindow () "Ellipses" sz Nothing (const (kbdcam ctrl))

    launch (worker w cam o)

-----------------------------------------------------------------

worker w cam param = do

    th2 <- fromIntegral `fmap` (getParam param "threshold" ::IO Int)
    area <- getParam param "area"
    fracpix <- getParam param "fracpix"
    tol <- getParam param "tolerance"

    orig <- cam

    inWin w $ do
        drawImage (gray orig)

        let (Size h w) = size (gray orig)
            pixarea = h*w*area`div`1000
            redu = Closed . pixelsToPoints (size $ gray orig). douglasPeuckerClosed fracpix
            cs1 = map (redu.fst3) $ contours 100 pixarea th2 True (gray orig)
            cs2 = map (redu.fst3) $ contours 100 pixarea th2 False (gray orig)
            candidates = cs1++cs2 -- ++[control]
            ellipses = sortBy (compare `on` (negate.areaConic)) $ map analyzeConic $ filter (isEllipse tol) candidates

        pointCoordinates (size $ gray orig)
        setColor' Col.yellow
        lineWidth $= 1
        mapM_ shcont candidates
        setColor' Col.red
        lineWidth $= 3
        mapM_ (shcont . Closed . showConic) ellipses
        when (length ellipses >= 1) $ do
            let w = scaling 0.2 <> whitenEllipse (head ellipses)
            lineWidth $= 1
            setColor' Col.blue
            mapM_ (dispConic w) ellipses


fst3 (a,_,_) = a

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c

shcontP (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    renderPrimitive Points $ mapM_ vertex c
    pointSize $= 5
    renderPrimitive Points $ vertex (head c)

isEllipse' tol (Closed ps) = s/k < fromIntegral (tol::Int)/200000
    where (con,s) = estimateConicRaw ps
          [a,b,f] = toList $ takeDiag con
          k = (sqrt (abs (a/f)) + sqrt (abs (b/f))) * fromIntegral (length ps)

isEllipse tol c = (ft-f1)/ft < fromIntegral (tol::Int)/1000 where
    wc = whitenContour c
    f  = fourierPL wc
    f0 = magnitude (f 0)
    f1 = sqrt (magnitude (f (-1)) ^2 + magnitude (f 1) ^2)
    ft = sqrt (norm2Cont wc - f0 ^2)

control = Closed [Point (0.5+0.4*cos t) (0.5+0.1*sin t) | t <- tail $ toList (linspace 20 (0,2*pi))]

estimateConicRaw ::  [Point] -> (Matrix Double, Double)
estimateConicRaw ps = (con,err) where
    con = (3><3) [a,c,d
                 ,c,b,e
                 ,d,e,f]
    (s,err) = homogSystem eqs
    [a,b,c,d,e,f] = toList s
    eqs = map eq ps
    eq (Point x y) = [x*x, y*y, x*y, x, y, 1.0]

showConic (mx,my,d1,d2,a) = xs where
    xs = map pt ts
    ts = tail $ toList $ linspace 30 (0,2*pi)
    pt t = Point (mx + x*cos a - y*sin a) (my + x*sin a + y*cos a)
        where x = d1*cos t
              y = d2*sin t

analyzeConic (Closed ps) = (mx,my,d1,d2,a) where
    (mx,my,cxx,cyy,cxy) = momentsContour ps
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    d1 = 2*sqrt l1
    d2 = 2*sqrt l2

areaConic (_,_,d1,d2,_) = pi*d1*d2

whitenEllipse (mx,my,d1,d2,a) = diag (fromList [1/d1,1/d2,1]) <> rot3 (a) <> desp (-mx,-my)



dispConic w c = renderPrimitive LineLoop $ mapM_ vertex $ ht w $ map (\(Point x y) -> [x,y]) (showConic c)