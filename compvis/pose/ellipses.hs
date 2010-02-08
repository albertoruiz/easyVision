-- ellipse detection and similar rectification from the images of circles

import EasyVision hiding ((.*))
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set,Matrix)
import qualified Data.Colour.Names as Col
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra
import Complex
import Control.Monad(when)
import Data.List(sortBy)
import Vision.Geometry
import Vision.Camera
import Util.Ellipses
import Numeric.GSL.Minimization
import Text.Printf
import Data.Maybe(isJust)

sz' = Size 600 600

main = do

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  ~> channels >>= withPause

    prepare

    o <- createParameters [("threshold",intParam 128 1 255),
                           ("area",percent 5),
                           ("tolerance",percent 10),
                           ("fracpix",realParam 0.8 0 10),
                           ("scale", realParam 1 0 5),
                           ("method", intParam 1 1 2)]

    w  <- evWindow () "Ellipses" sz  Nothing (const (kbdcam ctrl))
    wr <- evWindow () "Rectif"   sz' Nothing (const (kbdcam ctrl))

    launch (worker w wr cam o)

-----------------------------------------------------------------

worker w wr cam param = do

    th2 <- fromIntegral `fmap` (getParam param "threshold" ::IO Int)
    area <- getParam param "area"
    fracpix <- getParam param "fracpix"
    tol <- getParam param "tolerance"
    sc  <- getParam param "scale"
    method  <- getParam param "method" :: IO Int

    orig <- cam

    inWin w $ do
        drawImage (gray orig)

        let (Size h w) = size (gray orig)
            pixarea = h*w*area`div`1000
            redu = Closed . pixelsToPoints (size $ gray orig). douglasPeuckerClosed fracpix
            cs1 = map (redu.fst3) $ contours 100 pixarea th2 True (gray orig)
            cs2 = map (redu.fst3) $ contours 100 pixarea th2 False (gray orig)
            candidates = cs1++cs2
            rawellipses = sortBy (compare `on` (negate.perimeter))
                        . filter (isEllipse tol)
                        $ candidates
            est (Closed ps) = estimateConicRaw (map p2l ps)
            ellipMat = map est rawellipses
            ellipses = map (fst.analyzeEllipse) ellipMat
        pointCoordinates (size $ gray orig)
        lineWidth $= 1
        setColor' Col.yellow
        mapM_ shcont candidates
        setColor' Col.red
        lineWidth $= 3
        mapM_ (disppl . conicPoints) ellipses
        when (length ellipses >= 2) $ do
            let improve = if method == 1 then id else flip improveCirc ellipMat
            let sol = intersectionEllipses (ellipMat!!0) (ellipMat!!1)
                (mbij,mbother) = selectSol (ellipses!!0) (ellipses!!1) sol
            when (isJust mbij && isJust mbother) $ do
                let Just ijr    = mbij
                    ij = improve ijr
                    Just other = mbother
                    [h1,h2] = getHorizs [ij,other]
                lineWidth $= 1
                setColor' Col.blue
                shLine h1
                setColor' Col.yellow
                shLine h2
                pointSize $= 5
                setColor' Col.purple
                renderPrimitive Points $ mapM (vertex.map realPart.t2l) [ij,other]
                setColor' Col.green
                mapM_ shLine $ map (map realPart) $ tangentEllipses (ellipMat!!0) (ellipMat!!1)
                let recraw = rectifierFromCircularPoint ij
                    (mx,my,_,_,_) = ellipses!!0
                    (mx2,my2,_,_,_) = ellipses!!1
                    [[mx',my'],[mx'2,my'2]] = ht recraw [[mx,my],[mx2,my2]]
                    okrec = similarFrom2Points [mx',my'] [mx'2,my'2] [0,0] [-0.5, 0] <> recraw
                inWin wr $ do
                    --drawImage $ warp 0 sz' (scaling sc <> w) (gray orig)
                    drawImage $ warp (0,0,0) sz' (scaling sc <> okrec ) (rgb orig)
                    --text2D 30 30 $ show $ focalFromHomogZ0 (inv recraw)
                    -- it is also encoded in the circular points
                    text2D 30 30 $ printf "f = %.2f" $ focalFromCircularPoint ij
                    text2D 30 50 $ printf "ang = %.1f" $ abs ((acos $ circularConsistency ij)/degree - 90)


norm x = pnorm PNorm2 x
mt m = trans (inv m)
--diagl = diag . fromList

fst3 (a,_,_) = a
t2l (a,b) = [a,b]
htp h (Closed c) = Closed . map l2p . ht h . map p2l $ c
p2l (Point x y) = [x,y]
l2p [x,y] = Point x y

disppl l = renderPrimitive LineLoop $ mapM_ vertex l

shcont (Closed c) = disppl c

shLine [a,b,c] = renderPrimitive Lines $ mapM f [-1,1]
    where f x = vertex $ Point x ((-a*x-c)/b)

conicPoints (mx,my,d1,d2,a) = xs where
    xs = map pt ts
    ts = tail $ toList $ linspace 50 (0,2*pi)
    pt t = [mx + x*cos a - y*sin a, my + x*sin a + y*cos a]
        where x = d1*cos t
              y = d2*sin t

----------------------------------------------------------------

isEllipse tol c = (ft-f1)/ft < fromIntegral (tol::Int)/1000 where
    wc = whitenContour c
    f  = fourierPL wc
    f0 = magnitude (f 0)
    f1 = sqrt (magnitude (f (-1)) ^2 + magnitude (f 1) ^2)
    ft = sqrt (norm2Cont wc - f0 ^2)

----------------------------------------------------------------

tangentEllipses c1 c2 = map ((++[1]). t2l) $ intersectionEllipses (inv c1) (inv c2)

--------------------------------------------------------------------

getHorizs pts = map linf pts where
    linf (x,y) = toList $ unitary $ snd $ fromComplex $ cross v (conj v)
        where v = fromList [x,y,1]

selectSol (x1,y1,_,_,_) (x2,y2,_,_,_) pts = (ij,other) where
    ls = getHorizs pts
    ij    = mbhead [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) > 0 ]
    other = mbhead [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) < 0 ]
    f [a,b,c] (x,y) = a*x + b*y + c
    mbhead [] = Nothing
    mbhead x  = Just (head x)

-- hmm, this must be studied in more depth
improveCirc (rx:+ix,ry:+iy) ells = (rx':+ix',ry':+iy') where
    [rx',ix',ry',iy'] = fst $ minimize NMSimplex2 1e-5 300 (replicate 4 0.1) cost [rx,ix,ry,iy]
    cost [rx,ix,ry,iy] = sum $ map (eccentricity.rectif) $ ells
        where rectif e = mt t <> e <> inv t
              t = rectifierFromCircularPoint (rx:+ix,ry:+iy)
    eccentricity con = d1-d2 where (_,_,d1,d2,_) = fst $ analyzeEllipse con
