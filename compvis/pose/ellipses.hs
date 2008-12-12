-- ellipse detection and similar rectification from the images of circles

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
import Vision.Camera
import Numeric.GSL.Polynomials
import Text.Printf

------------------------------------------------------------

sz' = Size 640 480

main = do

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    prepare

    o <- createParameters [("threshold",intParam 128 1 255),
                           ("area",percent 5),
                           ("tolerance",percent 10),
                           ("fracpix",realParam 0.8 0 10),
                           ("scale", realParam 1 0 5)]

    w  <- evWindow () "Ellipses" sz  Nothing (const (kbdcam ctrl))
    wr <- evWindow () "Rectif"   sz' Nothing (const (kbdcam ctrl))

--     mapM_ print [(k,foudebug k) | k <- [-5..5]]
--     error "fin"

    launch (worker w wr cam o)

-----------------------------------------------------------------

worker w wr cam param = do

    th2 <- fromIntegral `fmap` (getParam param "threshold" ::IO Int)
    area <- getParam param "area"
    fracpix <- getParam param "fracpix"
    tol <- getParam param "tolerance"
    sc  <- getParam param "scale"

    orig <- cam

    inWin w $ do
        drawImage (gray orig)

        let (Size h w) = size (gray orig)
            pixarea = h*w*area`div`1000
            redu = Closed . pixelsToPoints (size $ gray orig). douglasPeuckerClosed fracpix
            cs1 = map (redu.fst3) $ contours 100 pixarea th2 True (gray orig)
            cs2 = map (redu.fst3) $ contours 100 pixarea th2 False (gray orig)
            candidates = cs1++cs2 -- ++[control]
            rawellipses = filter (isEllipse tol) candidates
            ellipses = sortBy (compare `on` (negate.areaConic)) $ map analyzeConic rawellipses

        pointCoordinates (size $ gray orig)
        setColor' Col.yellow
        lineWidth $= 1
        mapM_ shcont candidates
        setColor' Col.red
        lineWidth $= 3
        mapM_ (disppl . conicPoints) ellipses
        when (length ellipses >= 2) $ do
            let (w,coeffs) = reduceConics (ellipses!!0) (ellipses!!1)
                coeffs' = (-1,-16,8,11/4)
                sol = htc (comp $ inv w) $ intersectionReduced coeffs
                (ij,other) = selectSol (ellipses!!0) (ellipses!!1) sol
                [h1,h2] = getHorizs [ij,other]
            --lineWidth $= 1
            --setColor' Col.green
            --mapM_ (disppl. ht (scaling 0.2<>w). conicPoints) ellipses
            setColor' Col.blue
            shLine h1
            setColor' Col.yellow
            shLine h2
            pointSize $= 5
            setColor' Col.purple
            renderPrimitive Points $ mapM (vertex.map realPart) [ij,other]
            let recraw = diagl [1,-1,1] <> rectifierFromCircularPoint ij
            let rec' = recraw
                (mx,my,_,_,_) = ellipses!!0
                (mx2,my2,_,_,_) = ellipses!!1
                [[mx',my'],[mx'2,my'2]] = ht recraw [[mx,my],[mx2,my2]]
                okrec = similarFrom2Points [mx',my'] [mx'2,my'2] [0,0] [-0.5, 0] <> recraw
            inWin wr $ do
                --drawImage $ warp 0 sz' (scaling sc <> w) (gray orig)
                drawImage $ warp (0,0,0) sz' (scaling sc <> okrec ) (rgb orig)
                --text2D 30 30 $ show $ focalFromHomogZ0 (inv recraw)
                -- it is also encoded in the circular points
                text2D 30 30 $ printf "f = %.2f" $ focalFromCircularPoint (fromList ij)
                text2D 30 50 $ printf "ang = %.1f" $ abs ((acos $ orthoTest ij)/degree - 90)


norm x = pnorm PNorm2 x

fst3 (a,_,_) = a

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c

shcontP (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    renderPrimitive Points $ mapM_ vertex c
    pointSize $= 5
    renderPrimitive Points $ vertex (head c)

isEllipse' tol (Closed ps) = s < fromIntegral (tol::Int)/100000
    where (_,s) = estimateConicRaw ps

isEllipse tol c = (ft-f1)/ft < fromIntegral (tol::Int)/1000 where
    wc = whitenContour c
    f  = fourierPL wc
    f0 = magnitude (f 0)
    f1 = sqrt (magnitude (f (-1)) ^2 + magnitude (f 1) ^2)
    ft = sqrt (norm2Cont wc - f0 ^2)


estimateConicRaw ::  [Point] -> (Matrix Double, Double)
estimateConicRaw ps = (con,err) where
    con = (3><3) [a,c,d
                 ,c,b,e
                 ,d,e,f]
    (s,err) = homogSystem eqs
    [a,b,c,d,e,f] = toList s
    eqs = map eq ps
    eq (Point x y) = [x*x, y*y, 2*x*y, 2*x, 2*y, 1.0]


analyzeConic (Closed ps) = (mx,my,d1,d2,a) where
    (mx,my,cxx,cyy,cxy) = momentsContour ps
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    d1 = 2*sqrt l1
    d2 = 2*sqrt l2

areaConic (_,_,d1,d2,_) = pi*d1*d2

whitenEllipse (mx,my,d1,d2,a) = diag (fromList [1/d1,1/d2,1]) <> rot3 (a) <> desp (-mx,-my)

disppl l = renderPrimitive LineLoop $ mapM_ vertex l

conicPoints (mx,my,d1,d2,a) = xs where
    xs = map pt ts
    ts = tail $ toList $ linspace 50 (0,2*pi)
    pt t = [mx + x*cos a - y*sin a, my + x*sin a + y*cos a]
        where x = d1*cos t
              y = d2*sin t

chooseRot mx my a = w where
    v = fromList [mx,my,1]
    [mx',my',_] = toList $ rot3 a <> v
    w = if abs mx' > abs my'
        then rot3 a
        else rot3 (a+pi/2)

htp h (Closed c) = Closed . map l2p . ht h . map p2l $ c
p2l (Point x y) = [x,y]
l2p [x,y] = Point x y

diagl = diag . fromList

-------------------------------------------------------------------

-- computes a similar 2D homogeneous transformation with moves 2 points to a desired position
similarFrom2Points [ax,ay] [bx,by] [a'x,a'y] [b'x,b'y] = t where
    dx = a'x - ax
    dy = a'y - ay
    s = sqrt ((b'x-a'x)^2 + (b'y-a'y)^2) / sqrt ((bx-ax)^2 + (by-ay)^2)
    ang = atan2 (b'y-a'y) (b'x-a'x) - atan2 (by-ay) (bx-ax)
    t = rot3 (-ang) <> scaling s <> desp (dx,dy)

-------------------------------------------------------------------
-- moves two ellipses to reduced form for easy intersection
reduceConics ell1 ell2 = (w, coeffs) where
    w1 = whitenEllipse ell1
    (mx,my,_,_,a) = analyzeConic $ Closed $ map l2p $ ht w1 $ conicPoints $ ell2
    w2 = chooseRot mx my a
    w = w2 <> w1
    coeffs = getCoeffs $ fst $ estimateConicRaw $ map l2p $ ht w $ conicPoints $ ell2

-- coefficients in
-- ax^2 + by^2 + x + ey + f
-- (not the entry in the conic matrix)
getCoeffs m' = (a,b,e,f) where
    m = m' */ (2*m'@@>(0,2))
    a = m @@> (0,0)
    b = m @@> (1,1)
    f = m @@> (2,2)
    e = m @@> (1,2) * 2

--------------------------------------------------------------------
-- Intersection of two ellipses (after common diagonalization)
-- (based on the idea of elimination of the x^2coefficient in vgl : Geometry Library)
intersectionReduced prob = map sol (polySolve (coefs prob)) where
    sol y = [x,y] where x = thex prob y

    coefs (a,b,e,f) =
        [ -1 + a*a + 2*a*f + f*f
        , 2*a*e + 2*e*f
        , 1 - 2*a*a + 2*a*b + e*e - 2*a*f + 2*b*f 
        , -2*a*e + 2*b*e
        , a*a - 2*a*b + b*b]

    thex (a',b',e',f') y = -a-f - e*y + (a-b)*y*y
        where a = a':+0
              b = b':+0
              e = e':+0
              f = f':+0
--------------------------------------------------------------------

getHorizs pts = map linf pts where
    linf [x,y] = toList $ unitary $ snd $ fromComplex $ cross v (conj v)
        where v = fromList [x,y,1]

shLine [a,b,c] = renderPrimitive Lines $ mapM f [-1,1]
    where f x = vertex $ Point x ((-a*x-c)/b)

selectSol (x1,y1,_,_,_) (x2,y2,_,_,_) pts = (ij,other) where
    ls = getHorizs pts
    ij    = head [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) > 0 ]
    other = head [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) < 0 ]
    f [a,b,c] (x,y) = a*x + b*y + c

--------------------------------------------------------------------

rectifierFromCircularPoint j' = inv t where
    cir = fromList $ j'++[1]
    omega = fst $ fromComplex $ cir `outer` conj cir + conj cir `outer` cir
    (s,u) = eigSH omega -- positive definite by construction
    [a,b,_] = toList s
    t = trans $ diagl [sqrt a, sqrt b, 1] <> trans u
    -- 0 =~= norm $ (normat3 $ t <> diagl [1,1,0] <> trans t) - (normat3 omega)

focalFromCircularPoint j' = x * sqrt (1-(y/x)^2) where
    pn = fst $ fromComplex j'
    x = norm (complex pn - j')
    y = norm pn
    -- alpha = asin (y/x)

-- consistency with diag(f,f,1) camera
orthoTest [x,y] = innerLines n0 h where
    n0 = fromList[realPart x, realPart y, 1] `cross` fromList[0,0,1]
    h = snd $ fromComplex $ cross jh (conj jh)
    jh = fromList [x,y,1]

innerLines l m = (l.*.m)/ sqrt (l.*.l) / sqrt(m.*.m)
    where a.*.b = a <> mS <.> b

debugpoly = Closed [Point 0 0, Point 1 0, Point 1 1, Point 0 1]
foudebug = fourierPL debugpoly

control = Closed [Point (0.5+0.4*cos t) (0.5+0.1*sin t) | t <- tail $ toList (linspace 20 (0,2*pi))]