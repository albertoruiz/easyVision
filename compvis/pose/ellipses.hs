-- ellipse detection and similar rectification from the images of circles

import EasyVision hiding ((.*))
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
import Data.Maybe(isJust)

sz' = Size 600 600

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
            rawellipses = sortBy (compare `on` (negate.perimeter))
                        . filter (isEllipse tol)
                        $ candidates
            est (Closed ps) = fst $ estimateConicRaw ps
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
            let sol = intersectionEllipses (ellipMat!!0) (ellipMat!!1)
                (mbij,mbother) = selectSol (ellipses!!0) (ellipses!!1) sol
            when (isJust mbij && isJust mbother) $ do
                let Just ij    = mbij
                    Just other = mbother
                    [h1,h2] = getHorizs [ij,other]
                lineWidth $= 1
                setColor' Col.blue
                shLine h1
                setColor' Col.yellow
                shLine h2
                pointSize $= 5
                setColor' Col.purple
                renderPrimitive Points $ mapM (vertex.map realPart) [ij,other]
                setColor' Col.green
                mapM_ shLine $ map (map realPart) $ tangentEllipses (ellipMat!!0) (ellipMat!!1)
                let recraw = diagl [1,-1,1] <> rectifierFromCircularPoint ij
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

mt m = trans (inv m)

fst3 (a,_,_) = a

htp h (Closed c) = Closed . map l2p . ht h . map p2l $ c
p2l (Point x y) = [x,y]
l2p [x,y] = Point x y

diagl = diag . fromList

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

----------------------------------------------------------------

-- computes a similar 2D homogeneous transformation with moves 2 points to a desired position
similarFrom2Points [ax,ay] [bx,by] [a'x,a'y] [b'x,b'y] = t where
    dx = a'x - ax
    dy = a'y - ay
    s = sqrt ((b'x-a'x)^2 + (b'y-a'y)^2) / sqrt ((bx-ax)^2 + (by-ay)^2)
    ang = atan2 (b'y-a'y) (b'x-a'x) - atan2 (by-ay) (bx-ax)
    t = rot3 (-ang) <> scaling s <> desp (dx,dy)

-------------------------------------------------------------------

analyzeEllipse m = ((mx,my,dx,dy,alpha),t) where
    a = m@@>(0,0)
    b = m@@>(1,1)
    c = m@@>(0,1)
    phi = 0.5 * atan2 (2*c) (b-a)
    t1 = rot3 (-phi)
    m1 = t1 <> m <> trans t1
    a' = m1@@>(0,0)
    b' = m1@@>(1,1)
    t2 = diagl [sqrt (abs $ a'/b'), 1,1]
    -- abs is necessary for projective reduceConics
    m2' = mt t2 <> m1 <> inv t2
    m2 = m2' */ m2'@@>(0,0)
    d'' = m2@@>(0,2)
    e'' = m2@@>(1,2)
    f'' = m2@@>(2,2)
    sc  = sqrt (d''^2 + e''^2 - f'')
    t3 = scaling (1/sc) <> desp (d'',e'')
    t = t3 <> t2 <> t1
    m3 = mt t <> m <> inv t
    [mx,my,_] = toList (inv t <> fromList [0,0,1])
    [sx,sy,_] = toList $ sc .* (1 / takeDiag t2)
    (dx,dy,alpha) = if sx > sy
        then (sx,sy,-phi)
        else (sy,sx,-phi-pi/2)

-- moves two ellipses to reduced form for easy intersection
-- projective version, required for dual conics (tangents)
reduceConics c1 c2 = (w, c2') where
    (s,v) = eigSH' c1
    sg = signum s
    p = (if sg@>1 < 0 then flipud else id) (ident 3)
    w1 = mt $ p <> diag (sg / sqrt (abs s)) <> trans v
    (mx,my,_,_,a) = fst $ analyzeEllipse (mt w1 <> c2 <> inv w1)
    w2 = chooseRot mx my a
    w = w2 <> w1
    c2' = mt w <> c2 <> inv w

-- affine version
reduceConics' c1 c2 = (w, c2') where
    w1 = snd $ analyzeEllipse c1
    (mx,my,_,_,a) = fst $ analyzeEllipse (mt w1 <> c2 <> inv w1)
    w2 = chooseRot mx my a
    w = w2 <> w1
    c2' = mt w <> c2 <> inv w

chooseRot mx my a = w where
    v = fromList [mx,my,1]
    [mx',my',_] = toList $ rot3 a <> v
    w = if abs mx' > abs my'
        then rot3 a
        else rot3 (a+pi/2)

--------------------------------------------------------------------

-- coefficients in
-- ax^2 + by^2 + x + ey + f
-- (not the entry in the conic matrix)
getCoeffs m' = (a,b,e,f) where
    m = m' */ (2*m'@@>(0,2))
    a = m @@> (0,0)
    b = m @@> (1,1)
    f = m @@> (2,2)
    e = m @@> (1,2) * 2

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

intersectionCommonCenter (a,b) = [s1,s2,s3,s4] where
    p = sqrt $ (b+1)/(b-a)
    q = sqrt $ (-1-a)/(b-a)
    s1 = [-p,-q]
    s2 = [-p, q]
    s3 = [ p,-q]
    s4 = [ p, q]

intersectionEllipses c1 c2 = sol where
    (w, c2') = reduceConics c1 c2
    a = c2'@@>(0,0)
    b = c2'@@>(1,1)
    f = c2'@@>(2,2)
    d = c2'@@>(0,2)
    sol = if abs(d/a) > 1E-6
        then htc (comp $ inv w) $ intersectionReduced (getCoeffs c2')
        else htc (comp $ inv w) $ intersectionCommonCenter ((a/f):+0,(b/f):+0)

tangentEllipses c1 c2 = map (++[1]) $ intersectionEllipses (inv c1) (inv c2)

--------------------------------------------------------------------

getHorizs pts = map linf pts where
    linf [x,y] = toList $ unitary $ snd $ fromComplex $ cross v (conj v)
        where v = fromList [x,y,1]

selectSol (x1,y1,_,_,_) (x2,y2,_,_,_) pts = (ij,other) where
    ls = getHorizs pts
    ij    = mbhead [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) > 0 ]
    other = mbhead [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) < 0 ]
    f [a,b,c] (x,y) = a*x + b*y + c
    mbhead [] = Nothing
    mbhead x  = Just (head x)

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
