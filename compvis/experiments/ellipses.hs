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
import Numeric.GSL.Polynomials

------------------------------------------------------------

main = do

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    prepare

    o <- createParameters [("threshold",intParam 128 1 255),
                           ("area",percent 5),
                           ("tolerance",percent 10),
                           ("fracpix",realParam 0.8 0 10),
                           ("scale", realParam 0.3 0 5)]

    w  <- evWindow () "Ellipses" sz Nothing (const (kbdcam ctrl))
    wr <- evWindow () "Rectif"   sz Nothing (const (kbdcam ctrl))

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
            ellipses = sortBy (compare `on` (negate.areaConic)) $ map analyzeConic $ filter (isEllipse tol) candidates

        pointCoordinates (size $ gray orig)
        setColor' Col.yellow
        lineWidth $= 1
        mapM_ shcont candidates
        setColor' Col.red
        lineWidth $= 3
        mapM_ (disppl . conicPoints) ellipses
        when (length ellipses >= 1) $ do
            let w = whitenEllipse (head ellipses)
            lineWidth $= 1
            --setColor' Col.gray
            --mapM_ (disppl. ht w. conicPoints) ellipses
            when (length ellipses >= 2) $ do
                let (mx,my,_,_,a) = analyzeConic $ Closed $ map l2p $ ht w $ conicPoints $ ellipses !! 1
                    w2 = chooseRot mx my a
                setColor' Col.green
                mapM_ (disppl. ht (scaling 0.2<>w2<>w). conicPoints) ellipses
                let coeffs = getCoeffs $ fst $ estimateConicRaw $ map l2p $ ht (w2<>w) $ conicPoints $ ellipses !! 1
                let coeffs' = (-1,-16,8,11/4)
                let sol = htc (comp $ inv (w2<>w)) $ intersectionReduced coeffs
--                 mapM_ print $ sol
--                 let f (x,y) = x^2+y^2-1
--                     k x = x:+0
--                     g (a,b,e,f) (x,y) = k a*x^2 + k b*y^2 + x + k e*y + k f
--                 print $ sum $ map (magnitude.g coeffs) sol
--                 putStrLn "---------------------"
                let (ij,other) = selectSol (ellipses!!0) (ellipses!!1) sol
                let [h1,h2] = getHorizs [ij,other]
                setColor' Col.blue
                shLine h1
                setColor' Col.yellow
                shLine h2
                let cir = fromList $ ij++[1]
                    omega = fst $ fromComplex $ cir `outer` conj cir + conj cir `outer` cir
                    (s,u) = eigSH omega -- positive definite by construction
--                 print $ normat3 $ omega
--                 print $ normat3 $ u <> diag s <> trans u
--                 print $ v - trans u
--                 print s
                let [a,b,_] = toList s
                    t = trans $ diagl [sqrt a, sqrt b, 1] <> trans u
                --inWin wr $ drawImage $ warp 0 (size $ gray orig) (scaling 0.2 <> w) (gray orig)
--                 print $ pnorm PNorm2 $ (normat3 $ t <> diagl [1,1,0] <> trans t) - (normat3 omega)
                let rec' = diagl [1,-1,1] <> inv t
                    (mx,my,_,_,_) = ellipses!!0
                    [[dx,dy]] = ht rec' [[mx,my]]
                    rec = desp (-dx,-dy) <> rec'
                    (mx2,my2,_,_,_) = ellipses!!1
                    [d@[a,b]] = ht rec [[mx2,my2]]
                    s = pnorm PNorm2 (fromList d)
                    ang = atan2 b a + pi
                inWin wr $ drawImage $ warp (0,0,0) (size $ gray orig) (scaling (sc/s) <> rot3 ang <> rec) (rgb orig)


fst3 (a,_,_) = a

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c

shcontP (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    renderPrimitive Points $ mapM_ vertex c
    pointSize $= 5
    renderPrimitive Points $ vertex (head c)

isEllipse' tol (Closed ps) = s/k < fromIntegral (tol::Int)/2000000
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

getCoeffs m' = (a,b,e,f) where
    m = m' */ m' @@> (0,2)
    a = m @@> (0,0)
    b = m @@> (1,1)
    f = m @@> (2,2)
    e = m @@> (1,2)

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

intersectionReduced prob = map sol (polySolve (coefs prob)) where
    sol y = [x,y] where x = thex prob y

getHorizs pts = map linf pts where
    linf [x,y] = toList $ unitary $ snd $ fromComplex $ crossc v (conj v)
        where v = fromList [x,y,1]

asMatC v = fromLists [[ 0,-c, b],
                      [ c, 0,-a],
                      [-b, a, 0]]
    where a = v@>0
          b = v@>1
          c = v@>2

crossc a b = asMatC a <> b

shLine [a,b,c] = renderPrimitive Lines $ mapM f [-1,1]
    where f x = vertex $ Point x ((-a*x-c)/b)

selectSol (x1,y1,_,_,_) (x2,y2,_,_,_) pts = (ij,other) where
    ls = getHorizs pts
    ij    = head [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) > 0 ]
    other = head [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) < 0 ]
    f [a,b,c] (x,y) = a*x + b*y + c
