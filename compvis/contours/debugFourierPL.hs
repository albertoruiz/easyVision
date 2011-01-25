import EasyVision as EV hiding (debug,fourierPL)
import Control.Arrow
import Numeric.LinearAlgebra
import Util.Misc(memo,debug)
import Data.List(zipWith4)


g = fourierPL $ fst $ pentominos!!0

main = mapM_ (print . (id &&& g)) [-7..7]

-- | Exact Fourier series of a piecewise-linear closed curve
fourierPL :: Polyline -> (Int -> Complex Double)

fourierPL c = f
    where
        g = fourierPL' c
        p = map g [0..]
        n = map g [0,-1 .. ]
        f w | w >= 0    = p !! w
            | otherwise = n !! (-w)

fourierPL' (Closed ps) = g where
    (zs,ts,aAs,hs) = prepareFourierPL ps
    g0 = 0.5 * sum (zipWith4 gamma zs ts (tail zs) (tail ts))
        where gamma z1 t1 z2 t2 = (z2+z1)*(t2-t1)
    g 0 = g0
    g w = k* ((vhs**w') <.> vas)
        where k = recip (2*pi*w'')^2
              w' = fromIntegral w  :: Vector (Complex Double)
              w'' = fromIntegral w :: Complex Double
    vhs = debug "h" id $ fromList hs
    vas = debug "A" id $ fromList $ take (length hs) aAs

prepareFourierPL c = (zs,ts,aAs,hs) where
    zs = debug "z" id $ map p2c (c++[head c])
        where p2c (Point x y) = x:+y
    ts = debug "t" id $ map (/last acclen) acclen
        where acclen = scanl (+) 0 (zipWith sl zs (tail zs))
              sl z1 z2 = abs (z2-z1)
    hs = tail $ map exp' ts
        where exp' t = exp (-2*pi*i*t)
    as = cycle $ debug "a" id $ zipWith4 alpha zs ts (tail zs) (tail ts)
        where alpha z1 t1 z2 t2 = (z2-z1)/(t2-t1)
    aAs = zipWith (-) as (tail as)


{-

z: [5.0 :+ 0.0,
    5.0 :+ 1.0,
    0.0 :+ 1.0,
    0.0 :+ 0.0,
    5.0 :+ 0.0]
    
t: [0.0 :+ 0.0,
    8.333333333333333e-2 :+ 0.0,
    0.5 :+ 0.0,
    0.5833333333333334 :+ 0.0,
    1.0 :+ 0.0]

h: [0.8660254037844387 :+ (-0.49999999999999994),
    (-1.0) :+ (-1.2246063538223773e-16),
    (-0.8660254037844386) :+ 0.5000000000000001,
    1.0 :+ 2.4492127076447545e-16]

a: [0.0 :+ 12.0,
    (-12.0) :+ 0.0,
    0.0 :+ (-11.999999999999995),
    12.0 :+ 0.0]

A: [12.0 :+ 12.0,
   (-12.0) :+ 11.999999999999995,
   (-12.0) :+ (-11.999999999999995),
   12.0 :+ (-12.0)]

(k, f k)
(-7,7.86551712094698e-3 :+ (-2.935450952317709e-2))
(-6,(-1.2407246234035385e-17) :+ (-7.499274064133144e-18))
(-5,(-8.900670517104971e-3) :+ (-3.321775459126601e-2))
(-4,(-1.555788119139927e-17) :+ (-2.0907705120939567e-17))
(-3,(-3.049997726473942e-17) :+ (-3.049997726473942e-17))
(-2,(-4.7840827953075953e-17) :+ (-3.548789775026679e-17))
(-1,0.8304438647816504 :+ 0.22251676292762362)
(0,2.5 :+ 0.5)
(1,1.4383709666356772 :+ (-0.3854103389264029))
(2,4.7835335320704763e-17 :+ 1.7411644616676324e-18)
(3,0.13509491152311703 :+ (-0.13509491152311698))
(4,1.7667052021936717e-17 :+ (-2.2945471730651527e-18))
(5,1.5416413557056157e-2 :+ (-5.753483866542705e-2))
(6,1.2407246234035385e-17 :+ (-7.499274064133144e-18))
(7,(-4.541158427094343e-3) :+ (-1.69478339751357e-2))

-}
