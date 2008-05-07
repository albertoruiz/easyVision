-- check the UKF

import Numeric.LinearAlgebra
import Util.Kalman
import Vision
import Text.Printf

import Debug.Trace

debug x = trace (show x) x

disp m = putStrLn . format " " (printf "%.2f") $ m


vector x = fromList x :: Vector Double
diagl = diag . vector

partit _ [] = []
partit n l  = take n l : partit n (drop n l)

sep = putStrLn (replicate 20 '-')

main = do
    let m = 3 |> [1,1,1]
        --c = diagl [1,1,1]
        c = (3><3) [1,0.5,0,0.5,4,0,0,0,9]
        t = (2><3) [1,1,0,0,1,1]
    print $ unscentedTransform ((2><3) [1,1,0,0,1,1] <>) (m,c)
    print $ (t<>m, t <> c <> trans t)
    sep
    print $ unscent f 1 [1,1,1]

    test (pose Nothing)
    test (pose' Nothing)
    test (pose (Just 2.8))
    test (pose' (Just 2.8))

test f = do
    let pixerr = 2/640
    sep
    let (m,c) = unscent f pixerr rec
    print m
    print $ sqrt $ takeDiag c
    disp c

unscent f d v = unscentedTransform (fromList . f . toList) (fromList v :: Vector Double, d^2 .* ident (length v))

f [x,y,z] = [x+y,y+z]


rec = [-0.5846224018765912,0.2663793278234847,
       -6.056976868087708e-2,5.2699445744906405e-2,
       9.208272407552359e-2,0.2828595534609387,
       -2.7698388463374485e-2,0.5736364343807604,
       -0.2666551241508568,0.6538599906134482]

asym = map (map (*5.4))
       [ [ 0, 0]
       , [ 0, 2]
       , [ 1, 2]
       , [ 2, 1]
       , [ 2, 0] ]

pose mbf rec = [ f, p, t, r, x, y, z ] where
    h = estimateHomography (partit 2 rec) asym
    Just cam = poseFromHomogZ0 mbf h
    CamPar f p t r (x, y, z) = cam

pose' mbf rec = [ f, p, t, r, x, y, z ] where
    Just (mat,_) = cameraFromPlane 1E-3 300 mbf (partit 2 rec) asym
    cam = poseFromFactorization . factorizeCamera $ mat
    CamPar f p t r (x, y, z) = cam