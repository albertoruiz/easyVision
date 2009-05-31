-- check the UKF

import Numeric.LinearAlgebra
import Util.Kalman
import Vision
import Text.Printf

import Debug.Trace

debug x = trace (show x) x

disp = putStrLn . format " " (printf "%.2f")
dispv = putStrLn . ("|> "++) . concatMap (printf "%.3f ") . toList

vector x = fromList x :: Vector Double
diagl = diag . vector

sep = putStrLn (replicate 20 '-')

printmc (m,c) = do
    dispv m
    disp c

main = do
    let m = 3 |> [1,2,3]
        --c = diagl [1,1,1]
        c = (3><3) [1,0.5,0,0.5,4,0,0,0,9]
        t = (2><3) [1,1,0,0,1,1]
        (sigmapoints,(wm,wc)) = unscentedSamples ukfDefaultParam (m,c)
    print $ fromRows sigmapoints
    print $ wm
    print $ wc
    print $ unscentedTransform ukfDefaultParam ((2><3) [1,1,0,0,1,1] <>) (m,c)
    print $ (t<>m, t <> c <> trans t)
    sep
    print $ fromRows $ fst $ checksamp 1 [0,0]
    printmc $ unscent f     1 [0,0]
    printmc $ unscent (f .g (1*pi/180)) 1 [0,0]
    printmc $ unscent (f .g (5*pi/180)) 1 [0,0]
    printmc $ unscent (f .g (45*pi/180)) 1 [0,0]



unscent f d v = unscentedTransform ukfDefaultParam (fromList . f . toList) (fromList v :: Vector Double, d^2 .* ident (length v))
checksamp d v = unscentedSamples ukfDefaultParam (fromList v :: Vector Double, d^2 .* ident (length v))

--f [x,y,z] = [x*y, x^2-z^2 , y*sin z]
f [x,y] = [x*y,x^2-y^2]
--f [x,y] = [sin x, cos x]

g a = toList . (rot a <>) . fromList

rot alpha = (2><2) [ cos alpha, sin alpha,
                    -sin alpha, cos alpha ]