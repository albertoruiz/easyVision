-- check the UKF

import Numeric.LinearAlgebra
import Util.Kalman
import Vision
import Text.Printf

--import Debug.Trace

--debug x = trace (show x) x

disp = putStrLn . format " " (printf "%.3f")
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
    sep
    printmc (st, stCov)
    sep
    let State s p z = ukf ukfDefaultParam
                          (System sys obs sysNoise obsNoise)
                          (State st stCov undefined) 
                          Nothing
    printmc (s,p)
    sep
    let State s p z = ukf ukfDefaultParam
                          (System sys obs sysNoise obsNoise)
                          (State st stCov undefined) 
                          (Just $ vector [7.5,-0.5,7.5])
    printmc (s,p)
    sep
    let State s' p' z' = ukf ukfDefaultParam
                          (System sys obs sysNoise obsNoise)
                          (State s p z)
                           (Just $ vector [7.1,-0.2,7.8])
    printmc (s',p')

unscent f d v = unscentedTransform ukfDefaultParam (fromList . f . toList) (fromList v :: Vector Double, (d^2) `scale` ident (length v))
checksamp d v = unscentedSamples ukfDefaultParam (fromList v :: Vector Double, (d^2) `scale` ident (length v))

--f [x,y,z] = [x*y, x^2-z^2 , y*sin z]
f [x,y] = [x*y,x^2-y^2]
--f [x,y] = [sin x, cos x]

g a = toList . (rot a <>) . fromList

rot alpha = (2><2) [ cos alpha, sin alpha,
                    -sin alpha, cos alpha ]

------------------------------

sys [x,y,z,vx,vy,vz] = [x+vx,y+vy,z+vz,vx,vy,vz]
sysNoise = diagl [0,0,0,1,1,1]
obs  [x,y,z,vx,vy,vz] = [x+y,y-z,x+z]
obsNoise = diagl [1,2,3]

st = vector [3,4,5,0.1,0.2,0.3]
stCov = diagl [1,1,1,5,5,5]

