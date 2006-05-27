-- our nice kernel MSE algorithm
-- compile with -O

-- in ghc-6.4.1 there are misterious problems with the required full laziness
-- this should work well in ghc-6.4.2
-- if not, in main change n=100 to n=10 

module Main where

import GSL 
import System.Environment (getArgs)

kernelmatrix kernel objs = reshape (length objs) $ fromList [kernel x y | x <- objs, y <- objs]
    
kernelMSE :: [x] -> [Double] -> (x -> x -> Double) -> ( x-> Double)
kernelMSE objs labels kernel = fun where
    fun z = expan z <> a
    a = pinv (kernelmatrix kernel objs) <> fromList labels
    expan z = fromList $ map (kernel z) objs
    
    
kernelMSEgen :: Int -> Double -> [x] -> [Double] -> (x-> x -> Double) -> (x->Double)
kernelMSEgen step tol objs labels kernel = fun where
    fun z = expan z <> a
    expan z = fromList $ map (kernel z) subobjs
    subobjs = [ x | (x,k) <- zip objs [0 ..], k `mod` step ==0]
    k = reshape (length subobjs) $ fromList [kernel x y | x <- objs, y <- subobjs]
    a = pinvTol tol k <> fromList labels
    info = "learn "++ (show $ length subobjs)++", "++(show s)
    (_,s,_) = svd k
     
    
polykernel :: Int -> Vector -> Vector -> Double    
polykernel n x y = (x<>y + 1)^n

-- faster (!?)
polykernel' :: Int -> [Double] -> [Double] -> Double    
polykernel' n [a,b] [x,y] = (a*x+b*y + 1)^n

gausskernel s [a,b] [x,y] = exp (- ((a-x)^2+(b-y)^2)/ s^2)

main = do
    -- 60 example vectors
    let m = 20
    let ts = toList $ linspace m (0,2*pi)
    let dat1 = [ [2*cos t, 2*sin t] | t <- ts ]
    let dat2 = [ [cos t, sin t] | t <- ts ]
    let dat3 = [ [0.3*cos t, 0.3*sin t] | t <- ts ]
    let exs = dat1 ++ dat2 ++ dat3
    let labs = replicate m (-1) ++ replicate m (1) ++ replicate m (-1)

    -- 100 x 100 = 10000 test vectors 
    let n = 100
    let x = toList $ linspace n (-2.5,2.5)
    let y = toList $ linspace n (-2.5,2.5)

    --args <- getArgs
    --let frac = read (args!!0) :: Int
    --let tol  = read (args!!1) :: Double
    --let sigma = read (args!!2)
    let frac = 1
    let tol = 1.0
    let sigma = 2.0
    
    let g1 = kernelMSE exs labs (polykernel' 5)
    let z1 = reshape n $ realVector [tanh $ g1 [a,b] | a <- x, b <- y]
    imshow z1
    let g2 = kernelMSEgen frac tol exs labs (gausskernel sigma)
    let z2 = reshape n $ realVector [tanh $ g2 [a,b] | a <- x, b <- y]
    imshow z2
    