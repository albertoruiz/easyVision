-- our nice kernel MSE algorithm
-- compile with -O

-- in ghc-6.4.1 there are misterious problems with the required full laziness
-- this should work well in ghc-6.4.2
-- if not, in main change n=100 to n=10 

module Main where

import GSL 
import System.Environment (getArgs)

matrix = realMatrix
vector = realVector

partit _ [] = []
partit n l  = take n l : partit n (drop n l)

table f l1 l2 = partit (length l1) $ [f x y | x <- l1, y <- l2]

kernelMSE :: (x -> x -> Double) -> [x] -> [Double] -> ( x-> Double)
kernelMSE kernel objs labels = fun where
    fun z = expan z <> a
    a = {-# SCC "pinv" #-} pinv (matrix (table kernel objs objs)) <> vector labels
    expan z = vector $ map (kernel z) objs
    
    
kernelMSEgen :: (x-> x -> Double) -> Int -> Double -> [x] -> [Double] -> (x->Double)
kernelMSEgen kernel step tol objs labels = fun where
    fun z = expan z <> a
    expan z = vector $ map (kernel z) subobjs
    subobjs = [ x | (x,k) <- zip objs [0 ..], k `mod` step ==0]
    k = matrix (table kernel objs subobjs)
    a = {-# SCC "pinvTol" #-} pinvTol tol k <> fromList labels 
    
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
    
    let g1 = kernelMSE (polykernel' 5) exs labs
    let z1 = table f x y where f a b = tanh (g1 [a,b])
    imshow (matrix z1)
    let g2 = kernelMSEgen (gausskernel sigma) frac tol exs labs
    let z2 = table f x y where f a b = tanh (g2 [a,b])
    imshow (matrix z2)
    