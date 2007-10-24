-- Necessity of normalization in the linear estimation of geometric relationships

module Main where

import Vision
import Numeric.LinearAlgebra
import Text.Printf(printf)

matrix m = fromLists m :: Matrix Double
vector v = fromList v  :: Vector Double

infixl 5 |-|, |+|
mat |-| vec = mat - constant 1 (rows mat) `outer` vec
mat |+| vec = mat + constant 1 (rows mat) `outer` vec

disp = putStrLn . format "  " (printf "%.3f")

dest a b = [[0,0]
           ,[a,0]
           ,[0,a]
           ,[a-b,a-b]
           ,[a-b,a+b]
           ,[a+b,a+b]
           ,[a+b,a-b]]

orig     = [[0,0]
           ,[1,0]
           ,[0,1]
           ,[1,1]
           ,[1,1]
           ,[1,1]
           ,[1,1]]

h1 = estimateHomographyRaw (dest 100 1) orig
h2 = estimateHomography (dest 100 1) orig

e1 = (matrix $ ht h1 orig) - 100 * matrix orig
e2 = (matrix $ ht h2 orig) - 100 * matrix orig


world = [[0, 2, -1]
        ,[0, 1, -1]
        ,[0, 0, -1]
        ,[0, 2,  0]
        ,[0, 1,  0]
        ,[0, 0,  0]
        ,[1, 2,  0]
        ,[1, 1,  0]
        ,[1, 0,  0]]

imageCen=[[-123,   2]
        ,[-107, -47]
        ,[ -87,-117]
        ,[ -81,  89]
        ,[ -57,  51]
        ,[ -23,  -5]
        ,[   5,  58]
        ,[  40,  18]
        ,[  91, -40]]

image = toLists $ matrix imageCen |+| vector [200,150]

m1 = estimateCameraRaw image world
m2 = estimateCamera image world
e3 = (matrix $ ht m1 world) - (matrix image)
e4 = (matrix $ ht m2 world) - (matrix image)

printm = disp

main = do
    putStrLn "homography estimation"
    printm (normat3 h1)
    printm (normat3 h2)
    printm e1
    printm e2
    print (pnorm PNorm1 $ flatten e1)
    print (pnorm PNorm1 $ flatten e2)
    putStrLn ""
    putStrLn "camera estimation"
    printm (normat3 m1)
    printm (normat3 m2)
    printm e3
    printm e4
    print (pnorm PNorm1 $ flatten e3)
    print (pnorm PNorm1 $ flatten e4)
    putStrLn ""
