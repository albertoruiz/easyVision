-- Necessity of normalization in the linear estimation of geometric relationships

module Main where

import Vision
import GSL 
import Stat
import Data.List(genericLength)

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

e1 = (realMatrix $ ht h1 orig) - 100 * realMatrix orig
e2 = (realMatrix $ ht h2 orig) - 100 * realMatrix orig 


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

image = toList $ realMatrix imageCen |+| realVector [200,150]

m1 = estimateCameraRaw image world
m2 = estimateCamera image world
e3 = (realMatrix $ ht m1 world) - (realMatrix image)
e4 = (realMatrix $ ht m2 world) - (realMatrix image)


main = do
    print (normat3 h1)
    print (normat3 h2)
    print e1
    print e2
    print (pnorm 1 $ flatten e1)
    print (pnorm 1 $ flatten e2)
    putStrLn ""
    print (normat3 m1)
    print (normat3 m2)
    print e3
    print e4
    print (pnorm 1 $ flatten e3)
    print (pnorm 1 $ flatten e4)
    putStrLn ""
    correspondences <- fromFile "stereo.txt"
    let leftpts  = toList $ takeColumns 2 correspondences
    let rightpts = toList $ dropColumns 2 correspondences
    let f = estimateFundamental leftpts rightpts
    disp 8 (normat f)
    print $ mean $ epipolarQuality f leftpts rightpts
    let fs = linspace 500 (50,200)
    hplot [fs, vmap (qualityOfInducedEssential f) fs]
    print $ minimize (\[x]-> qualityOfInducedEssential f x) [170.0]

mean l = sum l / genericLength l

minimize f xi = minimizeNMSimplex f xi (replicate (length xi) 1) 1e-2 100
