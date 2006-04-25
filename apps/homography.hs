-- Necessity of normalization in homography estimation

module Main where

import Vision
import GSL 
import Stat

dest::Double->Double->[[Double]]
dest a b = [[0,0]
           ,[a,0]
           ,[0,a]
           ,[a-b,a-b]
           ,[a-b,a+b]
           ,[a+b,a+b]
           ,[a+b,a-b]]
             
orig::[[Double]]           
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

main = do
    print e1
    print e2