-- the minimization example for nmsimplex in the GSL manual

module Main where

import GSL
import GSL.Util

f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30

minimize f xi = minimizeNMSimplex f xi (replicate (length xi) 1) 1e-2 100

main = print $ minimize f [5,7]
