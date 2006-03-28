-- the minimization example for nmsimplex in the GSL manual

module Main where

import GSL
import GSL.Wrappers

f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30

df [x,y] = [20*(x-1), 40*(y-2)+1]

minimize f xi = minimizeNMSimplex f xi (replicate (length xi) 1) 1e-2 100

conjugrad f df xi = minimizeDerivV (f.toList)  (fromList.df.toList) 0.1 100 (fromList xi)

main = do
    let (s,p) = minimize f [5,7]
    print s
    print p
    let [x,y] = drop 3 (toCols p)
    hplot [x,y]
    print $ conjugrad f df [5,7]
