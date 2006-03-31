-- the minimization example for nmsimplex and conjugate_fr in the GSL manual

module Main where

import GSL

f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30

df [x,y] = [20*(x-1), 40*(y-2)]

minimize f xi = minimizeNMSimplex f xi (replicate (length xi) 1) 1E-2 100

conjugrad f df xi = minimizeConjugateGradient 1E-2 1E-4 1E-3 30 
                                              (f.toList1) 
                                              (fromList1.df.toList1) 
                                              (fromList1 xi)

partialDerivative n f v = fst (derivCentral 0.01 g (v!!n)) where
    g x = f (concat [a,x:b])
    (a,_:b) = splitAt n v
        
gradient f v = [partialDerivative k f v | k <- [0 .. length v -1]]

main = do
    let (s,p) = minimize f [5,7]
    print s
    print p
    let [x,y] = drop 3 (toColumns p)
    hplot [x,y]
    let (s,p) = conjugrad f df [5,7]
    print s
    print p
    hplot $ drop 2 (toColumns p)
    let (s,p) = conjugrad f (gradient f) [5,7]
    print s
    print p
    hplot $ drop 2 (toColumns p)
