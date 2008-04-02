-- experiments with Geometric Algebra

import GeometricAlgebra

a = mv [2,3,0]
b = e 1 + e 3


infixl 7 \/
a \/ b = i*((i*b) /\ (i*a))
    where i = product (map e [1..4])

po = mv[0,0,0,1]
px = mv[1,0,0,1]
py = mv[0,1,0,1]
pz = mv[0,0,1,1]

l1 = po /\ px
l2 = py /\ pz
l3 = po /\ py

l4 = mv[2,2,1,1] /\ mv[3,3,2,1]

pTest = (po /\ px /\ py) \/ l4

aa = 1/2 * (e 1 + e 2) /\ (e 2 + e 3)
bb = e 1 /\ e 2
pInt = meet 3 aa bb

main = do
     print $ meet 4 l4 ((e 1 + e 4) /\ (e 2 + e 4)/\ (e 4))
     print $ meet 4 l4 ((e 1 + e 3 + e 4) /\ (e 2 + e 3 + e 4)/\ (e 3 + e 4))
     let r = rotor 3 (pi/4) (e 3)
     print r
     print (1/r)
     print (rever r)
     let l4' = r * l4 * rever r -- or r * l4 / r if r is not unitary
     print l4
     print l4'
     print $ meet 4 l4' ((e 1 + e 4) /\ (e 2 + e 4)/\ (e 4))
     print $ meet 4 l4' ((e 1 + e 3 + e 4) /\ (e 2 + e 3 + e 4)/\ (e 3 + e 4))
