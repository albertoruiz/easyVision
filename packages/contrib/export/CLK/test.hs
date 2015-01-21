import HTools

import Contours
import Numeric.LinearAlgebra.HMatrix

p = fst (last (pentominos))

t = transPol (diagl [1.1, 0.9, 1]) p

main = do
    print (fun t p)

