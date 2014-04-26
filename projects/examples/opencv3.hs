-- findHomography from OpenCV does not get minimum RMS error?

import Devel.Vision.DLT
import OpenCV (findHomography)
import Numeric.LinearAlgebra
import Util.Homogeneous((!<>))
import Util.Geometry
import Vision
import Numeric.LinearAlgebra.Util(norm)

a = (5><2)
 [0,0
 ,1,0
 ,1,1
 ,0,1
 ,0,1.5] :: Matrix Double

 
t = (3><3)
 [ 1,2,3
 , 0,2,4
 , 2,0,1 ] :: Matrix Double


b = a !<> trans t + (5><2) [0,0,0,0,0,0,0,0,0,0.5]
 
h1 = findHomography b a 
h2 = estimateHomography (toLists b) (toLists a) 
 
main = do
    print (rank t)
    print b
    print h1
    print (normat3 h2)
    print $ norm $ flatten $ (a!<>h1-b)
    print $ norm $ flatten $ (a!<>h2-b)

