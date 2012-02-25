-- bayes inference for gaussians

import Numeric.LinearAlgebra
import Util.Gaussian
import Util.Misc(vec,mat)

----------------------------------------------------------------------

main = test

disp = putStr . dispf 2

z = N me co
  where
    me = vec [1,2,3,4]
    
    co = (4><4) [4, 2, 1, 0
                ,2, 4, 2, 1
                ,1, 2, 4, 2
                ,0, 1, 2, 4]


test = do
    print $ conditional (vec [3]) $ jointLinear (N 0 4) 2 (N 5 1)
    print $ bayesGaussianLinear (vec [3]) (N 0 4) 2 (N 5 1)
    print $ bayesGaussianLinearK (vec [3]) (N 0 4) 2 (N 5 1)
    putStrLn "--------------------"
    let h = (2><4) [1,0,2,-3,
                    0,3,-1,7]
        o = vec [-1,3]
        r = (2><2) [4,1,1,4]
        y = vec [3,5]
    print $ jointLinear z h (N o r)
    print $ conditional y $ jointLinear z h (N o r)
    print $ bayesGaussianLinear y z h (N o r)
    print $ bayesGaussianLinearK y z h (N o r)

