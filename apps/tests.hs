import Test.HUnit
 
import GSL 
import Vision
import Stat
import Classifier

a =~= b = pnorm 1 (flatten (a - b)) < 1E-12

factorizeCameraTest m = normatdet m =~= normatdet m' where
    (k,r,c) = factorizeCamera m
    m' = k <> r <> (ident 3 <|> -c)

poseEstimationTest m = normatdet m =~= normatdet m' where
    Just pars = poseGen Nothing (homogZ0 m)
    m' = syntheticCamera pars
    Just pars' = poseGen Nothing (homogZ0 m')
    m'' = syntheticCamera pars'

m1 = -3 * (syntheticCamera $ easyCamera 2 (1,2,3) (0,3,0) (30*degree))
    
m2 = realMatrix [[1,0.1, 1],
                 [0,  2,-2],
                 [0,  0, 1]]  <> m1   
    
-------------------------------------------------------------------

classifyTest n1 n2 numErr = do
    m <- gslReadMatrix "mnist.txt" (5000,785)
    let rawproblem = prepareProblem (takeRows n1 m) (dropRows (5000-n2) m)
    let st = stat (designSamples rawproblem)
    let codec = pca (ReconstructionQuality 0.8) st
    let problem = preprocessProblem codec rawproblem
    let (f,e,m) = prepareClasif (mahalanobisDist.stat) problem
    let errors = (sum.toList.flatten $ m) - (sum.toList.diag $ m)
    assertEqual ("classifyTest "++show (n1,n2)++", ") numErr errors
        
-------------------------------------------------------------------

tests = TestList 
    [ TestCase (assertBool "factorize1" (factorizeCameraTest m1))
    , TestCase (assertBool "factorize2" (factorizeCameraTest m2))
    , TestCase (assertBool "pose"       (poseEstimationTest  m1))
    , TestCase $ classifyTest 500 500 129 
    --, TestCase $ classifyTest 4000 1000 63 
    ]
                 
main = runTestTT tests
