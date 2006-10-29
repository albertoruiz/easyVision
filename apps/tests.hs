import Test.HUnit

import GSL
import Vision
import System.Random(randomRs,mkStdGen)
import System.Directory(doesFileExist)
import System(system)
import Control.Monad(when)

realMatrix = fromLists :: [[Double]] -> Matrix Double
realVector = fromList ::  [Double] -> Vector Double

infixl 2 =~=
a =~= b = pnorm 1 (flatten (a - b)) < 1E-9

randomMatrix seed (n,m) = reshape m $ realVector $ take (n*m) $ randomRs (-100,100) $ mkStdGen seed 

factorizeCameraTest m = normatdet m =~= normatdet m' where
    (k,r,c) = factorizeCamera m
    m' = k <> r <> (ident 3 <|> -c)

recoverFromHomogZ0Test m = normat3 m =~= normat3 m' where
    c  = -5 * homogZ0 m
    Just m' = cameraFromHomogZ0 Nothing c

poseEstimationTest m = normatdet m =~= normatdet m' where
    Just pars = poseFromHomogZ0 Nothing (homogZ0 m)
    m' = syntheticCamera pars
    Just pars' = poseFromHomogZ0 Nothing (homogZ0 m')
    m'' = syntheticCamera pars'

m1 = -3 * (syntheticCamera $ easyCamera (40*degree) (1,2,3) (0,3,0) (30*degree))

m2 = realMatrix [[1,0.1, 1],
                 [0,  2,-2],
                 [0,  0, 1]]  <> m1

m3 = -3 * (syntheticCamera $ easyCamera (90*degree) (0,-2,2) (0,0,0) (5*degree))

m4 = syntheticCamera $ easyCamera (60*degree) (6,6,6) (0,0,0) (20*degree)

m5 = syntheticCamera (CamPar {focalDist = 2, panAngle=0.1, tiltAngle= -0.2, rollAngle=0.3, cameraCenter=(1,2,3)})

m6 = syntheticCamera (CamPar {focalDist = 3, panAngle= -0.1, tiltAngle=0.2, rollAngle= -0.3, cameraCenter=(1,-2,3)})

-------------------------------------------------------------------

classifyTest n1 n2 numErr = do
    ok <- doesFileExist ("examples/mnist.txt")
    when (not ok)  $ do
        putStrLn "\nTrying to download test datafile..."
        system("wget -nv http://dis.um.es/~alberto/material/sp/mnist.txt.gz")
        system("gunzip mnist.txt.gz")
        system("mv mnist.txt examples")
        return ()
    m <- fromFile "examples/mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let (train',test') = (take n1 mnist, drop (5000-n2) mnist)

    let st = stat (fromRows $ map fst train')
    let codec = pca (ReconstructionQuality 0.8) st
    let (train,test) = (preprocess codec train', preprocess codec test')
    let c = fst $ distance mahalanobis train
    let e = errorRate test c
    let m = confusion test c
    let errors = (sum.toList.flatten $ m) - (sum.toList.takeDiag $ m)
    assertEqual ("classifyTest "++show (n1,n2)++", ") numErr errors

-------------------------------------------------------------------

besselTest = do
    let (r,e) = bessel_J0_e 5.0
    let expected = -0.17759677131433830434739701
    assertBool "bessel_J0_e" ( abs (r-expected) < e ) 

exponentialTest = do
    let (v,e,err) = exp_e10_e 30.0
    let expected = exp 30.0
    assertBool "exp_e10_e" ( abs (v*10^e - expected) < 4E-2 ) 

-------------------------------------------------------------------

ransacTest = assertBool "ransac homography" (normat3 h1 =~= normat3 h) where
    orig = [[x,y] | x <- [-1, -0.9 .. 1], y <- [-1, -0.9 .. 1]]
    dest = ht h1 a ++ ht h2 b where (a,b) = splitAt 230 orig
    h1 = realMatrix [[1,0,0],
                    [0,1,0],
                    [0,0,1]]
    h2 = realMatrix [[2,0,-0.5],
                    [0,1,0],
                    [0.02,0,1]]
    h = estimateHomographyRansac 0.001 dest orig

-------------------------------------------------------------------

disp m = putStrLn (format " " show m)

ms = realMatrix [[1,2,3]
                ,[-4,1,7]]

ms' = randomMatrix 27 (50,100)

ms'' = toComplex (randomMatrix 100 (50,100),randomMatrix 101 (50,100))

fullsvdTest method mat msg = do
    let (u,s,vt) = method mat
    assertBool msg (u <> s <> trans vt =~= mat)

--------------------------------------------------------------------

mcu = toComplex (randomMatrix 33 (20,20),randomMatrix 34 (20,20))

mcur = randomMatrix 35 (40,40)

-- eigenvectors are columns
eigTest method m msg = do
    let (s,v) = method m
    assertBool msg $ m <> v =~= v <> diag s

--------------------------------------------------------------------

tests = TestList 
    [ TestCase (assertBool "factorize1" (factorizeCameraTest m1))
    , TestCase (assertBool "factorize2" (factorizeCameraTest m2))
    , TestCase (assertBool "pose1"       (poseEstimationTest  m1))
    , TestCase (assertBool "pose2"       (poseEstimationTest  m3))
    , TestCase (assertBool "pose3"       (poseEstimationTest  m4))
    , TestCase (assertBool "pose4"       (poseEstimationTest  m5))
    , TestCase (assertBool "pose5"       (poseEstimationTest  m6))
    , TestCase (assertBool "recover1"    (recoverFromHomogZ0Test  m1))
    , TestCase (assertBool "recover2"    (recoverFromHomogZ0Test  m3))
    , TestCase (assertBool "recover3"    (recoverFromHomogZ0Test  m4))
    , TestCase (assertBool "recover4"    (recoverFromHomogZ0Test  m5))
    , TestCase (assertBool "recover5"    (recoverFromHomogZ0Test  m6))
    , TestCase $ besselTest
    , TestCase $ exponentialTest
    , TestCase $ ransacTest
    , TestCase $ eigTest eigC mcu "eigC" 
    , TestCase $ eigTest eigR mcur "eigR"
    , TestCase $ eigTest eigS (mcur+trans mcur) "eigS"
    , TestCase $ eigTest eigSg (mcur+trans mcur) "eigSg"
    , TestCase $ eigTest eigH (mcu+ (conj.trans) mcu) "eigH"
    , TestCase $ eigTest eigHg (mcu+ (conj.trans) mcu) "eigHg"
    , TestCase $ fullsvdTest svdR ms "fullsvdR small"
    , TestCase $ fullsvdTest svdR (trans ms) "fullsvdR small"
    , TestCase $ fullsvdTest svdR ms' "fullsvdR"
    , TestCase $ fullsvdTest svdR (trans ms') "fullsvdR"
    , TestCase $ fullsvdTest full_svd_Rd ms' "fullsvdRd"
    , TestCase $ fullsvdTest full_svd_Rd (trans ms') "fullsvdRd"
    , TestCase $ fullsvdTest svdC ms'' "fullsvdC"
    , TestCase $ fullsvdTest svdC (trans ms'') "fullsvdC"
    , TestCase $ classifyTest 500 500 129 
    --, TestCase $ classifyTest 4000 1000 63
    ]

main = runTestTT tests

