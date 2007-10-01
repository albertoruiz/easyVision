import Test.HUnit hiding (test,(~:))
import Numeric.LinearAlgebra
import Numeric.GSL
import Classifier
import Vision
import System.Random(randomRs,mkStdGen)
import System.Directory(doesFileExist)
import System(system)
import Control.Monad(when)
import Tensor

realMatrix = fromLists :: [[Double]] -> Matrix Double
realVector = fromList ::  [Double] -> Vector Double

randomMatrix seed (a,b) (n,m) = reshape m $ realVector $ take (n*m) $ randomRs (a,b) $ mkStdGen seed

infixl 2 =~=
a =~= b = pnorm PNorm1 (flatten (a - b)) < 1E-9

factorizeCameraTest m = normatdet m =~= normatdet m' where
    (k,r,c) = factorizeCamera m
    m' = k <> r <> ((ident 3 :: Matrix Double) <|> -c)

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
    ok <- doesFileExist ("data/mnist.txt")
    when (not ok)  $ do
        putStrLn "\nTrying to download test datafile..."
        system "wget -nv http://dis.um.es/~alberto/material/sp/mnist.txt.gz"
        system "gunzip mnist.txt.gz"
        system "mv mnist.txt data"
        return ()
    m <- fromFile "data/mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let (train',test') = (take n1 mnist, drop (5000-n2) mnist)

    let st = stat (fromRows $ map fst train')
    let f = encodeVector $ pca (ReconstructionQuality 0.8) st
    let (train,test) = (preprocess f train', preprocess f test')
    let c = fst $ distance mahalanobis train
    let e = errorRate test c
    let m = confusion test c
    let errors = (sum.toList.flatten $ m) - (sum.toList.takeDiag $ m)
    assertEqual ("classifyTest "++show (n1,n2)++", ") numErr errors

-------------------------------------------------------------------

ransacTest = (normat3 h1 =~= normat3 h) where
    orig = [[x,y] | x <- [-1, -0.9 .. 1], y <- [-1, -0.9 .. 1]]
    dest = ht h1 a ++ ht h2 b where (a,b) = splitAt 230 orig
    h1 = realMatrix [[1,0,0],
                    [0,1,0],
                    [0,0,1]]
    h2 = realMatrix [[2,0,-0.5],
                    [0,1,0],
                    [0.02,0,1]]
    h = fst $ estimateHomographyRansac 0.99 0.001 dest orig

--------------------------------------------------------------------

statTest = (whc =~= ident (rows whc)) &&
           (tc =~= ident (rows  tc)) where
    xs = randomMatrix 100 (10,20) (100,2) <|> randomMatrix 17 (-1,2) (100,2)
    st = stat xs
    whc = covarianceMatrix $ stat $ normalizedData st
    m = meanVector st
    w = whitener st
    f x = w<>(x-m)
    trans = fromRows $ map f $ toRows xs
    st' = stat trans
    tc = covarianceMatrix st'

-------------------------------------------------------------------

dist :: (Normed t, Num t) => t -> t -> Double
dist a b = pnorm Infinity (a-b)

infixl 4 |~|
a |~| b = a :~8~: b

data Aprox a = (:~) a Int

(~:) :: (Normed a, Num a) => Aprox a -> a -> Bool
a :~n~: b = dist a b < 10^^(-n)

---------------

infixl 8 !
t ! l = withIdx t (map return l)

infixl 7 <*>
(<*>) = mulT

tTrans m = tensor [rows m, -cols m] (flatten m)

eps3 = leviCivita 3
eps4 = raise $ leviCivita 4

tFundamental c1 c2 = eps3!"pqr" <*> eps4!"bcij" <*> c1!"pb" <*> c1!"qc" <*> c2!"si" <*> c2!"tj" <*> eps3!"stk"

c1 = syntheticCamera $ easyCamera 40 (0,0,0) (0.5,0,1) 0
ct1 = tTrans c1

c2 = syntheticCamera $ easyCamera 20 (1,0,0) (0.5,0.5,1) 0
ct2 = tTrans c2

f = fundamentalFromCameras c1 c2
tf = tFundamental ct1 ct2

tensorTest = normat f |~| normat (reshape 3 (coords tf))

-------------------------------------------------------------------

tests = TestList 
    [ test "factorize1"  (factorizeCameraTest m1)
    , test "factorize2"  (factorizeCameraTest m2)
    , test "pose1"       (poseEstimationTest  m1)
    , test "pose2"       (poseEstimationTest  m3)
    , test "pose3"       (poseEstimationTest  m4)
    , test "pose4"       (poseEstimationTest  m5)
    , test "pose5"       (poseEstimationTest  m6)
    , test "recover1"    (recoverFromHomogZ0Test  m1)
    , test "recover2"    (recoverFromHomogZ0Test  m3)
    , test "recover3"    (recoverFromHomogZ0Test  m4)
    , test "recover4"    (recoverFromHomogZ0Test  m5)
    , test "recover5"    (recoverFromHomogZ0Test  m6)
    , test "stat"        statTest
    , test "ransac"      ransacTest
    , TestCase        $  classifyTest 500 500 129 -- 4000 1000 63
    , test "tensor"      tensorTest
    ]

main = runTestTT tests

test str b = TestCase $ assertBool str b