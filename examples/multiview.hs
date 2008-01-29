import Numeric.LinearAlgebra
import Vision
import Tensor
import Text.Printf(printf)
import Debug.Trace(trace)

debug x = trace (show x) x

disp' :: Int -> Matrix Double -> IO ()
disp' n = putStrLn . format "  " (printf $ "%."++ show n++"f") . normat

disp = disp' 2

dispT' :: Int -> Tensor Double -> IO ()
dispT' n t = disp' n . reshape c . coords $ t
    where c = idxDim $ last (dims t)

dispT = dispT' 2

dispV' :: Int -> Vector Double -> IO ()
dispV' n = putStrLn . concatMap (printf $ "  %."++ show n++"f") . toList . unitary

dispV = dispV' 2

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

-- tensor from matrix as transformation
tTrans m = tensor [rows m, -cols m] (flatten m)

tvector v   = tensor [ dim v]  v  -- contravariant
tcovector v = tensor [-dim v] v   -- covariant

vector x = fromList x :: Vector Double

tv = tvector . vector
tc = tcovector . vector 

eps3 = leviCivita 3
eps4 = raise $ leviCivita 4

tFundamental c1 c2 = eps3!"pqr" <*> eps4!"bcij" <*> c1!"pb" <*> c1!"qc" <*> c2!"si" <*> c2!"tj" <*> eps3!"stk"

c1 = syntheticCamera $ easyCamera 40 (0,0,0) (0.5,0,1) 0
ct1 = tTrans c1

c2 = syntheticCamera $ easyCamera 20 (1,0,0) (0.5,0.5,1) 0
ct2 = tTrans c2

f12 = fundamentalFromCameras c1 c2
tf12 = tFundamental ct1 ct2 ! "12"

c3 = syntheticCamera $ easyCamera 20 (1,-2,-3) (0.5,-0.5,2) 0
ct3 = tTrans c3

f13 = fundamentalFromCameras c1 c3
tf13 = tFundamental ct1 ct3 ! "13"

f23 = fundamentalFromCameras c2 c3
tf23 = tFundamental ct2 ct3 ! "23"

trifocal m n p = eps4!"abcd" <*> m!"ia" <*> n!"jb" <*> p!"pc" <*> p!"qd" <*> eps3!"pqk"

tri = trifocal ct2 ct3 ct1 -- c' c'' c

linQuad tri = tri!"kij" <*> tv[0,0,1]!"a" <*> eps3!"ajJ" <*> tri!"KIJ" <*> eps3!"iIr"

lin tri p = linQuad tri <*> p!"k" <*> p!"K"

-- the epipole e' is computed as intersection of...
epitri tri = epi where
    epi = lin tri (tv[0,0,1]) !"a" <*> lin tri (tv[1,0,0])!"b" <*> (raise eps3)!"abc"

-- the fundamental 12 is an arbitrary transfer joined to the epipole
tri2fun12 tri epi2 = tri!"kij" <*> tc[0,0,1]!"j" <*> eps3!"ier" <*> epi2!"e"


getCams12 tri = canonicalCameras $ reshape 3 $ coords f
    where f = tri2fun12 tri epi2
          epi2 = epitri tri

-- but the cameras 1 2 can also be directly obtained by an arbitrary transfer plus "range restoration"
getCams12' tri = (p1,p2) where
    p1 = cameraAtOrigin
    ep = epitri tri
    added = (coords ep) `outer` vector[0,0,0,1]
    tcam = tTrans p1!"ka" <*> tri!"kij" <*> tc[0,0,1]!"j"
    cam = trans $ reshape 3 $ coords tcam
    p2 = cam + added

-- the 3rd camera can be obtained in "algorithmical form" from the images of 1 and
-- transfer through an arbitrary line in 2 passing by the im of 2.
-- x appears in two places.
-- it could be used to estimate a camera from the images of arbitrary points

quadCam3 tri = tri!"kij" <*> tTrans p1!"kx" <*> eps3!"ibc" <*> tv[4,-5,7]!"c" <*> tTrans p2!"by"
    where (p1,p2) = getCams12 tri

algoCam3 tri = f
    where q = quadCam3 tri
          f x = q!"jxy" <*> x!"x" <*> x!"y"

semiTriangulation cam1 cam2 = eps3!"x23" <*> cam1!"24" <*> cam1!"35" <*> eps4!"456z" <*> cam2!"76" <*> eps3!"78y" <*> tv[1,2,3]!"8"

somePoints = [
    [1,2,3],
    [1,-2,3],
    [-2,1,0],
    [7,7,-3],
    [2,1,1],
    [2,-1,1]]

main = do
    disp c1
    dispT ct1
    disp f12
    dispT tf12
    putStrLn "--------------"
    let epi2 = epitri tri
    dispV $ c2 <> nullVector c1
    dispT $ tvector $ snd $ epipoles f12
    dispT $ tri2fun12 tri epi2
    putStrLn "--------------"
    let (p1,p2) = canonicalCameras f12
    disp $ fundamentalFromCameras p1 p2
    putStrLn "--------getCams12------"
    let (p1,p2) = getCams12 tri
    dispV $ nullVector p1
    dispV $ nullVector p2
    disp $ fundamentalFromCameras p1 p2
    putStrLn "--------the third camera------"
    let images = map (toList.inHomog.coords.algoCam3 tri.tv.(++[1])) somePoints
    --print images
    let p3 = estimateCameraRaw images somePoints
    disp p3
    dispV $ nullVector p3
    print (rank p3)
    dispT $ trifocal (tTrans p2) (tTrans p3) (tTrans p1)
    dispT tri
    putStrLn "--------------"
    dispV $ nullVector p1
    dispV $ nullVector p2
    dispV $ nullVector p3
    putStr "c3 semitriang: "
    let a = tvector $ p1 <> nullVector p3
    let a' = tvector $ p2 <> nullVector p3
    dispT $ semiTriangulation (tTrans p1) (tTrans p2) <*> a!"x" <*> a'!"y"
    let epi2 = epitri (trifocal ct3 ct2 ct1)
    let [x1,x2,x3] = map (coords.algoCam3 tri.tv)[[1,0,0,0],[0,1,0,0],[0,0,1,0]]
        x = fromColumns [x1,x2,x3, coords epi2]
    --disp x
    dispV' 5 $ nullVector x
    let alpha = nullVector x / nullVector p3
    dispV' 5 alpha
    dispV' 5 $ nullVector (x <> diag alpha)
    disp $ x <> diag alpha
    disp p3

-- so we only need C3 (by triangulation?) (and epi'')
-- But the ratio system only works if C3 does not any zero coord... (!?)

nearlyTrifocal m p = eps4!"abcd" <*> m!"ia" <*> p!"pc" <*> p!"qd" <*> eps3!"pqk"
