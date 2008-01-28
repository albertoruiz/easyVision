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

tri = trifocal ct2 ct3 ct1

linQuad tri = tri!"kij" <*> tv[0,0,1]!"a" <*> eps3!"ajJ" <*> tri!"KIJ" <*> eps3!"iIr"

lin tri p = linQuad tri <*> p!"k" <*> p!"K"

epitri tri = epi where
    epi = lin tri (tv[0,0,1]) !"a" <*> lin tri (tv[1,0,0])!"b" <*> (raise eps3)!"abc"

tri2fun12 tri epi2 = tri!"kij" <*> tc[0,0,1]!"j" <*> eps3!"ier" <*> epi2!"e"

getCams12 tri = (p1,p2) where
    p1 = cameraAtOrigin
    ep = epitri tri
    added = (coords ep) `outer` vector[0,0,0,1]
    tcam = tTrans p1!"ka" <*> tri!"kij" <*> tc[0,0,1]!"j"
    cam = trans $ reshape 3 $ coords tcam
    p2 = cam + added

algoCam3' tri x = tri!"kij" <*> i1!"k" <*> eps3!"ibc" <*> tv[4,5,7]!"c" <*> i2!"b"
    where i1 = tTrans p1!"ab" <*> x!"b"
          i2 = tTrans p2!"ab" <*> x!"b"
          (p1,p2) = trace "\nP1P2\n" $ getCams12 tri

algoCam3 tri = f
    where f x = tri!"kij" <*> i1!"k" <*> eps3!"ibc" <*> tv[4,5,7]!"c" <*> i2!"b"
              where i1 = tTrans p1!"ab" <*> x!"b"
                    i2 = tTrans p2!"ab" <*> x!"b"
          (p1,p2) = {-trace "\nP1P2\n" $-} getCams12 tri


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
    putStrLn "--------------"
    let (p1,p2) = getCams12 tri
    disp $ fundamentalFromCameras p1 p2
    putStrLn "--------------"
    let images = map (toList.inHomog.coords.algoCam3 tri.tv.(++[1])) somePoints
    --print images
    let p3 = estimateCameraRaw images somePoints
    disp p3
    dispT $ trifocal (tTrans p2) (tTrans p3) (tTrans p1)
    dispT tri


cosa = eps4!"abcd" <*> p!"xa" <*> p!"yb" <*> eps3!"xyz"
    where p = tTrans cameraAtOrigin