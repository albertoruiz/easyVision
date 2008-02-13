import Numeric.LinearAlgebra
import Vision
import Tensor
import Text.Printf(printf)
import Debug.Trace(trace)

debug x = trace (show x) x

debugm m x = trace (m ++ show x) x

norm = pnorm PNorm2

distH t1 t2 = min (norm $ coords (nt1 - nt2))
                  (norm $ coords (nt1 + nt2))
    where nt1 = norten t1
          nt2 = norten t2


norten t = scalar (1/r) * t
    where r = pnorm PNorm2 (coords t)

-- or liftTensor unitary


disp' :: Int -> Matrix Double -> IO ()
disp' n = putStrLn . format "  " (printf $ "%."++ show n++"f") . normat

disp = disp' 2

dispS msg m = putStrLn (msg++" = ") >> disp m

dispT' :: Int -> Tensor Double -> IO ()
dispT' n t = do
    putStrLn $ unwords $ map (show) (dims t)
    disp' n . reshape c . coords $ t
  where c = idxDim $ last (dims t)

dispT = dispT' 2

dispTS msg t = putStr (msg++" = ") >> dispT t

dispV' :: Int -> Vector Double -> IO ()
dispV' n = putStrLn . (++"\n") . concatMap (printf $ "  %."++ show n++"f") . toList . unitary

dispV = dispV' 2

dispVS msg v = putStr (msg++" = ") >> dispV v

mat rowidx t = reshape c $ coords t'
    where c = idxDim $ last (dims t')
          t' = tridx [rowidx] t


---------------

-- tensor from matrix as transformation
tTrans m = tensor [rows m, -cols m] (flatten m)

tvector v   = tensor [ dim v]  v  -- contravariant
tcovector v = tensor [-dim v] v   -- covariant

vector x = fromList x :: Vector Double

tv = tvector . vector
tc = tcovector . vector 

eps3 = leviCivita 3
eps4 = raise $ leviCivita 4

tFundamental c1 c2 = eps3!"pqr" * eps4!"bcij" * c1!"pb" * c1!"qc" * c2!"si" * c2!"tj" * eps3!"stk"

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

trifocal m n p = eps4!"abcd" * m!"ia" * n!"jb" * p!"pc" * p!"qd" * eps3!"pqk"

tri = trifocal ct2 ct3 ct1 -- c' c'' c --> i j k

linQuad tri = tri!"kij" * tv[0,0,1]!"a" * eps3!"ajJ" * tri!"KIJ" * eps3!"iIr"

lin tri p = linQuad tri * p!"k" * p!"K"

-- the epipole e' is computed as intersection of...
epitri tri = epi where
    epi = lin tri (tv[0,0,1]) !"a" * lin tri (tv[1,0,0])!"b" * (raise eps3)!"abc"

-- the fundamental 12 is an arbitrary transfer joined to the epipole
tri2fun12 tri epi2 = tri!"kij" * tc[0,0,1]!"j" * eps3!"ier" * epi2!"e"


-- canonicalCameras should be changed to obtain always the same p2...
-- The standard method depends on the scale (and sign) of f.
getCams12 tri = canonicalCameras $ reshape 3 $ coords f
    where f = tri2fun12 tri epi2
          epi2 = epitri tri

-- the cameras 1 2 can also be directly obtained by an arbitrary transfer plus "range restoration"
getCams12' tri = (p1,p2) where
    p1 = cameraAtOrigin
    ep = epitri tri
    added = (coords ep) `outer` vector[0,0,0,1]
    tcam = tTrans p1!"ka" * tri!"kij" * tc[0,0,1]!"j"
    cam = trans $ reshape 3 $ coords tcam
    p2 = cam + added

-- but then c3 goes to infinity... 


-- the 3rd camera can be obtained in "algorithmical form" from the images of 1 and
-- transfer through an arbitrary line in 2 passing by the im of 2.
-- x appears in two places.
-- it could be used to estimate a camera from the images of arbitrary points, but the
-- third camera can be also obtained directly from the tensor given the image of the
-- three ideal points and the origin, adjusting the scaling factors to get the center,
-- which can be reconstructed from p1 and p2.

quadCam3 tri = tri!"kij" * tTrans p1!"kx" * eps3!"ibc" * tv[4,-5,7]!"c" * tTrans p2!"by"
    where (p1,p2) = getCams12 tri

algoCam3 tri = f
    where q = quadCam3 tri
          f x = q!"jxy" * x!"x" * x!"y"

-- better indices 12 instead of xy
semiTriangulation cam1 cam2 = eps3!"x23" * cam1!"24" * cam1!"35" * eps4!"456z" * cam2!"76" * eps3!"78y" * tv[1,2,3]!"8"



analyzeTrifocal' tri = do
    let e1 = epitri tri
    dispTS "e1" e1
    let f12 = tri!"kij" * tc[0,0,1]!"j" * eps3!"ier" * e1!"e"
    dispTS "f12" f12
    print (rank $ reshape 3 $ coords f12)
    let e2 = epitri (tridx ["k","j","i"] tri)
    dispTS "e2" e2
    let (p1,p2) = getCams12 tri -- replace by the transfer version
    dispS "p2" p2
    let c2 = nullVector p2
    dispVS "c2" c2
    let f13 = tri2fun12 (tridx ["k","j","i"] tri) e2
    dispTS "f13" f13
    let a = nullVector $ reshape 3 $ coords f13 -- replace by version with contractions
    dispVS "a" a
    let a' = tri!"kij" * tvector a!"k" * tc[13,17,19]!"j"
    dispTS "a'" a'
    let intersector = semiTriangulation (tTrans p1) (tTrans p2)
    dispTS "intersector" intersector
    let c3 = intersector * tvector a!"x" * a'!"y"
    dispTS "c3" c3
    let quadCam3 = tri!"kij" * tTrans p1!"kx" * eps3!"ibc" * tv[4,-5,7]!"c" * tTrans p2!"by"
        algoCam3 x  = quadCam3!"jxy" * x!"x" * x!"y"
        [x1,x2,x3] = map (coords.algoCam3.tv)[[1,0,0,0],[0,1,0,0],[0,0,1,0]]
        x = fromColumns [x1,x2,x3, coords e2]
    dispS "x" x
    dispVS "cx" $ nullVector x
    let alpha = nullVector x / coords c3 -- replace by version with contractions
    dispVS "alpha" alpha
    let p3 = x <> diag alpha
    dispS "p3" p3
    putStr "ranks = " >> (print $ map rank [p1,p2,p3]) >> putStrLn ""
    let recontri = trifocal (tTrans p2) (tTrans p3) (tTrans p1)
    dispTS "reconstructed trifocal tensor" recontri
    putStr "error = " >> print (distH tri recontri) >> putStrLn ""
    return (p1,p2,p3)

-- we need nullVector of camera (3x4, using "contractions") 
-- and nullVector from fundamental (3x3, using "intersections")


-- The algorithm in Hartley & Zisserman (debugging version)
camerasFromTrifocalHZ tri = do
    let p1 = cameraAtOrigin
    let e1 = norten $ epitri tri
    dispTS "e1" e1
    let e2 = norten $ epitri (tridx ["k","j","i"] tri)
    dispTS "e2" e2
    let p2 = (mat "i" $ tri!"kij"* raise e2!"j") <|> coords e1    -- (e in l slot, in HZ)
    disp p2
    let p3 = (mat "i" (e2!"i"*e2!"j") - ident 3) <> (mat "j" $ tri!"kij"* raise e1!"i") <|> coords e2
    disp p3
    putStr "ranks = " >> (print $ map rank [p1,p2,p3]) >> putStrLn ""
    let recontri = trifocal (tTrans p2) (tTrans p3) (tTrans p1)
    dispTS "reconstructed trifocal tensor" recontri
    putStr "error = " >> print (distH tri recontri) >> putStrLn ""
    dispVS "C2" $ nullVector p2
    dispVS "C3" $ nullVector p3
    return (p1,p2,p3)


analyzeTrifocal'' tri = do
    let p1 = cameraAtOrigin
    let e1 = epitri tri
    dispTS "e1" e1
    let e2 = epitri (tridx ["k","j","i"] tri)
    dispTS "e2" e2
    let someline = raise e2!"j" -- tc[2,5,-7]!"j"
    let p2 = (mat "i" $ tri * someline) <|> coords e1
    disp p2
    let otherline = raise e1!"i"-- tc[1,2,3]!"i"
    let x = (mat "j" $ tri * otherline) <|> coords e2
    disp x
    let trichun = trifocal (tTrans p2) (tTrans x) (tTrans p1)
        anyline = raise e1!"i"-- tc[1,-2,3]!"i"
        hv = mat "j" $ tri * anyline
        hx = mat "j" $ trichun * anyline
        c = hv <> inv hx
        p3 = c <> x
        recontri = trifocal (tTrans p2) (tTrans p3) (tTrans p1)
    printf "inv of condition: %f\n" (rcond hx)
    dispTS "reconstructed trifocal tensor" recontri
    putStr "error = " >> print (distH tri recontri) >> putStrLn ""
    return (p1,p2,p3)


-- this is good and understandable...
analyzeTrifocal tri = do
    let e1 = epitri tri
    dispTS "e1" e1
    let f12 = tri!"kij" * tc[0,0,1]!"j" * eps3!"ier" * e1!"e"
    dispTS "f12" f12
    print (rank $ reshape 3 $ coords f12)
    let e2 = epitri (tridx ["k","j","i"] tri)
    dispTS "e2" e2
    let (p1,p2) = getCams12 tri -- replace by the transfer version
    dispS "p2" p2
    let c2 = nullVector p2
    dispVS "c2" c2
    let f13 = tri2fun12 (tridx ["k","j","i"] tri) e2
    dispTS "f13" f13
    let a = nullVector $ reshape 3 $ coords f13 -- replace by version with contractions
    dispVS "a" a
    let a' = tri!"kij" * tvector a!"k" * tc[13,17,19]!"j"
    dispTS "a'" a'
    let intersector = semiTriangulation (tTrans p1) (tTrans p2)
    dispTS "intersector" intersector
    let c3 = intersector * tvector a!"x" * a'!"y"
    dispTS "c3" c3
    let x = cameraAtOrigin <> desp4' (coords c3)
    dispS "some camera at C3" x
    dispVS "cx" $ nullVector x
    let trichun = trifocal (tTrans p2) (tTrans x) (tTrans p1)
        anyline = raise e1!"i"-- tc[1,-2,3]!"i"
        hv = mat "j" $ tri * anyline
        hx = mat "j" $ trichun * anyline
        c = hv <> inv hx
        p3 = c <> x
        recontri = trifocal (tTrans p2) (tTrans p3) (tTrans p1)
    printf "inv of condition to match some homography: %f\n" (rcond hx)
    putStr "ranks = " >> (print $ map rank [p1,p2,p3]) >> putStrLn ""
    dispTS "reconstructed trifocal tensor" recontri
    putStr "error = " >> print (distH tri recontri) >> putStrLn ""
    return (p1,p2,p3)


desp4' v = (4><4) [ d, 0, 0, -a,
                    0, d, 0, -b,
                    0, 0, d, -c,
                    0, 0, 0, d ]
    where [a,b,c,d] = toList v


main = do
    putStrLn "-----Hartley & Zisserman ---------"
    (p1,p2,p3) <- camerasFromTrifocalHZ tri
    putStrLn "-----With C3 ------------"
    analyzeTrifocal tri
    putStrLn "-----With C3, and ratios from vanishing points"
    analyzeTrifocal' tri
    putStrLn "-----With aribitrary homography"
    analyzeTrifocal'' tri
    return ()


--------------- other experiments -------------

po = tv[0,0,0,1]
px = tv[1,0,0,1]
py = tv[0,1,0,1]
pz = tv[0,0,1,1]

r1 = po /\ px
r2 = py /\ pz
r3 = po /\ py

r4 = tv[2,2,1,1] /\ tv[3,3,2,1]

pTest = (po /\ px /\ py) \/ r4


-- recovery of a fundamental matrix from the trifocal tensor from the "algorithmic" transfer.
-- If it worked for ordinary coordinates the tensor coordinates would be just the images of
-- the inputs bases. But it is obtained by means of geometric arguments, and we get homogeneous
-- entities. Nevertheless the scale factors can be easily recovered from two "transposed" versions

-- (it is of course much easier to join a transfer and the epipole)

base = map tvector . toRows . ident

tri2fun12Alg tri x = thing * (x!"k") * (x!"K")
    where thing = tri!"kij" * eps3!"jJa" * tv[0,0,1]!"a" * tri!"KIJ" * eps3!"iIl"

fh tri = fromColumns $ map (unitary.coords.tri2fun12Alg tri) (base 3)

fv tri = fromRows $ map (unitary.coords.tri2fun21Alg tri) (base 3)

reverseTransfer tri cv x = x!"a" * eps3!"aiI" * cv!"j" * tri!"kij" * tri!"KIJ" * cv!"J" * raise eps3!"kKc"

tri2fun21Alg tri x = reverseTransfer tri (tc[2,7,4]) x !"i" *eps3!"ijk"*reverseTransfer tri (tc[0,-1,8]) x!"j"

exper tri = do
    let a = fromColumns $ map (unitary.coords.tri2fun12Alg tri) (base 3)
        b = fromRows $ map (unitary.coords.tri2fun21Alg tri) (base 3)

    let alpha1 = 1
        beta2  = b@@>(1,0)/a@@>(1,0)
        alpha2 = b@@>(1,1)/a@@>(1,1) / beta2
        alpha3 = b@@>(1,2)/a@@>(1,2) / beta2

    disp $ a <> (diag $ fromList [alpha1,alpha2,alpha3])
    disp $ f12
