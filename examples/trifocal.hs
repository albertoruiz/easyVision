import Vision
import Data.Packed.Tensor
import Data.Packed.Internal.Tensor
import Data.Packed.Internal.Matrix
import GSL.Vector
import GSL
import Data.List(sort)

-- some defs

instance (Field t, Eq t) => Eq (Tensor t) where
    t1 == t2 = undefined

instance (Num t, Field t) => Num (Tensor t) where
    a + b = liftTensor2 add a b
    a * b = okContract a b
    fromInteger n = scalar (fromInteger n)
    abs t = undefined
    signum t = undefined

vector l = fromList l :: Vector Double

diagl = diag . vector

nullspaceOK1 m = last $ toColumns $ v
    where (_,_,v) = svdR m

nullspaceGen :: Double -> Matrix Double -> [Vector Double]
nullspaceGen t m = ns where
    (_,s,v) = svdR' m
    sl@(g:_) = toList s
    tol = (fromIntegral (max (rows m) (cols m)) * g * t * eps)
    rank = length (filter (> g*tol) sl)
    ns = drop rank (toColumns v)

nullspace = nullspaceGen 1

shTM t = reshape (idxDim $ dims t!!1) (ten t)


chopV v = liftVector chop v
chopM m = liftMatrix chopV m
chopT t = liftTensor chopV t

chop n = if abs n < 1E-10 then 0 else n

-- cameras

--m1 = syntheticCamera $ easyCamera 40 (0,0,0) (0,0,1) 0
--m1 = cameraAtOrigin
m1 = m3
cen1 = tvector (nullspaceOK1 m1) ! "c"
c1 = tensorFromMatrix Contravariant Covariant m1


m2 = syntheticCamera $ easyCamera 20 (1,0,0) (0.5,0.5,1) 0
c2 = tensorFromMatrix Contravariant Covariant m2

f = fundamentalFromCameras m1 m2

eps3 = leviCivita 3
eps4 = raise $ leviCivita 4
tf = eps3!"pqr" * eps4!"bcij" * c1!"pb" * c1!"qc" * c2!"si" * c2!"tj" * eps3!"stk"

epip = nullspaceOK1 (trans f)
tepip = tvector epip
something = tcovector ({-asMat epip <> -}vector [1,2,3]) -- epip

tCamOrig = tensorFromMatrix Contravariant Covariant cameraAtOrigin
tfkey = chopT $ tCamOrig!"ri" * tf!"sr" * raise eps3!"sjk"

-- the apparently equally valid order ijk does not work!
c2' = tfkey!"ijk" * something!"j"  -- + adder (epip)

m2' = chopM $ normat $ reshape 4 $ ten $ c2'
f' = normat $ fundamentalFromCameras cameraAtOrigin m2'

madded = epip `outer` (vector [1,2,3,0])

m2'' = m2' + madded
f'' = normat $ fundamentalFromCameras cameraAtOrigin m2''

cen2chungo = tvector u /\ tvector v
    where [u,v] = nullspace m2'




m3 = syntheticCamera $ easyCamera 30 (0,1,0) (0.5,1,2) 20
c3 = tensorFromMatrix Contravariant Covariant m3

tri = eps4!"abcd" * c1!"ia" * c2!"jb" * c3!"pc" * c3!"qd" * eps3!"pqk"

m4 = syntheticCamera $ easyCamera 45 (1,1,-1) (2,1,0) (-30)
c4 = tensorFromMatrix Contravariant Covariant m3

qua = eps4!"abcd" * c1!"ia" * c2!"jb" * c3!"kc" * c4!"rd"


line = tvector (vector [1,2,3,1]) !"i" /\ tvector (vector [5,-4,3,1]) !"j"
il1 = eps3!"abp" * line * c1!"ai" * c1!"bj"
il2 = dual (line * c2!"ai" * c2!"bj") !"q"
il3 = eps3!"abr" * line * c3!"ai" * c3!"bj"

point = tvector (vector [2,5,7,1])
ip1 = point!"i" * c1!"ji"
ip2 = point!"i" * c2!"ji"
ip3 = point!"i" * c3!"ji"
ip4 = point!"i" * c4!"ji"

-- plane induced by reprojection of line joining two image points
p1 = vector [0,0.1,1]
p2 = vector [1,0.1,1]
l12 = cross p1 p2
plane1 = l12 <> m1

tp1 = tvector p1
tp2 = tvector p2

-- alternative tensor computation

tP1 = tvector (pinv m1 <> p1)
tP2 = tvector (pinv m1 <> p2)

plane1span = cen1 /\ tP1 /\ tP2

unitaryT = liftTensor unitary

pl1 = tcovector $ vector [0,-(p1@>2),p1@>1] <> m1
pl2 = tcovector $ vector [-(p1@>2),0,p1@>0] <> m1

infixl 8 @@
t @@ l = withIdx t l

infixl 8 !
t ! l = withIdx t (map return l)

infixl 5 /\
a /\ b = wedge a b

infixl 4 \/
a \/ b = intL a b

infixl 5 |*|
a |*| b = rawProduct a b


m3x3 = (3><3) [0.5,1,0,0,2,0,7,0,1::Double]
m4x4 = (3><3) [0.5,1,0,0,2,0,7,0,1::Double] <|> fromList [2,2,1::Double]
                             <-> fromList [2,0,0,2::Double]

y1 = tvector (vector [1,2,3,4]) ! "p"
y2 = tvector (vector [3,2,0,7]) ! "q"

z1 = tvector (vector [1,1,0,0]) ! "p"
z2 = tvector (vector [1,2,0,0]) ! "q"
z3 = tvector (vector [0,0,1,1]) ! "r"
z4 = tvector (vector [0,0,1,2]) ! "s"


tr4 l = T d (fromList $ take (4^4) l) where
    d = zipWith3 IdxDesc (repeat 4) (repeat Covariant) (take 4 seqind)

main = do
    putStrLn "Plane reprojection"
    print $ unitary plane1
    print $ unitaryT $ dual (tp1/\tp2) !"k" * c1!"kj"
    print $ unitaryT $ dual $ plane1span
    print $ niceAS $ unitaryT $ dual plane1span
    print $ niceAS $ unitaryT $ plane1span
    print $ niceAS $ unitaryT $ codual $ dual (tp1/\tp2) !"k" * c1!"kj"
    putStrLn "-----------------"
    putStrLn "point reprojection without pseudoinverse"
    print $ unitaryT $ cen1 /\ tP1
    print $ unitaryT $ codual $ dual tp1 !"ab" * c1!"ai" * c1!"bj"
    print $ unitaryT $ eps4!"ijkr" * eps3!"pab" * tp1 !"p" * c1!"ai" * c1!"bj"
    print $ unitaryT $ eps4!"ijkr" * pl1!"i" * pl2!"j"
    -- reproject two planes <=> reproject the dual, eps gives the lines...
    putStrLn "-----------------"
    putStrLn "Fundamental matrix"
    --print $ chopT tf
    print $ chopM $ normat $ reshape 3 $ ten tf
    print $ chopM $ normat $ f
    print $ chopT $ unitaryT tf!"ij"*c1!"jk"*c2!"il"
    let something = tcovector ({-asMat epip <> -}vector [1,2,3]) -- epip
    let c2' = tf!"kr"*c1!"ra" * raise eps3!"kst" * something!"s" + adder epip!"tu"
    let m2' = chopM $ normat $ reshape 4 $ ten $ c2'
    print $ m2'
    print $ (\(_,s,_)-> s) $ svdR' m2'
    print $ chopM $ normat $ reshape 3 $ ten $ tFundamental c1 c2'
    error "OK"
    putStrLn "-----------------"
    putStrLn "Trifocal tensor"
    print $ chopT tri
    print $ unitaryT il3
    print $ unitaryT $ tri!"kij" * il1!"i" * il2!"j"
    putStrLn "-----------------"
    putStrLn "Cuadrifocal tensor"
    print $ chopT qua
    print $ chopT $ qua!"pqrs" * eps3!"pib" * ip1!"i" * eps3!"qjc" * ip2!"j" * eps3!"rkd" * ip3!"k" * eps3!"sle" * ip4!"l"
    putStrLn "-----------------"
    putStrLn "Inverse"
    let m = tensorFromMatrix Contravariant Covariant m3x3
    print (2 * inv m3x3)
    print $ eps3!"iab" * m!"ap" * m!"bq" * raise eps3 ! "pqs"
    let m = tensorFromMatrix Contravariant Covariant m4x4
    let m' = leviCivita 4!"iabc" * m!"ap" * m!"bq" * m! "cr" * raise (leviCivita 4) ! "pqrs"
    print $ tridx ["s","i"] m'
    let ki = m'!"ki" * m!"kj"
    print $ reshape 4 $ ten ki
    print (12 * inv m4x4)
    putStrLn "-----------------"
    putStrLn "Det"
    let m3x3 = (3><3) [0.5,1,0,0,2,2,7,0,1::Double]
    let m = tensorFromMatrix Contravariant Covariant m3x3
    print (det $ m3x3)
    print $ niceAS $ eps3!"iab" * m!"ap" * m!"bq" * m!"ir"
    putStrLn "-----------------"
    putStrLn "Nullspace"
    print $ chopV $ unitary $ nullspaceOK1 m3
    print $ chopT $ unitaryT $ eps3!"ijk"*c3!"ip"*c3!"jq"*c3!"kr"*eps4!"pqrl"
    putStrLn "-----------------"
    putStrLn "exterior product properties"
    print $ leviCivita 4 !"pqrs" * (y1 /\ y2)
    print $ 2 * leviCivita 4 !"pqrs" *  y1  * y2
    let t = tr4 [1::Double .. ]
    print $ t !"pqrs" * (y1 /\ y2)
    print $ t !"pqrs" *  y1  * y2
    print $ y1 /\ y2
    print $ y1!"i" * y2!"j" * scalar 0.5 * leviCivita 4 !"ijab" * raise (leviCivita 4) !"abrs"
    putStrLn "-----------------"
    putStrLn "transformation of dual"
    let m = tensorFromMatrix Contravariant Covariant m4x4
        m' = tensorFromMatrix Contravariant Covariant (inv m4x4)
    print $ niceAS $ m!"ij" * y1!"j"
    print $ niceAS $ codual $ dual y1!"qrs" * m'!"qi" * m'!"rj" * m'!"sk" *2
    let obj = y1/\y2
    print $ niceAS $ obj!"pq" * m!"ip" * m!"jq"
    print $ niceAS $ codual $ dual obj!"rs" * m'!"rj" * m'!"sk" * scalar (-2)

yy1 = tvector $ vector [0,0,0,1]
yy2 = tvector $ vector [2,2,0,2]
yy3 = tvector $ vector [-3,-1,0,-1]
yy4 = tvector $ vector [12,0,0,3]
yy5 = tvector $ vector [12,0,2,3]

rdual = raise . dual

intersection a b = rdual $ (rdual a) /\ (rdual b)
intL a b = raise $ dualV [rdual a, rdual b]

pi1 = rdual $ tvector $ vector [1,0,0]
pi2 = rdual $ tvector $ vector [0,1,0]

kk = chopT $ unitaryT tf!"kr"*c1!"ra"*c2!"kb"

t1 = tvector (vector [1,0,0,0]) ! "p"
t2 = tvector (vector [0,1,0,0]) ! "q"
t3 = tvector (vector [1,0,3,1]) ! "p"

tFundamental c1 c2 = eps3!"pqr" * eps4!"bcij" * c1!"pb" * c1!"qc" * c2!"si" * c2!"tj" * eps3!"stk"

adder v = tensorFromMatrix Contravariant Covariant ((diagl[0,0,0]) <|> v)

tenident n = foldl1 rawProduct $ zipWith withIdx auxbase seqind'
    where auxbase = map tcovector (toRows (ident n))


sym t = T (dims t) (ten (sym' (withIdx t seqind)))
    where sym' t = addT $ map (flip tridx t) (perms (names t))
              where nms = map idxName . dims
