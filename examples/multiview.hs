import Vision
import Util.Tensor
import Numeric.LinearAlgebra

vector l = fromList l :: Vector Double

tvector v   = tensor [ dim v]  v  -- contravariant
tcovector v = tensor [-dim v] v   -- covariant

-- tensor from matrix as transformation
tTrans m = tensor [rows m, -cols m] (flatten m)

tv = tvector . vector
tc = tcovector . vector

diagl = diag . vector

normAT t = sqrt $ innerAT t t

codual = raise . dual . raise

chopV v = liftVector chop v
chopM m = liftMatrix chopV m
chopT t = liftTensor chopV t

chop n = if abs n < 1E-10 then 0 else n

-- cameras

m1 = syntheticCamera $ easyCamera 40 (0,0,0) (0.5,0,1) 0
--m1 = cameraAtOrigin
--m1 = m3
cen1 = tvector (nullVector m1) ! "c"
c1 = tTrans m1


m2 = syntheticCamera $ easyCamera 20 (1,0,0) (0.5,0.5,1) 0
c2 = tTrans m2

f = fundamentalFromCameras m1 m2

eps3 = leviCivita 3
eps4 = raise $ leviCivita 4
tf = eps3!"pqr" * eps4!"bcij" * c1!"pb" * c1!"qc" * c2!"si" * c2!"tj" * eps3!"stk"

epip = nullVector (trans f)
tepip = tvector epip
something = tcovector ({-asMat epip <> -}vector [1,2,3]) -- epip

tCamOrig = tTrans cameraAtOrigin
tfkey = chopT $ tCamOrig!"ri" * tf!"sr" * raise eps3!"sjk"

-- the apparently equally valid order ijk does not work!
c2' = tfkey!"ijk" * something!"j"  -- + adder (epip)

m2' = chopM $ normat $ reshape 4 $ coords $ c2'
f' = normat $ fundamentalFromCameras cameraAtOrigin m2'

madded = epip `outer` (vector [1,2,3,0])

m2'' = m2' + madded
f'' = normat $ fundamentalFromCameras cameraAtOrigin m2''

cen2chungo = tvector u /\ tvector v
    where [u,v] = nullspacePrec 1 m2'




m3 = syntheticCamera $ easyCamera 30 (0,1,0) (0.5,1,2) 20
c3 = tTrans m3

tri = eps4!"abcd" * c1!"ia" * c2!"jb" * c3!"pc" * c3!"qd" * eps3!"pqk"

m4 = syntheticCamera $ easyCamera 45 (1,1,-1) (2,1,0) (-30)
c4 = tTrans m4

qua = eps4!"abcd" * c1!"ia" * c2!"jb" * c3!"kc" * c4!"rd"


line = tvector (vector [1,2,3,1]) !"i" /\ tvector (vector [5,-4,3,1]) !"j"
il1 = eps3!"abp" * line * c1!"ai" * c1!"bj"
il2 = raise $ dual (line * c2!"ai" * c2!"bj") !"q"
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


m3x3 = (3><3) [0.5,1,0,0,2,0,7,0,1::Double]
m4x4 = (3><3) [0.5,1,0,0,2,0,7,0,1::Double] <|> fromList [2,2,1::Double]
                             <-> fromList [2,0,0,2::Double]

y1 = tv [1,2,3,4] ! "p"
y2 = tv [3,2,0,7] ! "q"

z1 = tv [1,1,0,0] ! "p"
z2 = tv [1,2,0,0] ! "q"
z3 = tv [0,0,1,1] ! "r"
z4 = tv [0,0,1,2] ! "s"



main = do
    putStrLn "Plane reprojection"
    print $ unitary plane1
    print $ unitaryT $ dual (tp1/\tp2) !"k" * raise c1!"kj"
    print $ unitaryT $ dual $ plane1span
    print $ niceAS $ unitaryT $ dual plane1span
    print $ niceAS $ unitaryT $ plane1span
    print $ niceAS $ unitaryT $ dual $ dual (tp1/\tp2) !"k" * raise c1!"kj"
    putStrLn "-----------------"
    putStrLn "point reprojection without pseudoinverse"
    print $ unitaryT $ cen1 /\ tP1
    print $ unitaryT $ codual $ raise (dual tp1) !"ab" * c1!"ai" * c1!"bj"
    print $ unitaryT $ eps4!"ijkr" * eps3!"pab" * tp1 !"p" * c1!"ai" * c1!"bj"
    print $ unitaryT $ eps4!"ijkr" * pl1!"i" * pl2!"j"
    -- reproject two planes <=> reproject the dual, eps gives the lines...
    putStrLn "-----------------"
    putStrLn "Fundamental matrix"
    --print $ chopT tf
    print $ chopM $ normat $ reshape 3 $ coords tf
    print $ chopM $ normat $ f
    print $ chopT $ unitaryT tf!"ij"*c1!"jk"*c2!"il"
    let something = tcovector ({-asMat epip <> -}vector [1,2,3]) -- epip
    let c2' = tf!"kr"*c1!"ra" * raise eps3!"kst" * something!"s" + (tepip!"i"*tc [0,0,0,1]!"j")!"ta"
    let m2' = chopM $ normat $ reshape 4 $ coords $ c2'
    print $ m2'
    print $ (\(_,s,_)-> s) $ svd m2'
    print $ chopM $ normat $ reshape 3 $ coords $ tFundamental c1 c2'
    --error "OK"
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
    let m = tTrans m3x3
    print (2 * inv m3x3)
    print $ eps3!"iab" * m!"ap" * m!"bq" * raise eps3 ! "pqs"
    let m = tTrans m4x4
    let m' = leviCivita 4!"iabc" * m!"ap" * m!"bq" * m! "cr" * raise (leviCivita 4) ! "pqrs"
    print $ tridx ["s","i"] m'
    let ki = m'!"ki" * m!"kj"
    print $ reshape 4 $ coords ki
    print (12 * inv m4x4)
    putStrLn "-----------------"
    putStrLn "Det"
    let m3x3 = (3><3) [0.5,1,0,0,2,2,7,0,1::Double]
    let m = tTrans m3x3
    print (det $ m3x3)
    print $ niceAS $ eps3!"iab" * m!"ap" * m!"bq" * m!"ir"
    putStrLn "-----------------"
    putStrLn "Nullspace"
    print $ chopV $ unitary $ nullVector m3
    print $ chopT $ unitaryT $ eps3!"ijk"*c3!"ip"*c3!"jq"*c3!"kr"*eps4!"pqrl"
    putStrLn "-----------------"
    putStrLn "exterior product properties"
    print $ leviCivita 4 !"pqrs" * (y1 /\ y2)
    print $ 2 * leviCivita 4 !"pqrs" *  y1  * y2
    let t = tensor [-4,-4,-4,-4] (vector [1::Double .. 4^4])
    print $ t !"pqrs" * (y1 /\ y2)
    print $ t !"pqrs" *  y1  * y2
    print $ y1 /\ y2
    print $ y1!"i" * y2!"j" * scalar 0.5 * leviCivita 4 !"ijab" * raise (leviCivita 4) !"abrs"
    putStrLn "-----------------"
    putStrLn "inner product of r-vectors and duals"
    let p1 = tv [1,0,0] /\ tv [0,2,0]
    let p2 = tv [3,0,0] /\ tv [0,4,4]
    print $ innerAT p1 p2
    print $ innerAT (dual p1) (dual p2)
    print $ (acos $ innerAT p1 p2 / normAT p1 / normAT p2) / degree
    putStrLn "-----------------"
    putStrLn "transformation of dual"
    let m = tTrans m4x4
        m' = tTrans (inv m4x4)
    print $ niceAS $ m!"ij" * y1!"j"
    print $ niceAS $ dual $ dual y1!"qrs" * raise m'!"qi" * raise m'!"rj" * raise m'!"sk" *2
    let obj = y1/\y2
    print $ niceAS $ obj!"pq" * m!"ip" * m!"jq"
    print $ niceAS $ dual $ dual obj!"rs" * raise m'!"rj" * raise m'!"sk" * scalar (-2)
    putStrLn "-----------------"
    putStrLn "epipoles from trifocal tensor"
    epitri

yy1 = tv [0,0,0,1]
yy2 = tv [2,2,0,2]
yy3 = tv [-3,-1,0,-1]
yy4 = tv [12,0,0,3]
yy5 = tv [12,0,2,3]



pi1 = dual $ tv [1,0,0]
pi2 = dual $ tv [0,1,0]

kk = chopT $ unitaryT tf!"kr"*c1!"ra"*c2!"kb"

t1 = tv [1,0,0,0] ! "p"
t2 = tv [0,1,0,0] ! "q"
t3 = tv [1,0,3,1] ! "p"

tFundamental c1 c2 = eps3!"pqr" * eps4!"bcij" * c1!"pb" * c1!"qc" * c2!"si" * c2!"tj" * eps3!"stk"




epitri = do
    let x1 = tv [1,2,3]
        x2 = tv [1,-2,1]
        l1 = tc [1,0,0]
        l2 = tc [0,1,0]
        i11 = coords $ tri!"kij"*l1!"i"*x1!"k"
        i21 = coords $ tri!"kij"*l2!"i"*x1!"k"
        i12 = coords $ tri!"kij"*l1!"i"*x2!"k"
        i22 = coords $ tri!"kij"*l2!"i"*x2!"k"
        ep = cross (cross i11 i21) (cross i12 i22)
    print $ chopV $ unitary ep
    print $ chopV $ unitary $ m2 <> (nullVector m3)
