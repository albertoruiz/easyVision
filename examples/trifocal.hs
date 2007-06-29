--{-# OPTIONS_GHC -fglasgow-exts #-}

import Vision
import Data.Packed.Tensor
import Data.Packed.Internal.Tensor
import Data.Packed.Internal.Matrix
import GSL.Vector
import GSL

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

nullspaceOK1 m = last $ toColumns $ v
    where (_,_,v) = svdR m

chopV v = liftVector chop v
chopM m = liftMatrix chopV m
chopT t = liftTensor chopV t

chop n = if abs n < 1E-10 then 0 else n

-- cameras

m1 = syntheticCamera $ easyCamera 40 (0,0,0) (0,0,1) 0
cen1 = tvector "c" $ nullspaceOK1 m1
c1 = tensorFromMatrix (Contravariant,"m1") (Covariant, "m2") m1

m2 = syntheticCamera $ easyCamera 20 (1,0,0) (0.5,0.5,1) 0
c2 = tensorFromMatrix (Contravariant,"n1") (Covariant, "n2") m2

f = fundamentalFromCameras m1 m2

eps3 = leviCivita 3
eps4 = raise $ leviCivita 4
tf = eps3!"pqr" * eps4!"bcij" * c1!"pb" * c1!"qc" * c2!"si" * c2!"tj" * eps3!"stk"

-- plane induced by reprojection of line joining two image points
p1 = vector [0,0.1,1]
p2 = vector [1,0.1,1]
l12 = cross p1 p2
plane1 = l12 <> m1


tp1 = tvector "p" p1

-- alternative tensor computation

tP1 = tvector "p" $ pinv m1 <> p1
tP2 = tvector "q" $ pinv m1 <> p2

plane1span = cen1 /\ tP1 /\ tP2

unitaryT = liftTensor unitary


ray1 = tvector "c" (vector [0,0,0,1]) /\ tvector "p" (pinv m1 <> p1)

pl1 = tvector "q1" $ vector [0,-(p1@>2),p1@>1] <> m1
pl2 = tvector "q2" $ vector [-(p1@>2),0,p1@>0] <> m1

infixl 7 -:
n -: t = T d (cdat m)
    where (d,m) = putFirstIdx n t

infixl 8 @@
t @@ l = withIdx t l

infixl 8 !
t ! l = withIdx t (map return l)


infixl 5 |*|
t1 |*| t2 = contractionF t1 t2


main = do
    print $ unitary plane1
    print $ unitaryT $ dualMV $ plane1span
    print $ niceAS plane1span
    print $ niceAS $ dualMV $ plane1span
    print $ dualMV $ plane1span
    putStrLn "-----------------"
    putStrLn "point reprojection without pseudoinverse"
    print $ unitaryT $ dualMV $ raise $ dualMV tp1 |*| "m1" -: c1 |*| "m1" -: c1
    print $ unitaryT $ cen1 /\ tP1
    putStrLn "-----------------"
    putStrLn "Fundamental matrix"
    print $ chopT tf
    print $ chopM $ normat $ reshape 3 $ ten tf
    print $ chopM $ normat $ f

