module Quaternion(
    Quaternion(..), (.*.),
    quat, conjQ,
    axisToQuat,
    quatToAxis,
    rotToAxis,
    getRotation,
    getRotationHL,
    slerp
) where

import Numeric.LinearAlgebra
import Vision.Geometry(cross)
import Data.List(minimumBy,maximumBy)
import Data.Function(on)

import Debug.Trace
debug x = trace (show x) x


vector v = fromList  v :: Vector Double
norm x = pnorm PNorm2 x

data Quaternion = Quat {qs::Double, qv::Vector Double} deriving (Eq, Show)

instance Num Quaternion where
    Quat{ qs = a, qv = u } + Quat{ qs = t, qv = v } = Quat { qs = a + t , qv = u + v }
    Quat{ qs = a, qv = u } * Quat{ qs = t, qv = v } = Quat { qs = a*t - u<.>v, qv = a.*v + t .* u + cross u v }
    abs Quat{ qs = s, qv = v } = Quat {qs = sqrt $ s^2 + norm v ^ 2, qv = vector [0,0,0] }
    signum q = error "signum of Quaternion not defined"
    fromInteger = quat . fromInteger

instance Fractional Quaternion where
    fromRational = quat . fromRational
    recip q = conjQ q * (quat . recip .qs) (q * conjQ q)


conjQ Quat{ qs = s, qv = v } = Quat { qs = s, qv = -v }

quat x = Quat {qs = x, qv = vector [0,0,0] }

normalize Quat{ qs = s, qv = v } = Quat { qs = s/m, qv = v */ m }
    where m = sqrt $ s^2 + norm v ^ 2


-- composition of rotations (Grassmann product)
infixl 7 .*.
a .*. b = normalize (a * b)

--------------------------------------

axisToQuat phi axis = Quat { qs = cos (phi/2), qv = sin (phi/2) .* v }
    where v = axis */ norm axis

--------------------------------------

quatToAxis q = r where
    Quat s v = normalize q
    vn = norm v
    phi = 2 * atan2 vn s
    r = if vn > 100*eps then (phi, v */ vn) else (0, vector [1,0,0])

--------------------------------------

getRotation Quat {qs = w, qv = v} =
  let [x, y, z] = toList v
      x2 = x*x; y2 = y*y; z2 = z*z
      xy = x*y; xz = x*z; yz = y*z
      wx = w*x; wy = w*y; wz = w*z
  in (3><3)
     [ 1.0 - 2.0 * (y2 + z2), 2.0 * (xy - wz), 2.0 * (xz + wy)
     , 2.0 * (xy + wz), 1.0 - 2.0 * (x2 + z2), 2.0 * (yz - wx)
     , 2.0 * (xz - wy), 2.0 * (yz + wx), 1.0 - 2.0 * (x2 + y2) ]

---------------------------------------

getRotationHL Quat {qs = w, qv = v} =
  let [x, y, z] = toList v
      x2 = x*x; y2 = y*y; z2 = z*z
      xy = x*y; xz = x*z; yz = y*z
      wx = w*x; wy = w*y; wz = w*z
  in [ 1.0 - 2.0 * (y2 + z2), 2.0 * (xy - wz), 2.0 * (xz + wy), 0,
       2.0 * (xy + wz), 1.0 - 2.0 * (x2 + z2), 2.0 * (yz - wx), 0,
       2.0 * (xz - wy), 2.0 * (yz + wx), 1.0 - 2.0 * (x2 + y2), 0,
       0, 0, 0, 1 ]

-------------------------------------------------------

rotToAxis rot = (angle,axis) where
    (l,v) = eig rot
    r = zip (toList l) (toColumns v)
    axis = liftVector realPart $ snd $ minimumBy (compare `on` (abs.imagPart.fst)) r
    angleraw = phase $ maximumBy (compare `on` imagPart) (toList l)
    mx = maximum $ map abs $ toList $ flatten $ rot
    rec = getRotation $ axisToQuat angleraw axis
    ok = (pnorm PNorm1 (flatten rot - flatten rec)) / mx < 1E-8
    angle = if ok then angleraw else -angleraw

-------------------------------------------------------

slerp1 p0 p1 t = if omega < pi/180/100
                    then p0
                    else quat a * p0 + quat b * p1
    where
        a = sin((1-t)*omega)/sin omega
        b = sin(t*omega)/sin omega
        dt = qdot p0 p1
        omega = if dt > 0 then acos dt else acos (-dt)
        qdot Quat{ qs = a, qv = u } Quat{ qs = t, qv = v } = a*t + u<.>v

slerp2 q0 q1 t = q0 * axisToQuat (phi*t) axis where
    (phi,axis) = quatToAxis $ (1/q0) * q1

slerp = slerp2
