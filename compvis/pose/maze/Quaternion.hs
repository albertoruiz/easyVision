-- vim: set et ts=2 sw=2:
{-
Useful quaternion stuff for rotations and orientations.
-}
module Quaternion where

import Numeric.LinearAlgebra
import Vision.Geometry

type Quaternion = Vector Double
type Vec3d = Vector Double

-- Grassman quaternion product
qProd :: Quaternion -> Quaternion -> Quaternion
qProd a b = 
  let [ax, ay, az, at] = toList a
      [bx, by, bz, bt] = toList b
  in vec4 (at*bx + ax*bt + ay*bz - az*by)
          (at*by - ax*bz + ay*bt + az*bx)
          (at*bz + ax*by - ay*bx + az*bt)
          (at*bt - ax*bx - ay*by - az*bz)

-- Shorthand notation
vec3 a b c = 3 |> [a,b,c]
vec4 a b c d = 4 |> [a,b,c,d]

-- The identity quaternion (no rotation, neutral orientation)
qIdent :: Quaternion
qIdent = vec4 0 0 0 1

-- Compute a rotation quaternion: angle in radians around an axis
qRot :: Double -> Vec3d -> Quaternion
qRot angle axis =
  let [x, y, z] = toList $ unitary axis
      sin_angle = sin (angle / 2)
  in vec4 (sin_angle * x) (sin_angle * y) (sin_angle * z) (cos (angle / 2))

-- Matrix form of a quaternion rotation, given as a row-major 4x4 list.
-- Use in OpenGL like: newMatrix RowMajor $ qMatrix quaternion
qMatrix :: Quaternion -> [Double]
qMatrix q  =
  let [x, y, z, w] = toList q
      x2 = x*x; y2 = y*y; z2 = z*z
      xy = x*y; xz = x*z; yz = y*z
      wx = w*x; wy = w*y; wz = w*z
  in [ 1.0 - 2.0 * (y2 + z2), 2.0 * (xy - wz), 2.0 * (xz + wy), 0,
       2.0 * (xy + wz), 1.0 - 2.0 * (x2 + z2), 2.0 * (yz - wx), 0,
       2.0 * (xz - wy), 2.0 * (yz + wx), 1.0 - 2.0 * (x2 + y2), 0,
       0, 0, 0, 1 ]
