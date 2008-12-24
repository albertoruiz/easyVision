-----------------------------------------------------------------------------
{- |
Module      :  Vision.Geometry
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Projective geometry utilities.

-}
-----------------------------------------------------------------------------

module Vision.Geometry
(
-- * Data types
-- | Homogeneous and inhomogeneous transformations and vectors are provisionally represented by
--   ordinary matrices and vectors.
--   Therefore composition and application is automatically implemented by the matrix product (\<\>).
--
--   Safer types will be used in the future.

-- * Transformation utilities
  homog
, inHomog
, ht
, htc
, htm
-- * Typical transformations
, rot1
, rot2
, rot3
, desp
, scaling
, mS
, mA
, mF
, asMat
, cross
-- * Constants
, linf
, degree
-- * Normalization
, unitary
, normatdet
, normat
, normat3
-- * Utilities
, similarFrom2Points
) where

import Numeric.LinearAlgebra

matrix = fromLists :: [[Double]] -> Matrix Double
vector = fromList ::  [Double] -> Vector Double

norm x = pnorm PNorm2 x

-- | 3x3 rotation around the X-axis
rot1 :: Double -> Matrix Double
rot1 a = matrix [[1, 0,0],
                 [0, c,s],
                 [0,-s,c]]
    where c = cos a
          s = sin a

-- | 3x3 rotation around the Y-axis
rot2 :: Double -> Matrix Double
rot2 a = matrix [[ c,0,s],
                 [ 0,1,0],
                 [-s,0,c]]
    where c = cos a
          s = sin a

-- | 3x3 rotation around the Z-axis
rot3 :: Double -> Matrix Double
rot3 a = matrix [[ c,s,0],
                 [-s,c,0],
                 [ 0,0,1]]
    where c = cos a
          s = sin a

-- | Homogeneous 3x3 matrix of a 2D displacement
desp :: (Double,Double) -- ^ (dx,dy)
      -> Matrix Double
desp (x,y) = matrix [[1,0,x],
                     [0,1,y],
                     [0,0,1]]

-- | Homogeneous 3x3 matrix of an isotropic 2D scaling
--
-- @ s 0 0
-- 0 s 0
-- 0 0 1@
scaling :: Double -> Matrix Double
scaling s = matrix [[s,0,0],
                    [0,s,0],
                    [0,0,1]]

-- |@  0 1 0
-- -1 0 0
--  0 0 0@
mA :: Matrix Double
mA = matrix [[ 0,1,0],
             [-1,0,0],
             [ 0,0,0]]

-- |@ 1 0 0
-- 0 1 0
-- 0 0 0@
mS :: Matrix Double
mS = matrix [[1,0,0],
             [0,1,0],
             [0,0,0]]

-- |@  1 1 0
-- -1 1 0
--  0 0 0@
mF :: Matrix Double
mF = matrix [[ 1,1,0],
             [-1,1,0],
             [ 0,0,0]]

-- | 0 0 1, the line of the infinity.
linf :: Vector Double
linf = vector [0,0,1]

-- | obtains the ordinary vector corresponding to a homogeneous vector (divides by the last component). Note that no ideal point checking is done.
inHomog :: Vector Double -> Vector Double
inHomog v = subVector 0 l v  */  v@>l where l = dim v - 1

-- | creates a homogeneus version of an ordinary vector (appends a constant component equal to 1)
homog :: Vector Double -> Vector Double
homog v = join [v,1]

-- | Obtains a vector in the same direction with 2-norm=1
unitary:: Vector Double -> Vector Double
unitary v = v */ norm v

-- | pi \/ 180
degree :: Double
degree = pi / 180

-- | 3x3 antisymmetric matrix
asMat :: Field a => Vector a -> Matrix a
asMat v = (3><3) [ 0,-c, b,
                   c, 0,-a,
                  -b, a, 0]
    where a = v@>0
          b = v@>1
          c = v@>2

-- | vector cross product
cross :: Field a => Vector a -> Vector a -> Vector a
cross a b = asMat a <> b

-- | obtains a normalized version of a homogeneous matrix dividing by the bottom-right element.
normat3 :: Matrix Double -> Matrix Double
normat3 m = m */ m@@>(rows m -1, cols m -1)

-- | obtains a normalized version of a homogeneous matrix such that the biggest square submatrix on the left has determinant == 1
normatdet :: Matrix Double -> Matrix Double
normatdet m = m */ k where
    s = subMatrix (0,0) (n,n) m
    n = min (rows m) (cols m)
    d = det s
    k = signum d * abs d **(1/ fromIntegral n)

-- | obtains a normalized version of a homogeneous matrix such that the vector of all entries has 2-norm ==1
normat :: Matrix Double -> Matrix Double
normat m = m */ norm (flatten m)

homogMat m = m <|> constant 1 (rows m)

inHomogMat m = ma / (mb `outer` constant 1 (cols ma))
    where ma = takeColumns (cols m -1) m
          mb = flatten $ dropColumns (cols m -1) m

htc h = toLists. inHomogMat . (<> trans h) . homogMat . fromLists

-- | transforms a list of inhomogeneous vectors, given as lists, using a homogeneous transformation.
ht :: Matrix Double -> [[Double]] -> [[Double]]
ht = htc

-- | transforms a list of inhomogeneous vectors, given as rows of a matrix, using a homogeneous transformation.
htm :: Matrix Double -- ^ transformation
    -> Matrix Double -- ^ inhomogeneous input vectors (as rows)
    -> Matrix Double -- ^ transformed vectors (as rows)
htm h = inHomogMat . (<> trans h) . homogMat

-- | Computes a similar 2D homogeneous transformation with moves 2 points to desired positions.
similarFrom2Points :: [Double] -> [Double] -> [Double] -> [Double] -> Matrix Double
similarFrom2Points [ax,ay] [bx,by] [a'x,a'y] [b'x,b'y] = t where
    dx = a'x - ax
    dy = a'y - ay
    s = sqrt ((b'x-a'x)^2 + (b'y-a'y)^2) / sqrt ((bx-ax)^2 + (by-ay)^2)
    ang = atan2 (b'y-a'y) (b'x-a'x) - atan2 (by-ay) (bx-ax)
    t = desp (a'x,a'y) <> rot3 (-ang) <> scaling s <> desp (-ax,-ay)
