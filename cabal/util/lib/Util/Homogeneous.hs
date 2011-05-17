{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
{- |
Module      :  Util.Geometry
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Projective geometry utilities.

-}
-----------------------------------------------------------------------------

module Util.Homogeneous
(
  -- * Basic Types
  Point(..),
  HPoint2, getHPoint2, IsHPoint2(..),
  HPoint3, getHPoint3, IsHPoint3(..),
-- * Transformation utilities
  homog
, inHomog
, ht
, htc
, htm
-- * Typical transformations and useful constants
, desp, desp34
, scaling
, mS
, mA
, mF
, asMat
, cross
, linf
-- * Normalization
, normatdet
, normat
, normat3
-- * Utilities
, similarFrom2Points
, adjustRectifier
, pt2hv, hv2pt
) where

import Numeric.LinearAlgebra
import Util.Rotation(rot3)
import Util.Misc(vec,Vec,mat,Mat,(#),unitary,norm,impossible)

----------------------------------------------------------------------

data Point = Point { px :: !Double, py :: !Double} deriving (Eq, Show, Read)

----------------------------------------------------------------------

-- | homogeneous 2D points
newtype HPoint2 = HPoint2 { getHPoint2 :: Vec } deriving (Read,Show)
class IsHPoint2 x where
    hPoint2 :: x -> HPoint2

instance IsHPoint2 [Double] where
    hPoint2 [x1,x2] = HPoint2 . unitary . vec $ [x1,x2,1]
    hPoint2 [x1,x2,x3] = HPoint2 . unitary . vec $ [x1,x2,x3]
    hPoint2 _ = error "wrong number of elements for hPoint2 [Double]"
    
instance IsHPoint2 Vec where
    hPoint2 v | dim v == 2 = hPoint2 (v#1)
    hPoint2 v | dim v == 3 = HPoint2 . unitary $ v
    hPoint2 _ = error "wrong number of elements for hPoint2 Vec"

instance IsHPoint2 (Double,Double) where
    hPoint2 (x1,x2) = hPoint2 [x1,x2]
    
instance IsHPoint2 (Double,Double,Double) where
    hPoint2 (x1,x2,x3) = hPoint2 [x1,x2,x3]

instance IsHPoint2 Point where
    hPoint2 (Point x y) = hPoint2 (vec [x,y,1])

----------------------------------------------------------------------

-- | homogeneous 3D points
newtype HPoint3 = HPoint3 { getHPoint3 :: Vec } deriving (Read,Show)
class IsHPoint3 x where
    hPoint3 :: x -> HPoint3

instance IsHPoint3 [Double] where
    hPoint3 [x1,x2,x3] = HPoint3 . unitary . vec $ [x1,x2,x3,1]
    hPoint3 [x1,x2,x3,x4] = HPoint3 . unitary . vec $ [x1,x2,x3,x4]
    hPoint3 _ = error "wrong number of elements for hPoint3 [Double]"
    
instance IsHPoint3 Vec where
    hPoint3 v | dim v == 3 = hPoint3 (v#1)
    hPoint3 v | dim v == 4 = HPoint3 . unitary $ v
    hPoint3 _ = error "wrong number of elements for hPoint3 Vec"

instance IsHPoint3 (Double,Double,Double) where
    hPoint3 (x1,x2,x3) = hPoint3 [x1,x2,x3]
    
instance IsHPoint3 (Double,Double,Double,Double) where
    hPoint3 (x1,x2,x3,x4) = hPoint3 [x1,x2,x3,x4]



-- | Homogeneous 3x3 matrix of a 2D displacement
desp :: (Double,Double) -- ^ (dx,dy)
      -> Mat
desp (x,y) = mat [[1,0,x],
                  [0,1,y],
                  [0,0,1]]

-- | Homogeneous 3x3 matrix of an isotropic 2D scaling
--
-- @ s 0 0
-- 0 s 0
-- 0 0 1@
scaling :: Double -> Mat
scaling s = mat [[s,0,0],
                 [0,s,0],
                 [0,0,1]]

-- | Translation and projection
-- @ 1 0 0 -x
-- 0 1 0 -y
-- 0 0 1 -z@
desp34 :: Double -> Double -> Double -> Mat
desp34 x y z = (3><4) [1,0,0,-x,
                       0,1,0,-y,
                       0,0,1,-z]

-- |@  0 1 0
-- -1 0 0
--  0 0 0@
mA :: Mat
mA = mat [[ 0,1,0],
          [-1,0,0],
          [ 0,0,0]]

-- |@ 1 0 0
-- 0 1 0
-- 0 0 0@
mS :: Mat
mS = mat [[1,0,0],
          [0,1,0],
          [0,0,0]]

-- |@  1 1 0
-- -1 1 0
--  0 0 0@
mF :: Mat
mF = mat [[ 1,1,0],
          [-1,1,0],
          [ 0,0,0]]

-- | 0 0 1, the line of the infinity.
linf :: Vec
linf = vec [0,0,1]

-- | obtains the ordinary vector corresponding to a homogeneous vector (divides by the last component). Note that no ideal point checking is done.
inHomog :: Vec -> Vec
inHomog v = subVector 0 l v  / scalar (v@>l) where l = dim v - 1

-- | creates a homogeneus version of an ordinary vector (appends a constant component equal to 1)
homog :: Vec -> Vec
homog v = v#1


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
normat3 :: Mat -> Mat
normat3 m = m / scalar (m@@>(rows m -1, cols m -1))

-- | obtains a normalized version of a homogeneous matrix such that the biggest square submatrix on the left has determinant == 1
normatdet :: Mat -> Mat
normatdet m = m / scalar k where
    s = subMatrix (0,0) (n,n) m
    n = min (rows m) (cols m)
    d = det s
    k = signum d * abs d **(1/ fromIntegral n)

-- | obtains a normalized version of a homogeneous matrix such that the vector of all entries has 2-norm ==1
normat :: Mat -> Mat
normat m = m / scalar (norm (flatten m))

homogMat :: (Container Vector t, Num (Vector t))
         => Matrix t -> Matrix t
homogMat m = fromBlocks [[m,1]]

inHomogMat :: (Container Vector t, Product t, Num (Vector t)) => Matrix t -> Matrix t
inHomogMat m = ma / (mb `outer` constant 1 (cols ma))
    where ma = takeColumns (cols m -1) m
          mb = flatten $ dropColumns (cols m -1) m

htc :: (Product t, Container Vector t, Num (Vector t)) => Matrix t -> [[t]] -> [[t]]
htc h = toLists. inHomogMat . (<> trans h) . homogMat . fromLists

-- | transforms a list of inhomogeneous vectors, given as lists, using a homogeneous transformation.
ht :: Mat -> [[Double]] -> [[Double]]
ht = htc

-- | transforms a list of inhomogeneous vectors, given as rows of a matrix, using a homogeneous transformation.
htm :: Mat -- ^ transformation
    -> Mat -- ^ inhomogeneous input vectors (as rows)
    -> Mat -- ^ transformed vectors (as rows)
htm h = inHomogMat . (<> trans h) . homogMat

-- | Computes a similar 2D homogeneous transformation with moves 2 points to desired positions.
similarFrom2Points :: [Double] -> [Double] -> [Double] -> [Double] -> Mat
similarFrom2Points [ax,ay] [bx,by] [a'x,a'y] [b'x,b'y] = t where
    --dx = a'x - ax
    --dy = a'y - ay
    s = sqrt ((b'x-a'x)**2 + (b'y-a'y)**2) / sqrt ((bx-ax)**2 + (by-ay)**2)
    ang = atan2 (b'y-a'y) (b'x-a'x) - atan2 (by-ay) (bx-ax)
    t = desp (a'x,a'y) <> rot3 (-ang) <> scaling s <> desp (-ax,-ay)
similarFrom2Points _ _ _ _ = impossible "similarFrom2Points"

-- | apply similar transformation to make the given points fixed under the transformation 
adjustRectifier :: Mat -> [Double] -> [Double] -> Matrix Double
adjustRectifier rec p1 p2 = s <> rec
  where
    [p1',p2'] = ht rec [p1,p2]
    s = similarFrom2Points p1' p2' p1 p2
 
pt2hv :: Point -> Vec
pt2hv (Point x y) = vec [x,y,1]

hv2pt :: Vec -> Point
hv2pt v = Point x y where [x,y] = toList (inHomog v)

