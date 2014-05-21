{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts, TypeFamilies #-}
-----------------------------------------------------------------------------
{- |
Module      :  Util.Homogeneous
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Projective geometry utilities.

-}
-----------------------------------------------------------------------------

module Util.Homogeneous
(
-- * Transformation utilities
  homog
, inHomog
, ht
, htc
, htm
, (!<>), (<>!)
-- * Typical transformations and useful constants
, desp, desp34
, scaling
, flipx
, mS
, mA
, mF
, asMat
, cross
, linf
, rodrigues
-- * Normalization
, normatdet
, normat
, normat3
-- * Utilities
, similarFrom2Points
, adjustRectifier
, pt2hv, hv2pt
, ln2hv, hv2ln
) where

import Numeric.LinearAlgebra.Compat
import Numeric.LinearAlgebra.Util((&),norm,diagl)
import Util.Rotation(rot3)
import Util.Misc(vec,Vec,mat,Mat,impossible)
import Util.Geometry(Point(..),HLine(..))
import qualified Util.Geometry as G


----------------------------------------------------------------------

flipx :: Matrix Double
flipx = diagl [-1,1,1]


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
homog v = v & 1


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

infixl 7 !<>, <>!

(!<>) ::  (Product t, Container Vector t, Num (Vector t))
       => Matrix t -> Matrix t -> Matrix t
a !<> b = G.inhomog (G.homog a <> b)

(<>!) ::  (Product t, Container Vector t, Num (Vector t))
       => Matrix t -> Matrix t -> Matrix t
a <>! b = G.inhomog (a <> G.homog b)


htc :: (Product t, Container Vector t, Num (Vector t)) => Matrix t -> [[t]] -> [[t]]
htc _ [] = []
htc h l  = toLists. inHomogMat . (<> trans h) . homogMat . fromLists $ l

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

ln2hv :: HLine -> Vec
ln2hv (HLine a b c) = vec [a,b,c]

hv2ln :: Vec -> HLine
hv2ln v = HLine a b c where [a,b,c] = toList v

--------------------------------------------------------------------------------

rodrigues :: Vector Double -> Matrix Double
rodrigues v = c * ident 3 + (1-c)*outer r r + s*asMat r
  where
    θ = pnorm PNorm2 v
    r = v / scalar θ
    c = scalar (cos θ)
    s = scalar (sin θ)

--------------------------------------------------------------------------------

