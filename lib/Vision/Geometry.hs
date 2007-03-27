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
( rot1
, rot2
, rot3
, desp
, scaling
, mS
, mA
, mF
, asMat
, linf
, cross
, unitary
, homog
, inHomog
, degree
, normatdet
, normat
, normat3
, ht
, htc
) where

import GSL hiding (Matrix, Vector)
import qualified GSL as G

type Matrix = G.Matrix Double
type Vector = G.Vector Double

matrix = fromLists :: [[Double]] -> Matrix
vector = fromList ::  [Double] -> Vector

svd = svdR'

(!:) = (@>)
(!!:) = (@@>)

-- | 3x3 rotation around the X-axis
rot1 :: Double -> Matrix
rot1 a = matrix [[1, 0,0],
                     [0, c,s],
                     [0,-s,c]] 
    where c = cos a 
          s = sin a

-- | 3x3 rotation around the Y-axis
rot2 :: Double -> Matrix
rot2 a = matrix [[ c,0,s],
                     [ 0,1,0],
                     [-s,0,c]] 
    where c = cos a 
          s = sin a

-- | 3x3 rotation around the Z-axis
rot3 :: Double -> Matrix
rot3 a = matrix [[ c,s,0],
                     [-s,c,0],
                     [ 0,0,1]] 
    where c = cos a 
          s = sin a

-- | Homogeneous 3x3 matrix of a 2D displacement
desp :: (Double,Double) -- ^ (dx,dy)
      -> Matrix
desp (x,y) = matrix [[1,0,x],
                       [0,1,y],
                       [0,0,1]]

-- | Homogeneous 3x3 matrix of a isotropic 2D scaling
scaling :: Double -> Matrix
scaling s = matrix [[s,0,0],
                        [0,s,0],
                        [0,0,1]]

-- | Antisymmetric matrix 0 0 1
mA :: Matrix
mA = matrix [[ 0,1,0],
                 [-1,0,0],
                 [ 0,0,0]] 

-- | diag(1,1,0)
mS :: Matrix
mS = matrix [[1,0,0],
                 [0,1,0],
                 [0,0,0]] 

mF = matrix [[ 1,1,0],
                 [-1,1,0],
                 [ 0,0,0]] 

-- | the line of the infinity.
linf :: Vector
linf = vector [0,0,1]

inHomog v = subVector 0 l v <> recip (v!:l) where l = size v - 1
homog v = join [v,1]

-- | Obtains a vector in the same direction with 2-norm=1
unitary:: Vector -> Vector
unitary v = v <> recip (norm v)


degree = pi / 180

extractColumns cs = trans . extractRows cs . trans



asMat v = matrix [[ 0,-c, b],
                  [ c, 0,-a],
                  [-b, a, 0]]
    where a = v!:0
          b = v!:1
          c = v!:2     

cross a b = asMat a <> b    


normat3 m = m <> (recip m!!:(rows m -1, cols m -1)::Double)

normatdet m = m <> recip k where
    s = subMatrix (0,0) (n,n) m
    n = min (rows m) (cols m)
    d = det s
    k = signum d * abs d **(1/ fromIntegral n)

normat m = m <> recip (norm (flatten m))

homogMat m = m <|> constant 1 (rows m)

inHomogMat m = ma / (mb `outer` constant 1 (cols ma))
    where ma = takeColumns (cols m -1) m
          mb = flatten $ dropColumns (cols m -1) m


htc h = toLists. inHomogMat . (<> trans h) . homogMat . fromLists

ht :: Matrix -> [[Double]] -> [[Double]]
ht = htc
