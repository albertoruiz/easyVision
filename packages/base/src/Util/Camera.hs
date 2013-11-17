{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
{- |
Module      :  Util.Camera
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Util.Camera
    ( cameraAtOrigin
    , factorizeCamera
    , rotOfCam
    , mkCamera
    , sepCam
    , kgen
    , knor
    , toCameraSystem
    ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util((#),row,diagl,(¦))
import Util.Homogeneous(normat3)
import Util.Geometry(inhomog)

cameraAtOrigin :: Matrix Double
cameraAtOrigin = ident 3 ¦ 0


-- | Matrix of intrinsic parameters of a diag(f,f,1) camera
kgen :: Double -> Matrix Double
kgen f = (3><3) [ f,0,0
                , 0,f,0
                , 0,0,1 ]



toCameraSystem :: Matrix Double -> (Matrix Double, Double)
toCameraSystem cam = (inv m, f) where
    (k,r,c) = factorizeCamera cam
    m = r ¦ asColumn (-r <> c)
      # row [0,0,0,1]
    (f:_):_ = toLists k


mkCamera :: Matrix Double -- ^ K
         -> Matrix Double -- ^ R
         -> Vector Double -- ^ C
         -> Matrix Double
mkCamera k r c = k <> fromBlocks [[r, - asColumn (r <> c)]]



-- | Given a camera matrix m it returns (K, R, C)
--   such as m \=\~\= k \<\> r \<\> (ident 3 \<\|\> -c)
factorizeCamera :: Matrix Double -> (Matrix Double, Matrix Double, Vector Double)
factorizeCamera m = (normat3 k, signum (det r) `scale` r ,c) where
    m' = takeColumns 3 m
    (k',r') = rq m'
    s = diag(signum (takeDiag k'))
    (_,_,v) = svd m
    (v',_) = qr v
    k = k'<>s
    r = s<>r'
    c = inhomog $ flatten $ dropColumns 3 v'


-- | Factorize a camera matrix as (K, [R|t])
sepCam :: Matrix Double -> (Matrix Double, Matrix Double)
sepCam m = (k,p) where
    (k,r,c) = factorizeCamera m
    p = fromBlocks [[r,-r <> asColumn c]]

rotOfCam :: Matrix Double -> Matrix Double
rotOfCam c = r where (_,r,_) = factorizeCamera c


-- | Scaling of pixel coordinates to get values of order of 1
knor :: (Int,Int) -> Matrix Double
knor (szx,szy) = (3><3) [-a, 0, a,
                          0,-a, b,
                          0, 0, 1]
    where a = fromIntegral szx/2
          b = fromIntegral szy/2

--------------------------------------------------------------------------------

