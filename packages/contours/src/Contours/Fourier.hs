-----------------------------------------------------------------------------
{- |
Module      :  Contours.Fourier
Copyright   :  (c) Alberto Ruiz 2007-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Piecewise linear DFT.

-}
-----------------------------------------------------------------------------

module Contours.Fourier (
    fourierPL,
    invFou,
    normalizeStart,
    shiftStart,
    norm2Cont
)
where

import Util.Geometry(Polyline(..),Point(..),distPoints)
import Contours.Base(perimeter)
import Data.List(zipWith4)
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier(ifft)

-- | Exact Fourier series of a piecewise-linear closed curve
fourierPL :: Polyline -> (Int -> Complex Double)

fourierPL c = f
    where
        g = fourierPL' c
        p = map g [0..]
        n = map g [0,-1 .. ]
        f w | w >= 0    = p !! w
            | otherwise = n !! (-w)

fourierPL' (Closed ps) = g where
    (zs,ts,aAs,hs) = prepareFourierPL ps
    g0 = 0.5 * sum (zipWith4 gamma zs ts (tail zs) (tail ts))
        where gamma z1 t1 z2 t2 = (z2+z1)*(t2-t1)
    g 0 = g0
    g w = k* (conj (vhs**w') <.> vas)
        where k = recip (2*pi*w'')^2
              w' = fromIntegral w  :: Vector (Complex Double)
              w'' = fromIntegral w :: Complex Double
    vhs = fromList hs
    vas = fromList $ take (length hs) aAs

prepareFourierPL c = (zs,ts,aAs,hs) where
    zs = map p2c (c++[head c])
        where p2c (Point x y) = x:+y
    ts = map (/last acclen) acclen
        where acclen = scanl (+) 0 (zipWith sl zs (tail zs))
              sl z1 z2 = abs (z2-z1)
    hs = tail $ map exp' ts
        where exp' t = exp (-2*pi*i*t)
    as = cycle $ zipWith4 alpha zs ts (tail zs) (tail ts)
        where alpha z1 t1 z2 t2 = (z2-z1)/(t2-t1)
    aAs = zipWith (-) as (tail as)


--------------------------------------------------------------------------------
-- | The average squared distance to the origin, assuming a parameterization between 0 and 1.
-- | it is the same as sum [magnitude (f k) ^2 | k <- [- n .. n]] where n is sufficiently large
-- | and f = fourierPL contour
norm2Cont :: Polyline -> Double
norm2Cont c@(Closed ps) = 1/3/perimeter c * go (ps++[head ps]) where
    go [_] = 0
    go (a@(Point x1 y1) : b@(Point x2 y2) : rest) =
        distPoints a b *
        (x1*x1 + x2*x2 + x1*x2 + y1*y1 + y2*y2 + y1*y2)
        + go (b:rest)

----------------------------------------------------------------------

shiftStart :: Double -> (Int -> Complex Double) -> (Int -> Complex Double)
shiftStart r f = \w -> cis (fromIntegral w*r) * f w

normalizeStart :: (Int -> Complex Double) -> (Int -> Complex Double)
normalizeStart f = shiftStart (-t) f
    where t = phase ((f (1)- (conjugate $ f(-1))))

invFou :: Int -> Int -> (Int -> Complex Double) -> Polyline
invFou n w fou = Closed r where
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map fou [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)
    c2p (x:+y) = Point x y

