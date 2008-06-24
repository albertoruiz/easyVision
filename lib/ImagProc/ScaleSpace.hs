-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.ScaleSpace
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  ghc

Scale-space tools.

-}
-----------------------------------------------------------------------------

module ImagProc.ScaleSpace (
    getLocalMaxima,
    Stage(..),
    getSigmas
)
where

import ImagProc.Ipp.Core
import ImagProc.Ipp.Pure
import ImagProc.ImageProcessing
import ImagProc.C.Simple(localMaxScale3Simplified)
import Numeric.LinearAlgebra hiding ((.*))


-- | typical sequence of sigmas for scale space
getSigmas :: Float  -- ^ starting sigma (e.g. 1.0)
          -> Int    -- ^ steps / octave (e.g. 3)
          -> [Float]
getSigmas sigma steps = [sigma*k^i | i <- [0..]] where k = 2**(1/fromIntegral steps)



-- | level in the scale space with utilities for extraction of local maxima
data Stage = Stage
    { stSigma    :: Float
    , stResponse :: ImageFloat
    , stMaxLoc   :: ImageFloat
    , stFiltMax  :: ImageFloat }

-- | auxiliary function to create a level given a response filter
mkStage :: (ImageFloat, Float)
        -> Stage
mkStage (resp,sigma) = Stage
    { stSigma    = sigma
    , stResponse = resp
    , stMaxLoc   = maxloc
    , stFiltMax  = filtMax
    } where maxloc   = copyMask32f resp mask
            mask     = compare32f IppCmpEq filtMax resp
            filtMax  = filterMax (round sigma) resp

-- | auxiliary function which extracts the local maxima in the pyramid
localMaxScale :: Int -> Float -> [Stage] -> [[Pixel]]
localMaxScale nmax h stages = zipWith3 (localMaxScale3Simplified nmax h) l1 l2 l3
    where l1 = map stFiltMax stages
          l2 = map stMaxLoc  (tail stages)
          l3 = tail (tail l1)

-- | extracts interest points which are local maxima in the scale space (see Imagproc/InterestPoints.hs)
-- The interest points are grouped by levels. The obtained scales are interpolated.
getLocalMaxima :: Int      -- ^ maximum number of points at each scale
                  -> Float   -- ^ response threshold
                  -> [(ImageFloat, Float)] -- ^ responses at different increasing scales
                  -> ([[(Pixel,Int)]],[Stage]) -- ^ interest points with scale and the pyramid
getLocalMaxima nmax h resps = (pts,pyr) where
        rawpts = localMaxScale nmax h pyr
        pts = zipWith fixPts rawpts [1..]
        sigmas = map stSigma pyr
        pyr = map mkStage resps
        fixPts l k = map (g k) l where g k p = (p,round $ 1.4* interpolateScale p k)
        interpolateScale p k = inter sa a sb b sc c
            where a = flip fval p $ stResponse (pyr!!(k-1))
                  b = flip fval p $ stResponse (pyr!!(k))
                  c = flip fval p $ stResponse (pyr!!(k+1))
                  sa = sigmas!!(k-1)
                  sb = sigmas!!(k)
                  sc = sigmas!!(k+1)
                  inter x1 y1 x2 y2 x3 y3 = n/d where
                      n = x3^2 *(y1 - y2) + x1^2 *(y2 - y3) + x2^2 *(-y1 + y3)
                      d = 2 *(x3 *(y1 - y2) + x1 *(y2 - y3) + x2 *(-y1 + y3))
