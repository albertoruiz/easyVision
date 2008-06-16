-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.InterestPoints
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  ghc

Scale-space tools.

-}
-----------------------------------------------------------------------------

module ImagProc.InterestPoints (
    getInterestPoints,
    localMaxScale,
    Stage(..), mkStage
)
where

import ImagProc.Ipp.Core
import ImagProc.Ipp.Pure
import ImagProc.ImageProcessing
import ImagProc.C.Simple(localMaxScale3Simplified)
import Numeric.LinearAlgebra hiding ((.*))

-- | level in the scale space with utilities for extraction of local maxima
data Stage = Stage
    { stSigma    :: Float
    , stResponse :: ImageFloat
    , stMaxLoc   :: ImageFloat
    , stFiltMax  :: ImageFloat }

-- | auxiliary function to create a level given a response filter
mkStage :: (Float -> a -> ImageFloat) -- ^ response function at given scale
        -> a                          -- ^ source image
        -> Float                      -- ^ scale
        -> Stage                      -- ^ result
mkStage fun img sigma = Stage
    { stSigma    = sigma
    , stResponse = resp
    , stMaxLoc   = maxloc
    , stFiltMax  = filtMax
    } where resp = fun sigma img
            maxloc  = copyMask32f resp mask
            mask    = compare32f IppCmpEq filtMax resp
            filtMax = filterMax (round sigma) resp

-- | auxiliary function which extracts the local maxima in the pyramid
localMaxScale :: Int -> Float -> [Stage] -> [[Pixel]]
localMaxScale nmax h stages = zipWith3 (localMaxScale3Simplified nmax h) l1 l2 l3
    where l1 = map stFiltMax stages
          l2 = map stMaxLoc  (tail stages)
          l3 = tail (tail l1)

-- | extracts interest points using a given response function (see experiments/scale.hs)
getInterestPoints :: (Float -> a -> ImageFloat) -- ^ response function at given scale
                  -> [Float] -- ^ desired scales
                  -> Int      -- ^ maximum number of points at each scale
                  -> Int      -- ^ maximum total number
                  -> Float   -- ^ response threshold
                  -> a -- ^ source image
                  -> ([(Pixel,Int)],[Stage]) -- ^ interest points with scale and the pyramid
getInterestPoints proc sigmas nmax tot h img = (pts,pyr) where
        rawpts = localMaxScale nmax h pyr
        pts = take tot $ concat $ reverse $ zipWith fixPts rawpts sigmas
        pyr = map (mkStage proc img) sigmas
        fixPts l s = map g l where g x = (x,round $ 1.7*s)
