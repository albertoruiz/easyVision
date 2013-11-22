{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Moments
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module ImagProc.Moments (
    ImageBasis(..),
    imageBasis,
    momentsImage
) where

import ImagProc.Ipp.Pure
import ImagProc.Ipp.AdHoc
import Image.Convert(mat2img)
import Image.Core
import Numeric.LinearAlgebra

data ImageBasis = ImageBasis { zeroIb, oneIb, xIb, yIb, x2Ib, y2Ib, xyIb :: ImageFloat }

imageBasis :: Size -> ImageBasis
imageBasis (Size r c) = ImageBasis {
    zeroIb = mat2img $ konst 0 (r,c),
    oneIb = mat2img $ konst 1 (r,c),
    xIb = xI,
    yIb = yI,
    x2Ib = xI |*| xI,
    y2Ib = yI |*| yI,
    xyIb = xI |*| yI }
  where
    xcoord = linspace c (1,-1::Double)
    ycoord = linspace r (0.75,-0.75::Double)
    xI = mat2img $ single $ fromRows (replicate r xcoord)
    yI = mat2img $ single $ fromColumns (replicate c ycoord)

-- | (mx,my,cxx,cyy,cxy)
momentsImage :: ImageBasis -> ImageFloat -> (Double,Double,Double,Double,Double)
momentsImage ImageBasis {..} g = (mx,my,cxx,cyy,cxy)
  where
    s = sum32f g
    sx = sum32f (g |*| xIb)
    sy = sum32f (g |*| yIb)
    sx2 = sum32f (g |*| x2Ib)
    sy2 = sum32f (g |*| y2Ib)
    sxy = sum32f (g |*| xyIb)
    mx = sx/s
    my = sy/s
    cxx = sx2/s - mx*mx
    cyy = sy2/s - my*my
    cxy = sxy/s - mx*my


