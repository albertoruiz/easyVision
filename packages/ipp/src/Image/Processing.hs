module Image.Processing(
    -- * basic functions
    Image(), Pix(), Size(..), size, ROI(..), roi, setROI, modifyROI,
    G.constant, set, copy,
    -- * spatial transformations
    resize, G.resizeFull, G.warp, warpon, uradial,
    -- * logical functions
    andI, orI, notI, xorI, dilate3x3, erode3x3,
    -- * arithmetic functions
    (.*),(.+),
    (|+|),(|-|),absDiff,(|*|),(|/|),
    addC8u, add8u, absDiff8u, sub8u, sub8uRel,
    -- * image filters and matching
    crossCorr, sqrDist,
    -- * color transformations
    rgbToGray, rgbToHSV, hsvToRGB, yCbCrToRGB, rgbToYCbCr,
    twistColors, G.Channels(..), G.channelsFromRGB, G.grayscale, G.grayf,
    -- * pixel transformations
    toFloat, toGray, scale32f8u, scale8u32f,
    -- * misc
    ippSetNumThreads
) where

import Image.Core
import ImagProc.Ipp.Generic(Pix)
import qualified ImagProc.Ipp.Generic as G
import ImagProc.Ipp.Pure
import ImagProc.Ipp.AdHoc(twistColors)
import ImagProc.Ipp.Wrappers(ippSetNumThreads)

set  x l = G.set  x l
copy x l = G.copy x l
resize s i = G.resize s i
warpon i l = G.warpon i l
uradial f g i = G.uradial f g i
crossCorr a b = G.crossCorr a b
sqrDist a b = G.sqrDist a b

