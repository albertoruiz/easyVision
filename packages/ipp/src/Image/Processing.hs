module Image.Processing(
    -- * Types
    Image(), Pix(), Size(..), ROI(..),
    -- * basic functions
    size, roi, setROI, modifyROI,
    G.constant, set, copy, G.blockImage,
    -- * spatial transformations
    resize, G.resizeFull, G.warp, warpon, uradial,
    -- * logical functions
    andI, orI, notI, xorI, dilate3x3, erode3x3,
    -- * arithmetic functions
    (.*),(.+),
    (|+|),(|-|),absDiff,(|*|),(|/|), abs32f, sqrt32f,
    addC8u, add8u, absDiff8u, sub8u, sub8uRel,
    -- * threshold and comparison
    thresholdVal32f, thresholdVal8u,
    compareC8u, compare8u, IppCmp(..),
    -- * histograms
    histogram, histogramN,
    -- * filters
    filterMax, filterMin, filterMax8u, filterMin8u,
    filterBox, filterBox8u, filterMedian,
    maxEvery, minEvery,
    maxEvery8u, minEvery8u,
    sobelVert, sobelHoriz,
    gauss, gauss8u, laplace, median, highPass8u, Mask(..),
    -- * matching
    crossCorr, sqrDist,
    -- * color transformations
    rgbToGray, rgbToHSV, hsvToRGB, yCbCrToRGB, rgbToYCbCr,
    twistColors, G.Channels(..), G.channelsFromRGB, G.grayscale, G.grayf,
    -- * pixel transformations
    toFloat, toGray, scale32f8u, scale8u32f,
    -- * misc
    ippSetNumThreads
) where

import Image
import ImagProc.Ipp.Generic(Pix)
import qualified ImagProc.Ipp.Generic as G
import ImagProc.Ipp.Pure
import ImagProc.Ipp.AdHoc
import ImagProc.Ipp.Wrappers(ippSetNumThreads)
import ImagProc.Ipp.Structs

set  x l = G.set  x l
copy x l = G.copy x l
resize s i = G.resize s i
warpon i l = G.warpon i l
uradial f g i = G.uradial f g i
crossCorr a b = G.crossCorr a b
sqrDist a b = G.sqrDist a b

